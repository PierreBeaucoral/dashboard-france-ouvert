# ============================================================
# scripts/14_mobilite.R
#
# Bloc I — Mobilité domicile-travail INSEE 2019 (MOBPRO).
#
# Source : INSEE / recensement 2019, fichier détail MOBPRO.
#   data.gouv.fr ID : 63db95ccf8de145951fa5fa3
#   Resource ZIP (82 MB) : f3f22487-22d0-45f4-b250-af36fc56ccd0
#
# Le fichier contient ~5 M lignes (1 ligne = un individu actif occupé
# pondéré IPONDI) avec :
#   - COMMUNE : code commune résidence
#   - DCLT    : code commune lieu de travail
#   - IPONDI  : pondération individuelle
#   - TRANS   : mode transport RP 2019 (1=Pas, 2=Marche, 3=Vélo,
#               4=2-roues mot., 5=Voiture/camion/fourgon, 6=Transports
#               en commun)
#
# Agrégation par commune de résidence :
#   - mob_n_actifs           : Σ IPONDI
#   - mob_pct_emploi_local   : Σ IPONDI[COMMUNE==DCLT] / total
#   - mob_pct_voiture        : Σ IPONDI[TRANS==4] / total
#   - mob_pct_tc             : Σ IPONDI[TRANS==5] / total
#   - mob_pct_actifs_velo    : Σ IPONDI[TRANS==2] / total
#   - mob_top_destination    : code commune où le flux sortant est max
#
# Sortie : data/processed/mobilite/mobilite_commune.parquet
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(arrow)
  library(here)
  library(curl)
  library(duckdb)
  library(DBI)
})

URL_MOBPRO <- paste0(
  "https://static.data.gouv.fr/resources/",
  "mobilites-professionnelles-des-individus-deplacements-commune-de-residence-",
  "commune-de-travail-en-2019/20230202-115223/rp2019-mobpro-csv.zip"
)

raw_dir  <- here::here("data", "raw", "mobilite")
out_dir  <- here::here("data", "processed", "mobilite")
zip_path <- file.path(raw_dir, "rp2019-mobpro.zip")
out_path <- file.path(out_dir, "mobilite_commune.parquet")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(zip_path) || file.size(zip_path) < 5e7) {
  message("Téléchargement MOBPRO 2019 (82 MB)…")
  curl::curl_download(URL_MOBPRO, zip_path, quiet = FALSE)
}

# Unzip dans raw_dir
csv_files <- unzip(zip_path, list = TRUE)
message("Contenu du zip :")
print(csv_files)
csv_internal <- csv_files$Name[1]
csv_path <- file.path(raw_dir, csv_internal)
if (!file.exists(csv_path)) {
  unzip(zip_path, files = csv_internal, exdir = raw_dir)
}
message(sprintf("→ CSV décompressé : %s (%.1f MB)",
                csv_path, file.size(csv_path) / 1024 / 1024))

# Inspection : 3 premières lignes
preview <- readr::read_delim(csv_path, delim = ";",
                              n_max = 3,
                              col_types = readr::cols(.default = readr::col_character()),
                              show_col_types = FALSE)
message("Colonnes : ", paste(names(preview), collapse = ", "))
print(preview)

# duckdb pour traiter ~5M lignes efficacement
con <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

DBI::dbExecute(con, sprintf(
  "CREATE TABLE mob AS SELECT * FROM read_csv('%s', delim=';', header=true, all_varchar=true)",
  csv_path
))
n_rows <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM mob")$n
message(sprintf("→ %d lignes chargées dans duckdb", n_rows))

# Schema check
schema <- DBI::dbGetQuery(con, "DESCRIBE mob")
print(schema)

# Agrégation par commune de résidence
agg <- DBI::dbGetQuery(con, "
  WITH base AS (
    SELECT
      COMMUNE,
      DCLT,
      CAST(IPONDI AS DOUBLE) AS w,
      TRANS
    FROM mob
    WHERE COMMUNE IS NOT NULL
  ),
  tot AS (
    SELECT COMMUNE,
           SUM(w) AS mob_n_actifs,
           SUM(CASE WHEN COMMUNE = DCLT THEN w ELSE 0 END) AS w_local,
           -- TRANS RP 2019 : 5=voiture, 6=TC, 3=vélo, 2=marche, 1=pas
           SUM(CASE WHEN TRANS = '5' THEN w ELSE 0 END) AS w_voiture,
           SUM(CASE WHEN TRANS = '6' THEN w ELSE 0 END) AS w_tc,
           SUM(CASE WHEN TRANS = '3' THEN w ELSE 0 END) AS w_velo,
           SUM(CASE WHEN TRANS = '2' THEN w ELSE 0 END) AS w_pied
    FROM base
    GROUP BY COMMUNE
  )
  SELECT
    COMMUNE AS code_commune,
    ROUND(mob_n_actifs, 0)               AS mob_n_actifs,
    ROUND(100.0 * w_local   / mob_n_actifs, 1) AS mob_pct_emploi_local,
    ROUND(100.0 * w_voiture / mob_n_actifs, 1) AS mob_pct_voiture,
    ROUND(100.0 * w_tc      / mob_n_actifs, 1) AS mob_pct_tc,
    ROUND(100.0 * w_velo    / mob_n_actifs, 1) AS mob_pct_velo,
    ROUND(100.0 * w_pied    / mob_n_actifs, 1) AS mob_pct_pied
  FROM tot
")
message(sprintf("→ %d communes de résidence", nrow(agg)))

# Top destination (commune ≠ résidence) ayant flux max
top_dest <- DBI::dbGetQuery(con, "
  WITH flow AS (
    SELECT COMMUNE, DCLT, SUM(CAST(IPONDI AS DOUBLE)) AS w
    FROM mob
    WHERE COMMUNE IS NOT NULL AND DCLT IS NOT NULL AND COMMUNE != DCLT
    GROUP BY COMMUNE, DCLT
  ),
  ranked AS (
    SELECT COMMUNE, DCLT, w,
           ROW_NUMBER() OVER (PARTITION BY COMMUNE ORDER BY w DESC) AS rnk
    FROM flow
  )
  SELECT COMMUNE AS code_commune,
         DCLT    AS mob_top_dest_code,
         ROUND(w, 0) AS mob_top_dest_flux
  FROM ranked WHERE rnk = 1
")
message(sprintf("→ %d top-destinations", nrow(top_dest)))

# Merge
out <- agg |>
  dplyr::left_join(top_dest, by = "code_commune")

arrow::write_parquet(out, out_path)

cat(sprintf("\n✔ %d communes → %s (%.1f KB)\n",
            nrow(out), out_path, file.size(out_path) / 1024))
cat("\nAperçu :\n")
print(head(out, 8))
cat("\n% emploi local :\n")
print(summary(out$mob_pct_emploi_local))
cat("\n% voiture :\n")
print(summary(out$mob_pct_voiture))
