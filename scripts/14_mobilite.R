# ============================================================
# scripts/14_mobilite.R
#
# Bloc I — Mobilité domicile-travail INSEE, RP 2022 (MOBPRO).
#
# Source : INSEE / recensement 2022, fichier détail MOBPRO
#   https://www.insee.fr/fr/statistiques/8589904
#   RP2022_mobpro.parquet (66 MB, 7 388 023 lignes x 32 variables)
#   varmod_mobpro_2022.csv (dictionnaire des modalités)
#   Géographie : COG 2024 (cf. caveat harmonisation dans scripts/README.md)
#
# 1 ligne = un individu actif occupé, pondéré IPONDI :
#   - COMMUNE : commune de résidence (Paris/Lyon/Marseille = commune mère
#               75056/69123/13055 ; l'arrondissement est dans ARM)
#   - DCLT    : commune du lieu de travail (arrondissements PLM détaillés)
#   - IPONDI  : pondération individuelle
#   - TRANS   : mode de transport (varmod_mobpro_2022.csv, vérifié) :
#               1=Pas de transport, 2=Marche, 3=Vélo (y c. VAE),
#               4=Deux-roues motorisé, 5=Voiture/camion/fourgonnette,
#               6=Transports en commun
#
# Agrégation par commune de résidence (schéma inchangé vs RP 2019) :
#   - mob_n_actifs, mob_pct_emploi_local (COMMUNE==DCLT, PLM consolidé),
#     mob_pct_voiture (TRANS 5), mob_pct_tc (6), mob_pct_velo (3),
#     mob_pct_pied (2), mob_top_dest_code / mob_top_dest_flux
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
source(here::here("scripts", "cog_helpers.R"))

URL_MOBPRO <- "https://www.insee.fr/fr/statistiques/fichier/8589904/RP2022_mobpro.parquet"
URL_VARMOD <- "https://www.insee.fr/fr/statistiques/fichier/8589904/varmod_mobpro_2022.csv"

raw_dir      <- here::here("data", "raw", "mobilite")
out_dir      <- here::here("data", "processed", "mobilite")
pq_path      <- file.path(raw_dir, "RP2022_mobpro.parquet")
varmod_path  <- file.path(raw_dir, "varmod_mobpro_2022.csv")
out_path     <- file.path(out_dir, "mobilite_commune.parquet")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

dl_if_missing <- function(url, dest, min_bytes) {
  if (file.exists(dest) && file.size(dest) >= min_bytes) {
    message("  Cache : ", basename(dest)); return(invisible(NULL))
  }
  message("  Télécharge : ", url)
  h <- curl::new_handle(useragent = "Mozilla/5.0")
  curl::curl_download(url, dest, quiet = TRUE, handle = h)
}
dl_if_missing(URL_MOBPRO, pq_path, 5e7)
dl_if_missing(URL_VARMOD, varmod_path, 1e6)

# Garde-fou : les codes TRANS doivent rester ceux supposés ci-dessous.
vm <- readr::read_delim(varmod_path, delim = ";", show_col_types = FALSE,
                        col_names = FALSE, col_types = readr::cols(.default = "c"))
vm <- vm[vm[[1]] == "TRANS", ]
expected <- c("1" = "Pas de transport", "2" = "Marche", "3" = "Vélo",
              "4" = "Deux-roues", "5" = "Voiture", "6" = "Transports en commun")
stopifnot(all(startsWith(vm[[4]][match(names(expected), vm[[3]])], expected)))

# duckdb pour traiter ~5M lignes efficacement
con <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

# Harmonisation COG 2024 (millésime du fichier) -> COG 2026 : on remappe les
# codes AVANT d'agréger, donc les effectifs de communes fusionnées s'additionnent.
cogmap <- cog_map("2024-01-01")
DBI::dbWriteTable(con, "cogmap", cogmap)
DBI::dbExecute(con, sprintf(
  "CREATE TABLE mob AS SELECT COALESCE(a.code_2026, CAST(COMMUNE AS VARCHAR)) AS COMMUNE,
          COALESCE(b.code_2026, CAST(DCLT AS VARCHAR)) AS DCLT, IPONDI, CAST(TRANS AS INTEGER) AS TRANS
   FROM read_parquet('%s') m
   LEFT JOIN cogmap a ON CAST(m.COMMUNE AS VARCHAR) = a.code_avant
   LEFT JOIN cogmap b ON CAST(m.DCLT AS VARCHAR) = b.code_avant", pq_path))
# PLM : COMMUNE est déjà la commune mère, DCLT est détaillé par arrondissement
DBI::dbExecute(con, "
  CREATE VIEW mob_c AS
  SELECT COMMUNE, IPONDI, TRANS,
    CASE
      WHEN DCLT LIKE '751%' THEN '75056'
      WHEN DCLT LIKE '6938%' THEN '69123'
      WHEN DCLT LIKE '132%' AND CAST(SUBSTRING(DCLT,4,2) AS INT) BETWEEN 1 AND 16 THEN '13055'
      ELSE DCLT
    END AS DCLT
  FROM mob")
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
    FROM mob_c
    WHERE COMMUNE IS NOT NULL
  ),
  tot AS (
    SELECT COMMUNE,
           SUM(w) AS mob_n_actifs,
           SUM(CASE WHEN COMMUNE = DCLT THEN w ELSE 0 END) AS w_local,
           -- TRANS RP 2022 (codes inchangés vs 2019) : 5=voiture, 6=TC, 3=vélo, 2=marche, 1=pas
           SUM(CASE WHEN TRANS = 5 THEN w ELSE 0 END) AS w_voiture,
           SUM(CASE WHEN TRANS = 6 THEN w ELSE 0 END) AS w_tc,
           SUM(CASE WHEN TRANS = 3 THEN w ELSE 0 END) AS w_velo,
           SUM(CASE WHEN TRANS = 2 THEN w ELSE 0 END) AS w_pied
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
    FROM mob_c
    WHERE COMMUNE IS NOT NULL AND DCLT IS NOT NULL AND COMMUNE != DCLT
      AND DCLT NOT IN ('99999','ZZZZZ')
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

# ---- Contrôles ----
stopifnot(
  !anyDuplicated(out$code_commune),
  all(nchar(out$code_commune) == 5),
  abs(sum(out$mob_n_actifs) / 28382871 - 1) < 0.001,   # Σ IPONDI du fichier
  abs(nrow(out) / 34820 - 1) < 0.05,
  all(out$mob_pct_voiture >= 0 & out$mob_pct_voiture <= 100),
  all(out$mob_pct_emploi_local >= 0 & out$mob_pct_emploi_local <= 100),
  all(c("75056", "69123", "13055") %in% out$code_commune)
)
cat("\n✔ stopifnot() OK\n")
