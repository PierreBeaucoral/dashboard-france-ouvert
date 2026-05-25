# ============================================================
# scripts/03_dvf_aggregate.R
#
# Étape 3 — Demandes de Valeurs Foncières (DVF) :
# agrégation par commune × année × type_local.
#
# Source : DVF géolocalisé Etalab, files.data.gouv.fr.
#   URL : https://files.data.gouv.fr/geo-dvf/latest/csv/{YYYY}/full.csv.gz
#   Format : CSV gz, ~50-100 MB par année compressé, 10-50 M lignes
#            décompressées sur 5 ans.
#
# Filtres méthodo (cf. docs/decisions.qmd) :
#   - nature_mutation = "Vente"
#   - type_local ∈ {"Appartement", "Maison"}
#   - surface_reelle_bati > 9 m²
#   - valeur_fonciere / surface_reelle_bati ∈ [200, 30000] €/m²
#   - Mono-local : un seul bien résidentiel par id_mutation
#                  (évite la répartition arbitraire des prix sur les lots)
#
# Sortie : data/processed/dvf/dvf_aggregated.parquet
#   1 ligne / (commune × année × type_local) avec
#   prix_m2_median, prix_m2_q25, prix_m2_q75, n_transactions
#
# Engine : duckdb — lit directement les .csv.gz sans tout charger en RAM,
# group-by performant, médianes via QUANTILE_CONT.
#
# Lancement : Rscript scripts/03_dvf_aggregate.R
# ============================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(arrow)
  library(here)
  library(glue)
  library(dplyr)
})

# Etalab "latest" ne conserve que les 4 dernières années. Pour les années
# plus anciennes (avant 2021), il faudrait taper dans les snapshots datés
# (https://files.data.gouv.fr/geo-dvf/{YYYY-MM}/), à faire plus tard si besoin.
YEARS    <- 2021:2024
raw_dir  <- here::here("data", "raw", "dvf")
out_dir  <- here::here("data", "processed", "dvf")
out_path <- file.path(out_dir, "dvf_aggregated.parquet")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Vérifier que les fichiers raw sont là ----

raw_files <- file.path(raw_dir, paste0("dvf_", YEARS, ".csv.gz"))
missing <- raw_files[!file.exists(raw_files)]
if (length(missing) > 0) {
  stop("Fichiers DVF manquants :\n  ",
       paste(missing, collapse = "\n  "),
       "\n\nLancer le téléchargement :\n",
       "  cd data/raw/dvf && for y in 2019..2024; do ",
       "    curl -sSL -o dvf_$y.csv.gz ",
       "https://files.data.gouv.fr/geo-dvf/latest/csv/$y/full.csv.gz; ",
       "  done")
}

# ---- DuckDB : agrégation streamée année par année ----

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

# Charger les fonctions zlib pour CSV gzippés (built-in en DuckDB récent)
DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;")

results <- list()

for (year in YEARS) {
  path <- file.path(raw_dir, sprintf("dvf_%d.csv.gz", year))
  message(glue::glue("→ Année {year} ({basename(path)}, {round(file.size(path)/1e6, 1)} MB)"))

  # 1) Construire la table 'residential' filtrée
  # 2) Identifier les mutations mono-local
  # 3) Calculer prix_m2 borné, agréger
  sql <- glue::glue("
    WITH src AS (
      SELECT
        id_mutation,
        date_mutation::DATE                          AS date_mutation,
        nature_mutation,
        valeur_fonciere,
        code_commune,
        type_local,
        surface_reelle_bati
      FROM read_csv_auto('{path}', header = TRUE, sample_size = -1,
                         types = {{
                           'code_commune': 'VARCHAR',
                           'code_postal' : 'VARCHAR',
                           'code_departement': 'VARCHAR'
                         }})
      WHERE nature_mutation = 'Vente'
        AND type_local IN ('Appartement', 'Maison')
        AND surface_reelle_bati > 9
        AND valeur_fonciere IS NOT NULL
        AND valeur_fonciere > 0
    ),
    mono AS (
      SELECT id_mutation
      FROM src
      GROUP BY id_mutation
      HAVING COUNT(*) = 1
    ),
    filtered AS (
      SELECT
        s.code_commune,
        EXTRACT(YEAR FROM s.date_mutation)::INT     AS annee,
        s.type_local,
        s.valeur_fonciere / s.surface_reelle_bati   AS prix_m2
      FROM src s
      INNER JOIN mono m USING (id_mutation)
      WHERE s.valeur_fonciere / s.surface_reelle_bati BETWEEN 200 AND 30000
    )
    SELECT
      code_commune,
      annee,
      type_local,
      COUNT(*)                            AS n_transactions,
      MEDIAN(prix_m2)                     AS prix_m2_median,
      QUANTILE_CONT(prix_m2, 0.25)        AS prix_m2_q25,
      QUANTILE_CONT(prix_m2, 0.75)        AS prix_m2_q75
    FROM filtered
    GROUP BY code_commune, annee, type_local
  ")

  df <- DBI::dbGetQuery(con, sql)
  message(glue::glue("  {nrow(df)} (commune × type) agrégés ({sum(df$n_transactions)} ventes)"))

  results[[as.character(year)]] <- df
}

# ---- Concaténer + écrire ----

out <- dplyr::bind_rows(results)

# Normalisation du code commune sur 5 chars (au cas où duckdb ait perdu des
# zéros initiaux — improbable car on force VARCHAR, mais belt+suspenders).
out$code_commune <- stringr::str_pad(out$code_commune, 5, "left", "0")

# Marqueur de qualité : N < 5 transactions = estimation insuffisante
out <- out |>
  dplyr::mutate(
    estimation_insuffisante = n_transactions < 5
  )

arrow::write_parquet(out, out_path)

# ---- Rapport ----

cat(glue::glue("

  ✔ {nrow(out)} lignes (commune × année × type) → {out_path}
    ({round(file.size(out_path)/1024)} KB)

  Couverture par année :
"))
print(out |> dplyr::count(annee, type_local) |>
        tidyr::pivot_wider(names_from = type_local, values_from = n))

cat("\n  Prix médian m² par année (toutes communes confondues, App) :\n")
print(out |>
        dplyr::filter(type_local == "Appartement", n_transactions >= 5) |>
        dplyr::group_by(annee) |>
        dplyr::summarise(prix_median_global = median(prix_m2_median, na.rm = TRUE)))

cat("\n  Prix médian m² par année (Maison) :\n")
print(out |>
        dplyr::filter(type_local == "Maison", n_transactions >= 5) |>
        dplyr::group_by(annee) |>
        dplyr::summarise(prix_median_global = median(prix_m2_median, na.rm = TRUE)))
