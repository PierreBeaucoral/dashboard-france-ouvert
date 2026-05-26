# ============================================================
# scripts/17_flows_per_commune.R
#
# Pour chaque commune (≥ 50 actifs total), calcule :
#   - Top 5 communes d'origine des entrants (résidents d'ailleurs qui
#     viennent travailler ici)
#   - Top 5 communes de destination des sortants (résidents d'ici qui vont
#     travailler ailleurs)
#
# Source : MOBPRO 2019 (déjà téléchargé par scripts/14_mobilite.R).
# Consolidation PLM appliquée (Paris/Lyon/Marseille agrégés depuis
# les arrondissements pour cohérence avec le panneau commune).
#
# Sortie : data/processed/mobilite/flows_per_commune.parquet
#   Schéma : code_focal, direction (in/out), code_counter, nom_counter,
#            dept_counter, flux, rang (1-5)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(here)
  library(duckdb)
  library(DBI)
})

csv_path <- here::here("data", "raw", "mobilite", "FD_MOBPRO_2019.csv")
out_dir  <- here::here("data", "processed", "mobilite")
out_path <- file.path(out_dir, "flows_per_commune.parquet")

if (!file.exists(csv_path)) {
  stop("MOBPRO CSV introuvable. Lancer scripts/14_mobilite.R d'abord.")
}

con <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

DBI::dbExecute(con, sprintf(
  "CREATE TABLE mob AS SELECT * FROM read_csv('%s', delim=';', header=true, all_varchar=true)",
  csv_path
))

# Vue PLM-consolidée
DBI::dbExecute(con, "
  CREATE OR REPLACE VIEW mob_c AS
  SELECT
    CASE
      WHEN COMMUNE LIKE '751%' THEN '75056'
      WHEN COMMUNE LIKE '6938%' THEN '69123'
      WHEN COMMUNE LIKE '132%' AND CAST(SUBSTRING(COMMUNE,4,2) AS INT) BETWEEN 1 AND 16 THEN '13055'
      ELSE COMMUNE
    END AS COMMUNE,
    CASE
      WHEN DCLT LIKE '751%' THEN '75056'
      WHEN DCLT LIKE '6938%' THEN '69123'
      WHEN DCLT LIKE '132%' AND CAST(SUBSTRING(DCLT,4,2) AS INT) BETWEEN 1 AND 16 THEN '13055'
      ELSE DCLT
    END AS DCLT,
    CAST(IPONDI AS DOUBLE) AS w
  FROM mob
  WHERE COMMUNE IS NOT NULL
")

# Flux total par paire (résidence, destination)
DBI::dbExecute(con, "
  CREATE TABLE pair_flows AS
  SELECT COMMUNE AS code_res, DCLT AS code_dest,
         ROUND(SUM(w), 0) AS flux
  FROM mob_c
  WHERE COMMUNE != DCLT
    AND DCLT NOT IN ('99999','ZZZZZ')
  GROUP BY COMMUNE, DCLT
")

# Top 5 SORTANTS par commune de résidence
out_flows <- DBI::dbGetQuery(con, "
  SELECT code_res AS code_focal,
         'out' AS direction,
         code_dest AS code_counter,
         flux,
         rang
  FROM (
    SELECT code_res, code_dest, flux,
           ROW_NUMBER() OVER (PARTITION BY code_res ORDER BY flux DESC) AS rang
    FROM pair_flows
  ) WHERE rang <= 5
")
message(sprintf("→ %d flux sortants (top 5 par commune)", nrow(out_flows)))

# Top 5 ENTRANTS par commune de destination
in_flows <- DBI::dbGetQuery(con, "
  SELECT code_dest AS code_focal,
         'in' AS direction,
         code_res AS code_counter,
         flux,
         rang
  FROM (
    SELECT code_res, code_dest, flux,
           ROW_NUMBER() OVER (PARTITION BY code_dest ORDER BY flux DESC) AS rang
    FROM pair_flows
  ) WHERE rang <= 5
")
message(sprintf("→ %d flux entrants (top 5 par commune)", nrow(in_flows)))

# Joindre noms communes via populations.parquet (inclut ARM Paris/Lyon/Marseille)
pop <- arrow::read_parquet(here::here("data", "processed", "insee",
                                       "populations.parquet")) |>
  dplyr::filter(type_com %in% c("COM", "ARM")) |>
  dplyr::distinct(code_commune, nom_commune, code_dept) |>
  dplyr::transmute(code = code_commune,
                   nom = nom_commune,
                   dept = code_dept)

flows <- dplyr::bind_rows(out_flows, in_flows) |>
  dplyr::left_join(pop, by = c("code_counter" = "code")) |>
  dplyr::rename(nom_counter = nom, dept_counter = dept)

arrow::write_parquet(flows, out_path)

cat(sprintf("\n✔ %d lignes flux (top 5 in + top 5 out par commune) → %s (%.1f KB)\n",
            nrow(flows), out_path, file.size(out_path) / 1024))

# Stats : nombre de communes uniques avec au moins un flux entrant/sortant
n_focal <- length(unique(flows$code_focal))
n_with_in  <- length(unique(flows$code_focal[flows$direction == "in"]))
n_with_out <- length(unique(flows$code_focal[flows$direction == "out"]))
cat(sprintf("\n  %d communes uniques (focal) — %d ont des entrants, %d ont des sortants\n",
            n_focal, n_with_in, n_with_out))

cat("\nExemple Lyon (69123) :\n")
print(flows |> dplyr::filter(code_focal == "69123") |>
        dplyr::arrange(direction, rang) |>
        dplyr::select(direction, rang, nom_counter, dept_counter, flux))
