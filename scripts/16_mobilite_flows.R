# ============================================================
# scripts/16_mobilite_flows.R
#
# Bloc I — Agrégation des flux domicile-travail entre bassins d'emploi
# pour le Sankey de la page Mobilité.
#
# Source : INSEE MOBPRO 2019 (déjà téléchargé par scripts/14_mobilite.R).
# Stratégie :
#   1. Identifier les 50 plus grosses communes destination (DCLT)
#   2. Pour chaque résidence COMMUNE, agréger Σ IPONDI vers ces 50
#   3. Conserver les 200 plus grands flux pour le Sankey
#
# Sortie : data/processed/mobilite/flows_top.parquet
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
out_path <- file.path(out_dir, "flows_top.parquet")

if (!file.exists(csv_path)) {
  stop("MOBPRO CSV introuvable. Lancer scripts/14_mobilite.R d'abord.")
}

con <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

DBI::dbExecute(con, sprintf(
  "CREATE TABLE mob AS SELECT * FROM read_csv('%s', delim=';', header=true, all_varchar=true)",
  csv_path
))

# Vue agrégée : on consolide les arrondissements PLM vers leur ville mère.
# Paris 75101-75120 → 75056, Lyon 69381-69389 → 69123, Marseille 132xx → 13055.
DBI::dbExecute(con, "
  CREATE OR REPLACE VIEW mob_consolidated AS
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
    IPONDI
  FROM mob
")

# Top 50 destinations par poids total entrant (après consolidation PLM)
top_dest <- DBI::dbGetQuery(con, "
  SELECT DCLT AS code_commune, SUM(CAST(IPONDI AS DOUBLE)) AS w_in
  FROM mob_consolidated
  WHERE DCLT IS NOT NULL AND DCLT NOT IN ('99999','ZZZZZ')
  GROUP BY DCLT
  ORDER BY w_in DESC
  LIMIT 50
") |> dplyr::pull(code_commune)

dest_list <- paste0("'", top_dest, "'", collapse = ",")

# Flux résidence × destination (uniquement vers top-50, COMMUNE != DCLT)
flows <- DBI::dbGetQuery(con, sprintf("
  SELECT COMMUNE AS code_res,
         DCLT    AS code_dest,
         ROUND(SUM(CAST(IPONDI AS DOUBLE)), 0) AS flux
  FROM mob_consolidated
  WHERE DCLT IN (%s)
    AND COMMUNE IS NOT NULL
    AND COMMUNE != DCLT
  GROUP BY COMMUNE, DCLT
  HAVING flux >= 100
  ORDER BY flux DESC
", dest_list))

message(sprintf("→ %d flux (résidence → top-50 destination) avec >= 50 actifs",
                nrow(flows)))

# Limite : 200 plus gros flux pour la lisibilité du Sankey
flows_top <- flows |>
  dplyr::arrange(dplyr::desc(flux)) |>
  dplyr::slice(1:200)

# Joindre les noms des communes pour le rendu
geo <- arrow::read_parquet(here::here("data", "processed", "insee",
                                       "populations.parquet")) |>
  # Inclut COM (communes) ET ARM (arrondissements PLM)
  dplyr::filter(type_com %in% c("COM", "ARM")) |>
  dplyr::distinct(code_commune, nom_commune, code_dept) |>
  dplyr::transmute(code = code_commune,
                   nom  = nom_commune,
                   dept = code_dept)

flows_named <- flows_top |>
  dplyr::left_join(geo, by = c("code_res" = "code")) |>
  dplyr::rename(nom_res = nom, dept_res = dept) |>
  dplyr::left_join(geo, by = c("code_dest" = "code")) |>
  dplyr::rename(nom_dest = nom, dept_dest = dept) |>
  dplyr::mutate(
    label_res  = paste0(nom_res,  " (", dept_res,  ")"),
    label_dest = paste0(nom_dest, " (", dept_dest, ")")
  )

arrow::write_parquet(flows_named, out_path)

cat(sprintf("\n✔ %d flux → %s (%.1f KB)\n",
            nrow(flows_named), out_path, file.size(out_path) / 1024))
cat("\nTop 10 flux:\n")
print(head(flows_named |>
             dplyr::select(label_res, label_dest, flux), 10))
