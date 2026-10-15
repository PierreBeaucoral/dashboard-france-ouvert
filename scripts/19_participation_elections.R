# ============================================================
# scripts/19_participation_elections.R
#
# Abstention / participation 2002-2026, toutes élections agrégées par commune.
#
# Source : data.gouv.fr "Données des élections agrégées" (Min. Intérieur,
#   consolidé par data.gouv.fr), niveau bureau de vote, licence Ouverte 2.0.
#   https://data-pipeline-open.s3.sbg.io.cloud.ovh.net/elections/general_results.parquet
#   Présidentielles 2002-2022, législatives 2002-2024, européennes 2004-2024,
#   régionales 2004-2021, départementales 2015-2021, municipales 2008-2026.
#
# Méthode : somme des EFFECTIFS (inscrits, votants, abstentions, blancs, nuls)
# des bureaux de vote par commune, remappage des codes vers le COG 2026 AVANT
# la somme (scripts/cog_helpers.R), taux calculés après.
# Hors périmètre : Français de l'étranger (ZZ), COM du Pacifique/SPM/Saint-Martin
# (codes absents du COG 2026 des communes). Mayotte (976xx) conservée si présente.
#
# Sorties : data/processed/elections/participation_long.parquet
#             (commune x id_election : effectifs + taux)
#           data/processed/elections/participation_commune.parquet
#             (1 ligne/commune : indicateurs clés pour le merge)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(arrow); library(here)
  library(curl);  library(glue);  library(duckdb); library(DBI)
})
source(here::here("scripts", "cog_helpers.R"))

URL <- "https://data-pipeline-open.s3.sbg.io.cloud.ovh.net/elections/general_results.parquet"
raw_dir <- here::here("data", "raw", "elections_agregees")
out_dir <- here::here("data", "processed", "elections")
pq_path <- file.path(raw_dir, "general_results.parquet")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(pq_path) || file.size(pq_path) < 5e7) {
  message("Télécharge : ", URL)
  curl::curl_download(URL, pq_path, quiet = TRUE)
} else message("  Cache : ", basename(pq_path))

# ---- Agrégation commune x élection (duckdb, mapping COG 2026 avant somme) ----
# Millésime géographique mélangé (2002 -> 2026) : on applique tous les mouvements
# depuis 2002 ; un code vivant en 2026 n'est jamais remappé.
cmap <- cog_map("2002-01-01")
con <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
DBI::dbWriteTable(con, "cmap", cmap)

raw <- DBI::dbGetQuery(con, glue("
  SELECT g.id_election,
         COALESCE(m.code_2026, g.code_commune) AS code_commune,
         SUM(inscrits)    AS inscrits,
         SUM(votants)     AS votants,
         SUM(abstentions) AS abstentions,
         SUM(blancs)      AS blancs,
         SUM(nuls)        AS nuls,
         SUM(exprimes)    AS exprimes
  FROM read_parquet('{pq_path}') g
  LEFT JOIN cmap m ON g.code_commune = m.code_avant
  WHERE g.code_commune IS NOT NULL
  GROUP BY 1, 2
")) |> tibble::as_tibble()
message("Lignes commune x élection brutes : ", nrow(raw))
n_src_2022 <- DBI::dbGetQuery(con, glue("
  SELECT SUM(inscrits) AS n FROM read_parquet('{pq_path}')
  WHERE id_election = '2022_pres_t1' AND code_commune IS NOT NULL"))$n

valid <- arrow::read_parquet(here::here("data", "processed", "insee", "populations.parquet")) |>
  filter(type_com == "COM") |> pull(code_commune)
dropped <- raw |> filter(!code_commune %in% valid)
message("Part des inscrits prés. 2022 T1 hors périmètre COG 2026 (étranger, COM) : ",
        round(100 * sum(dropped$inscrits[dropped$id_election == "2022_pres_t1"]) / n_src_2022, 2), " %")
message("Hors COG 2026 (étranger, COM Pacifique, etc.) : ",
        n_distinct(dropped$code_commune), " codes, ", nrow(dropped), " lignes écartées")

long <- raw |>
  filter(code_commune %in% valid) |>
  mutate(
    annee = as.integer(substr(id_election, 1, 4)),
    type_election = sub("^\\d{4}_([a-z]+)_t\\d$", "\\1", id_election),
    tour = as.integer(sub(".*_t(\\d)$", "\\1", id_election))
  ) |>
  filter(annee >= 2002, type_election != "cant", inscrits > 0) |>
  # incohérences de la source (votants > inscrits, abstentions < 0) : taux mis à NA
  mutate(incoherent = abstentions < 0 | votants > inscrits | coalesce(blancs + nuls > votants, FALSE),
         part_abstention  = if_else(incoherent, NA_real_, round(100 * abstentions / inscrits, 2)),
         part_blancs_nuls = if_else(incoherent | is.na(blancs + nuls), NA_real_, round(100 * (blancs + nuls) / inscrits, 2))) |>
  select(-incoherent) |>
  relocate(code_commune, id_election, annee, type_election, tour)

# ---- Indicateurs clés (wide) ----
pick <- c(pres02_t1 = "2002_pres_t1", pres22_t1 = "2022_pres_t1", pres22_t2 = "2022_pres_t2",
          euro24 = "2024_euro_t1", legi24_t1 = "2024_legi_t1", legi24_t2 = "2024_legi_t2",
          muni26_t1 = "2026_muni_t1")
wide <- long |>
  filter(id_election %in% pick) |>
  mutate(k = names(pick)[match(id_election, pick)]) |>
  select(code_commune, k, part_abstention, part_blancs_nuls) |>
  pivot_wider(names_from = k, values_from = c(part_abstention, part_blancs_nuls),
              names_glue = "{ifelse(.value == 'part_abstention', 'abst', 'blancnul')}_{k}") |>
  select(code_commune, starts_with("abst_"), blancnul_pres22_t1) |>
  mutate(
    abst_delta_pres02_pres22 = round(abst_pres22_t1 - abst_pres02_t1, 2),     # points de %
    abst_ecart_euro24_pres22 = round(abst_euro24 - abst_pres22_t1, 2)         # points de %
  )

arrow::write_parquet(long, file.path(out_dir, "participation_long.parquet"), compression = "zstd")
arrow::write_parquet(wide, file.path(out_dir, "participation_commune.parquet"), compression = "zstd")

cat(glue("\n✔ long : {nrow(long)} lignes, {n_distinct(long$id_election)} scrutins ",
         "({round(file.size(file.path(out_dir, 'participation_long.parquet'))/1e6, 1)} MB)\n",
         "✔ wide : {nrow(wide)} communes x {ncol(wide)} colonnes\n"))
print(long |> filter(id_election %in% pick) |> count(id_election))
print(summary(wide$abst_pres22_t1))

# ---- Contrôles ----
DBI::dbWriteTable(con, "valid", tibble::tibble(code = valid))
n_src_kept_2022 <- DBI::dbGetQuery(con, glue("
  SELECT SUM(inscrits) AS n FROM read_parquet('{pq_path}')
  WHERE id_election = '2022_pres_t1' AND code_commune IS NOT NULL
    AND (code_commune IN (SELECT code FROM valid) OR code_commune IN (SELECT code_avant FROM cmap))"))$n
n22 <- sum(long$id_election == "2022_pres_t1")
stopifnot(
  !anyDuplicated(long[c("code_commune", "id_election")]),
  !anyDuplicated(wide$code_commune),
  all(long$code_commune %in% valid),
  all(long$part_abstention >= 0 & long$part_abstention <= 100, na.rm = TRUE),
  sum(is.na(long$part_abstention)) < 50,
  abs(n22 / 34900 - 1) < 0.05,
  all(c("75056", "69123", "13055") %in% wide$code_commune),
  all(wide$abst_pres22_t1 >= 0 & wide$abst_pres22_t1 <= 100, na.rm = TRUE),
  # Conservation (même fichier, calcul INDÉPENDANT de la jointure ci-dessus) : les inscrits 2022 T1
  # conservés = ceux des lignes sources dont le code est un code COG 2026 vivant OU remappable vers
  # un code vivant ; seuls les codes ni l'un ni l'autre (étranger ZZ, COM 975-988...) sont écartés.
  # Échoue si la jointure perd, double-compte ou mal remappe des inscrits.
  abs(n_src_kept_2022 - sum(long$inscrits[long$id_election == "2022_pres_t1"])) < 0.5
)
cat("\n✔ stopifnot() OK\n")
