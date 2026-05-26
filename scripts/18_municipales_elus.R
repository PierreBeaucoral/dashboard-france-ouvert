# ============================================================
# scripts/18_municipales_elus.R
#
# Couvre les ~35 000 communes (vs ~3 300 pour le fichier "résultats-communes"
# qui limite aux ≥ 1 000 hab.). Source : fichier des **élus** Tour 1 et Tour 2,
# Ministère Intérieur — 1 ligne par élu (469 k lignes T1 + ~50 k lignes T2).
#
# Indicateurs calculés par commune (toutes communes, scrutin liste OU
# scrutin individuel) :
#   - n_elus           : nombre total d'élus au conseil municipal
#   - pct_femmes       : % de femmes élues
#   - age_median       : âge médian des élus (calculé depuis date naissance)
#   - top_profession   : code profession le plus fréquent (CODPRO)
#   - tour_decisif     : T1 si entièrement T1, T2 si présence T2
#
# Sortie : data/processed/municipales_2026/elus_commune.parquet
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(arrow)
  library(here)
  library(curl)
})

URL_T1 <- paste0(
  "https://static.data.gouv.fr/resources/",
  "elections-municipales-2026-resultats-du-premier-tour/",
  "20260320-164100/municipales-2026-candidats-elus-france-entiere-tour-1-2026-03-20.csv"
)
URL_T2 <- paste0(
  "https://static.data.gouv.fr/resources/",
  "elections-municipales-2026-resultats-du-scond-tour/",
  "20260323-180122/municipales-2026-candidats-elus-france-entiere-tour-2-2026-03-23.csv"
)

raw_dir  <- here::here("data", "raw", "municipales_2026")
out_dir  <- here::here("data", "processed", "municipales_2026")
t1_path  <- file.path(raw_dir, "elus_t1.csv")
t2_path  <- file.path(raw_dir, "elus_t2.csv")
out_path <- file.path(out_dir, "elus_commune.parquet")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(t1_path) || file.size(t1_path) < 1e7) {
  message("Téléchargement élus T1 (33 MB)…")
  curl::curl_download(URL_T1, t1_path, quiet = FALSE)
}
if (!file.exists(t2_path) || file.size(t2_path) < 1e6) {
  message("Téléchargement élus T2 (3 MB)…")
  curl::curl_download(URL_T2, t2_path, quiet = FALSE)
}

# Parser commun
parse_elus <- function(path, tour_label) {
  df <- readr::read_delim(
    path, delim = ";",
    locale = readr::locale(encoding = "UTF-8"),
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  ) |>
    dplyr::transmute(
      code_dept    = CODDPT,
      code_com     = CODCOM,
      sexe         = SEXPSN,
      date_nais    = DATNAIPSN,
      codpro       = CODPRO,
      effectif_legal = suppressWarnings(as.integer(EFFECTIF_LEGAL)),
      tour         = tour_label
    ) |>
    dplyr::filter(!is.na(code_com))
  df
}

t1 <- parse_elus(t1_path, "T1")
t2 <- parse_elus(t2_path, "T2")
message(sprintf("→ %d élus T1, %d élus T2", nrow(t1), nrow(t2)))

# Combine : on garde tous les élus, indique le tour décisif par commune
all_elus <- dplyr::bind_rows(t1, t2)
tour_decisif_commune <- all_elus |>
  dplyr::group_by(code_com) |>
  dplyr::summarise(tour_decisif = ifelse(any(tour == "T2"), "T2", "T1"),
                   .groups = "drop")

# Calcul âge à la date du 1er tour (15 mars 2026)
ref_date <- as.Date("2026-03-15")
all_elus <- all_elus |>
  dplyr::mutate(
    naissance = suppressWarnings(as.Date(date_nais)),
    age       = as.integer(as.numeric(ref_date - naissance) / 365.25)
  ) |>
  dplyr::filter(!is.na(age), age >= 18, age <= 110)

# Agrégation par commune
agg <- all_elus |>
  dplyr::group_by(code_dept, code_com) |>
  dplyr::summarise(
    n_elus       = dplyr::n(),
    n_femmes     = sum(sexe == "F", na.rm = TRUE),
    pct_femmes   = round(100 * n_femmes / n_elus, 1),
    age_median   = median(age, na.rm = TRUE),
    age_moyen    = round(mean(age, na.rm = TRUE), 1),
    effectif_legal = first(effectif_legal),
    .groups = "drop"
  ) |>
  dplyr::left_join(tour_decisif_commune, by = "code_com")

# Top profession par commune (mode CODPRO)
top_pro <- all_elus |>
  dplyr::filter(!is.na(codpro), codpro != "") |>
  dplyr::count(code_com, codpro) |>
  dplyr::group_by(code_com) |>
  dplyr::slice_max(n, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::transmute(code_com,
                   top_codpro = codpro,
                   n_top_codpro = n)
agg <- agg |> dplyr::left_join(top_pro, by = "code_com")

# Renommer pour cohérence avec les autres parquets
out <- agg |>
  dplyr::transmute(
    code_commune = code_com,
    n_elus, n_femmes, pct_femmes,
    age_median, age_moyen,
    effectif_legal,
    top_codpro, n_top_codpro,
    tour_decisif
  )

arrow::write_parquet(out, out_path)

cat(sprintf("\n✔ %d communes → %s (%.1f KB)\n",
            nrow(out), out_path, file.size(out_path) / 1024))
cat("\nDistribution pct_femmes :\n"); print(summary(out$pct_femmes))
cat("\nDistribution age_median :\n"); print(summary(out$age_median))
cat("\nDistribution n_elus :\n"); print(summary(out$n_elus))
cat("\nRépartition tour décisif :\n"); print(table(out$tour_decisif))
