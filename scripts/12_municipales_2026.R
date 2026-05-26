# ============================================================
# scripts/12_municipales_2026.R
#
# Élections municipales 2026, T1 + T2 par commune.
#
# Sources : data.gouv.fr Min. Intérieur
#   T1 commune : 4feeef01-24f7-4d5a-914f-8aa806f31ec2 (13.8 MB)
#   T2 commune : 6ff67a28-01bf-459e-beca-dd7aa8132dc1 (873 KB)
#
# Stratégie comme Législatives :
#   - T1 : toutes les communes (~35k) — vainqueur si élu au T1
#   - T2 : 1 526 communes ayant nécessité un 2nd tour
#   - Combine : T2 prioritaire, T1 comble (élections décidées au T1)
#   - Colonne `tour` trace le tour décisif par commune
#
# Sortie : data/processed/municipales_2026/resultats.parquet
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(arrow)
  library(here)
  library(curl)
  library(glue)
})

URL_T1 <- paste0("https://static.data.gouv.fr/resources/",
                 "elections-municipales-2026-resultats-du-premier-tour/",
                 "20260320-164339/",
                 "municipales-2026-resultats-communes-2026-03-20.csv")
URL_T2 <- paste0("https://static.data.gouv.fr/resources/",
                 "elections-municipales-2026-resultats-du-scond-tour/",
                 "20260323-180124/",
                 "municipales-2026-resultats-communes-2026-03-23-16h14.csv")

raw_dir  <- here::here("data", "raw", "municipales_2026")
out_dir  <- here::here("data", "processed", "municipales_2026")
t1_path  <- file.path(raw_dir, "resultats_t1_communes.csv")
t2_path  <- file.path(raw_dir, "resultats_t2_communes.csv")
out_path <- file.path(out_dir, "resultats.parquet")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Mapping nuance liste municipales 2026 (préfixe "L") → familles politiques
nuance_to_famille_mun <- c(
  # NFP (gauche unie + listes membres)
  "LUG"  = "NFP", "LFI" = "NFP", "LSOC" = "NFP",
  "LCOM" = "NFP", "LVEC" = "NFP", "LECO" = "NFP",
  # Ensemble (centre + Union du Centre)
  "LUC"  = "ENS",
  # Droite parlementaire
  "LLR"  = "LR",  "LUDI" = "LR", "LUDR" = "LR", "LUD" = "LR",
  # Extrême droite
  "LRN"  = "RN",  "LUXD" = "RN", "LEXD" = "RN",
  # Divers locaux / régionalistes
  "LDIV" = "Divers", "LDVG" = "Divers", "LDVD" = "Divers", "LDVC" = "Divers",
  "LREG" = "Divers"
)

clean_pct <- function(x) {
  x <- stringr::str_remove(x, "%")
  x <- stringr::str_replace(x, ",", ".")
  suppressWarnings(as.numeric(stringr::str_trim(x)))
}
clean_int <- function(x) {
  x <- stringr::str_remove_all(x, "\\s| ")
  suppressWarnings(as.integer(x))
}

# Téléchargements
if (!file.exists(t1_path) || file.size(t1_path) < 5e6) {
  message(glue::glue("Téléchargement T1 ({URL_T1})"))
  curl::curl_download(URL_T1, t1_path, quiet = FALSE)
}
if (!file.exists(t2_path) || file.size(t2_path) < 1e5) {
  message(glue::glue("Téléchargement T2 ({URL_T2})"))
  curl::curl_download(URL_T2, t2_path, quiet = FALSE)
}

# ---- Parser commun T1/T2 ----
parse_tour <- function(csv_path, tour_label) {
  message(glue::glue("→ Parsing {basename(csv_path)} (tour = {tour_label})"))
  df_wide <- readr::read_delim(
    csv_path, delim = ";",
    locale = readr::locale(decimal_mark = ",", grouping_mark = " ",
                           encoding = "UTF-8"),
    col_types = readr::cols(.default = readr::col_character()),
    na = c("", "NA"), trim_ws = TRUE, show_col_types = FALSE
  )
  all_cols <- names(df_wide)
  candidate_cols <- all_cols[stringr::str_detect(all_cols, "\\s\\d+$")]
  max_cand <- max(as.integer(stringr::str_extract(candidate_cols, "\\d+$")),
                  na.rm = TRUE)
  message(glue::glue("  {nrow(df_wide)} communes, candidats max = {max_cand}"))

  df_long <- df_wide |>
    tidyr::pivot_longer(
      cols          = tidyselect::all_of(candidate_cols),
      names_to      = c(".value", "candidate_num"),
      names_pattern = "^(.*) (\\d+)$"
    ) |>
    dplyr::filter(!is.na(`Nuance liste`) & `Nuance liste` != "")

  df_long |>
    dplyr::transmute(
      code_insee     = `Code commune`,
      nom_commune    = `Libellé commune`,
      code_dept      = `Code département`,
      libelle_dept   = `Libellé département`,
      inscrits       = clean_int(Inscrits),
      votants        = clean_int(Votants),
      exprimes       = clean_int(`Exprimés`),
      abstentions    = clean_int(Abstentions),
      pct_abstention = clean_pct(`% Abstentions`),
      candidate_num  = as.integer(candidate_num),
      nuance         = `Nuance liste`,
      libelle_liste  = `Libellé abrégé de liste`,
      voix           = clean_int(Voix),
      pct_voix_exprimes = clean_pct(`% Voix/exprimés`),
      elu            = !is.na(Elu) & stringr::str_detect(tolower(Elu), "lu"),
      tour           = tour_label
    ) |>
    dplyr::mutate(
      famille = dplyr::coalesce(unname(nuance_to_famille_mun[nuance]), "Divers")
    )
}

aggregate_commune <- function(df_long) {
  ranked <- df_long |>
    dplyr::group_by(code_insee) |>
    dplyr::arrange(dplyr::desc(voix), .by_group = TRUE) |>
    dplyr::mutate(rang = dplyr::row_number()) |>
    dplyr::ungroup()
  vainqueur <- ranked |> dplyr::filter(rang == 1L) |>
    dplyr::transmute(code_insee, tour,
                     nuance_vainqueur  = nuance,
                     famille_vainqueur = famille,
                     libelle_vainqueur = libelle_liste,
                     voix_vainqueur    = voix,
                     pct_vainqueur     = pct_voix_exprimes)
  second <- ranked |> dplyr::filter(rang == 2L) |>
    dplyr::transmute(code_insee, voix_second = voix,
                     pct_second  = pct_voix_exprimes)
  fixes <- df_long |>
    dplyr::distinct(code_insee, nom_commune, code_dept, libelle_dept,
                    inscrits, votants, exprimes, abstentions, pct_abstention)
  fixes |>
    dplyr::left_join(vainqueur, by = "code_insee") |>
    dplyr::left_join(second,    by = "code_insee") |>
    dplyr::mutate(marge_vainqueur = pct_vainqueur - dplyr::coalesce(pct_second, 0))
}

# ---- Pipeline ----
agg_t1 <- parse_tour(t1_path, "T1") |> aggregate_commune()
agg_t2 <- parse_tour(t2_path, "T2") |> aggregate_commune()

# Combine : T2 prioritaire, T1 comble (élections décidées au T1)
out <- dplyr::bind_rows(
  agg_t2,
  agg_t1 |> dplyr::anti_join(agg_t2, by = "code_insee")
)

arrow::write_parquet(out, out_path)

cat(glue::glue("

  ✔ {nrow(out)} communes → {out_path}
    ({round(file.size(out_path)/1024)} KB)

  Couverture par tour :
"))
print(out |> dplyr::count(tour))

cat("\n  Famille gagnante × tour :\n")
print(out |> dplyr::count(tour, famille_vainqueur) |>
        tidyr::pivot_wider(names_from = tour, values_from = n, values_fill = 0L))
