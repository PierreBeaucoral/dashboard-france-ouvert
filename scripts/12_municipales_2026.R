# ============================================================
# scripts/12_municipales_2026.R
#
# Élections municipales 2026, 2nd tour, par commune.
#
# Source : data.gouv.fr Ministère de l'Intérieur,
#   dataset 69c17fed9f18c7781fd11a14
#   Ressource "Municipales 2026 - Résultats - Communes_2026-03-23.csv"
#
# Format wide identique aux Législatives (1 ligne / commune, blocs candidat).
# Nuances "liste" (LCOM, DVG, DVD, UG, ENS, LR, RN, etc.).
#
# ⚠ Limite : ce fichier ne contient QUE les communes ayant eu un 2nd tour
# (1 527 communes). Les ~32k communes élues au 1er tour (la grande
# majorité, surtout petites communes) ne sont pas dans cette source.
# Pour avoir la couverture complète, il faudrait combiner avec le 1er tour
# (sa propre ressource, à venir).
#
# Sortie : data/processed/municipales_2026/resultats_t2.parquet
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(arrow)
  library(here)
  library(glue)
})

URL_T2_COMMUNES <- paste0(
  "https://static.data.gouv.fr/resources/",
  "elections-municipales-2026-resultats-du-scond-tour/",
  "20260323-180124/",
  "municipales-2026-resultats-communes-2026-03-23-16h14.csv"
)

raw_path <- here::here("data", "raw", "municipales_2026",
                       "resultats_t2_communes.csv")
out_dir  <- here::here("data", "processed", "municipales_2026")
out_path <- file.path(out_dir, "resultats_t2.parquet")
dir.create(dirname(raw_path), recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(raw_path) || file.size(raw_path) < 1e5) {
  message(glue::glue("Téléchargement {URL_T2_COMMUNES}"))
  curl::curl_download(URL_T2_COMMUNES, raw_path, quiet = FALSE)
}

# Mapping nuance liste municipales → famille politique
# (les nuances Mun 2026 incluent : LCOM local, DVG/DVD/DVC divers,
# UG union gauche, ENS, LR, UDI, RN, UXD, EXG, EXD, DSV, REG, ECO, …)
# Les nuances Municipales 2026 sont préfixées "L" (= Liste).
# Mapping vers les 5 familles utilisées partout dans le dashboard.
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

df_wide <- readr::read_delim(
  raw_path, delim = ";",
  locale = readr::locale(decimal_mark = ",", grouping_mark = " ",
                         encoding = "UTF-8"),
  col_types = readr::cols(.default = readr::col_character()),
  na = c("", "NA"), trim_ws = TRUE, show_col_types = FALSE
)
message(glue::glue("Lu : {nrow(df_wide)} communes, {ncol(df_wide)} colonnes"))

all_cols <- names(df_wide)
candidate_cols <- all_cols[stringr::str_detect(all_cols, "\\s\\d+$")]
max_cand <- max(as.integer(stringr::str_extract(candidate_cols, "\\d+$")), na.rm = TRUE)
message(glue::glue("Max candidats : {max_cand}"))

# Pivot long candidat
df_long <- df_wide |>
  tidyr::pivot_longer(
    cols          = tidyselect::all_of(candidate_cols),
    names_to      = c(".value", "candidate_num"),
    names_pattern = "^(.*) (\\d+)$"
  ) |>
  dplyr::filter(!is.na(`Nuance liste`) & `Nuance liste` != "")

df_long <- df_long |>
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
    elu            = !is.na(Elu) & stringr::str_detect(tolower(Elu), "lu")
  ) |>
  dplyr::mutate(
    famille = dplyr::coalesce(unname(nuance_to_famille_mun[nuance]), "Divers")
  )

# Vainqueur par commune
ranked <- df_long |>
  dplyr::group_by(code_insee) |>
  dplyr::arrange(dplyr::desc(voix), .by_group = TRUE) |>
  dplyr::mutate(rang = dplyr::row_number()) |>
  dplyr::ungroup()

vainqueur <- ranked |> dplyr::filter(rang == 1) |>
  dplyr::transmute(code_insee,
                   nuance_vainqueur  = nuance,
                   famille_vainqueur = famille,
                   libelle_vainqueur = libelle_liste,
                   voix_vainqueur    = voix,
                   pct_vainqueur     = pct_voix_exprimes)
second <- ranked |> dplyr::filter(rang == 2) |>
  dplyr::transmute(code_insee,
                   voix_second = voix,
                   pct_second  = pct_voix_exprimes)
fixes <- df_long |>
  dplyr::distinct(code_insee, nom_commune, code_dept, libelle_dept,
                  inscrits, votants, exprimes, abstentions, pct_abstention)

out <- fixes |>
  dplyr::left_join(vainqueur, by = "code_insee") |>
  dplyr::left_join(second,    by = "code_insee") |>
  dplyr::mutate(marge_vainqueur = pct_vainqueur - dplyr::coalesce(pct_second, 0))

arrow::write_parquet(out, out_path)

cat(glue::glue("

  ✔ Municipales 2026 T2 · {nrow(out)} communes → {out_path}
    ({round(file.size(out_path)/1024)} KB)

  Famille gagnante (T2 seulement) :
"))
print(out |> dplyr::count(famille_vainqueur, sort = TRUE))
