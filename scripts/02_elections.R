# ============================================================
# scripts/02_elections.R
#
# Étape 2 — Législatives 2024 : résultats par commune.
#
# Source : Ministère de l'Intérieur, via data.gouv.fr.
#   Datasets :
#     T1 : elections-legislatives-des-30-juin-et-7-juillet-2024-resultats-
#          definitifs-du-1er-tour
#     T2 : elections-legislatives-des-30-juin-et-7-juillet-2024-resultats-
#          provisoires-du-2nd-tour (titre interne : "définitifs")
#   Ressources utilisées : resultats-definitifs-par-commune(s).csv (T1 ~77 MB, T2 ~12 MB)
#
# Format source : CSV ; séparateur ";" ; décimales "," ; encodage UTF-8.
# Wide : 1 ligne / commune × N blocs de colonnes "candidat N".
#
# Logique :
#   - Pour chaque tour, on parse en wide → long, on calcule le vainqueur
#     commune-level (candidat avec max voix).
#   - On combine : si une commune existe au T2, on garde le résultat T2.
#     Sinon (commune dans une circo décidée au T1), on prend le résultat T1.
#   - La colonne `tour` trace si le résultat affiché vient de T1 ou T2.
#
# Produit : data/processed/elections/legislatives_2024_t2.parquet
#   1 ligne / commune, ~36 000 lignes (couverture quasi-totale).
#
# Lancement : Rscript scripts/02_elections.R
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

# ---- Sources ----

URL_T2_COMMUNE <- paste0(
  "https://static.data.gouv.fr/resources/",
  "elections-legislatives-des-30-juin-et-7-juillet-2024-resultats-",
  "definitifs-du-2nd-tour/20240710-170606/",
  "resultats-definitifs-par-commune.csv"
)

# T1 via le redirector data.gouv (URL latest stable)
URL_T1_COMMUNE <- "https://www.data.gouv.fr/api/1/datasets/r/bd32fcd3-53df-47ac-bf1d-8d8003fe23a1"

# ---- Mapping nuance MI → famille politique ----

nuance_to_famille <- c(
  # NFP / gauche unie
  "UG"  = "NFP",
  "FI"  = "NFP",
  "SOC" = "NFP",
  "COM" = "NFP",
  "VEC" = "NFP",
  # Ensemble (majorité présidentielle + Horizons)
  "ENS" = "ENS",
  "HOR" = "ENS",
  # Les Républicains + UDI
  "LR"  = "LR",
  "UDI" = "LR",
  "UDR" = "LR",
  # RN + alliés + extrême-droite hors RN
  "RN"  = "RN",
  "UXD" = "RN",
  "EXD" = "RN",
  # Divers locaux / régionaux / dissidents
  "DVG" = "Divers",
  "DVD" = "Divers",
  "DVC" = "Divers",
  "DSV" = "Divers",
  "REG" = "Divers",
  "ECO" = "Divers",
  "EXG" = "Divers",
  "DIV" = "Divers"
)

# ---- Helpers de nettoyage ----

clean_pct <- function(x) {
  x <- stringr::str_remove(x, "%")
  x <- stringr::str_replace(x, ",", ".")
  x <- stringr::str_trim(x)
  suppressWarnings(as.numeric(x))
}

clean_int <- function(x) {
  x <- stringr::str_remove_all(x, "\\s| ")  # espaces fines (séparateur milliers)
  suppressWarnings(as.integer(x))
}

# ---- Parser commun T1/T2 ----

parse_tour <- function(csv_path, tour_label) {
  message(glue::glue("→ Parsing {basename(csv_path)} (tour = {tour_label})"))

  df_wide <- readr::read_delim(
    csv_path,
    delim   = ";",
    locale  = readr::locale(decimal_mark = ",", grouping_mark = " ",
                            encoding = "UTF-8"),
    col_types = readr::cols(.default = readr::col_character()),
    na = c("", "NA"),
    trim_ws = TRUE,
    progress = FALSE
  )

  all_cols <- names(df_wide)
  candidate_cols <- all_cols[stringr::str_detect(all_cols, "\\s\\d+$")]
  max_cand <- max(as.integer(stringr::str_extract(candidate_cols, "\\d+$")),
                  na.rm = TRUE)
  message(glue::glue("  {nrow(df_wide)} communes, {ncol(df_wide)} colonnes, ",
                     "candidats max = {max_cand}"))

  df_long <- df_wide |>
    tidyr::pivot_longer(
      cols          = tidyselect::all_of(candidate_cols),
      names_to      = c(".value", "candidate_num"),
      names_pattern = "^(.*) (\\d+)$"
    ) |>
    dplyr::filter(!is.na(`Nuance candidat`) & `Nuance candidat` != "")

  # Important : la CSV T1 stocke les codes sans zéros initiaux pour les
  # départements 01-09 ("1", "1001" au lieu de "01", "01001") alors que T2
  # les conserve. On normalise systématiquement avec str_pad pour avoir des
  # codes 5 chars (commune) et 2 chars (département). Sans cette étape,
  # ~5000 communes (Aisne, Alpes-Maritimes, Ariège, Cantal, Aube, etc.) ne
  # joignent pas avec le GeoJSON.
  df_long |>
    dplyr::transmute(
      code_insee        = stringr::str_pad(`Code commune`,      width = 5,
                                           side = "left", pad = "0"),
      nom_commune       = `Libellé commune`,
      code_dept         = stringr::str_pad(`Code département`,  width = 2,
                                           side = "left", pad = "0"),
      libelle_dept      = `Libellé département`,
      inscrits          = clean_int(Inscrits),
      votants           = clean_int(Votants),
      exprimes          = clean_int(`Exprimés`),
      abstentions       = clean_int(Abstentions),
      pct_abstention    = clean_pct(`% Abstentions`),
      blancs            = clean_int(Blancs),
      nuls              = clean_int(Nuls),
      candidate_num     = as.integer(candidate_num),
      nuance            = `Nuance candidat`,
      voix              = clean_int(Voix),
      pct_voix_exprimes = clean_pct(`% Voix/exprimés`),
      elu               = !is.na(Elu) & stringr::str_detect(tolower(Elu), "lu"),
      tour              = tour_label
    ) |>
    dplyr::mutate(
      famille = dplyr::coalesce(unname(nuance_to_famille[nuance]), "Divers")
    )
}

# ---- Agrégation commune-level ----

aggregate_commune <- function(df_long) {
  ranked <- df_long |>
    dplyr::group_by(code_insee) |>
    dplyr::arrange(dplyr::desc(voix), .by_group = TRUE) |>
    dplyr::mutate(rang = dplyr::row_number()) |>
    dplyr::ungroup()

  vainqueur <- ranked |>
    dplyr::filter(rang == 1L) |>
    dplyr::transmute(
      code_insee,
      tour,
      nuance_vainqueur  = nuance,
      famille_vainqueur = famille,
      voix_vainqueur    = voix,
      pct_vainqueur     = pct_voix_exprimes
    )

  second <- ranked |>
    dplyr::filter(rang == 2L) |>
    dplyr::transmute(
      code_insee,
      voix_second = voix,
      pct_second  = pct_voix_exprimes
    )

  pct_par_famille <- df_long |>
    dplyr::group_by(code_insee, famille) |>
    dplyr::summarise(pct = sum(pct_voix_exprimes, na.rm = TRUE),
                     .groups = "drop") |>
    tidyr::pivot_wider(
      names_from   = famille,
      values_from  = pct,
      names_prefix = "pct_"
    )

  fixes <- df_long |>
    dplyr::distinct(code_insee, nom_commune, code_dept, libelle_dept,
                    inscrits, votants, exprimes, abstentions, pct_abstention)

  fixes |>
    dplyr::left_join(vainqueur,       by = "code_insee") |>
    dplyr::left_join(second,          by = "code_insee") |>
    dplyr::left_join(pct_par_famille, by = "code_insee") |>
    dplyr::mutate(
      marge_vainqueur = pct_vainqueur - dplyr::coalesce(pct_second, 0)
    )
}

# ---- Chemins ----

raw_dir   <- here::here("data", "raw", "elections")
out_dir   <- here::here("data", "processed", "elections")
t1_path   <- file.path(raw_dir, "resultats-2024-t1-par-commune.csv")
t2_path   <- file.path(raw_dir, "resultats-2024-t2-par-commune.csv")
out_path  <- file.path(out_dir, "legislatives_2024_t2.parquet")

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Téléchargement (idempotent) ----

if (!file.exists(t2_path) || file.size(t2_path) < 5e6) {
  message(glue::glue("Téléchargement T2 ({URL_T2_COMMUNE})"))
  curl::curl_download(URL_T2_COMMUNE, t2_path, quiet = FALSE)
}
if (!file.exists(t1_path) || file.size(t1_path) < 5e7) {
  message(glue::glue("Téléchargement T1 ({URL_T1_COMMUNE})"))
  curl::curl_download(URL_T1_COMMUNE, t1_path, quiet = FALSE)
}

# ---- Pipeline ----

agg_t2 <- parse_tour(t2_path, "T2") |> aggregate_commune()
agg_t1 <- parse_tour(t1_path, "T1") |> aggregate_commune()

# Combine : T2 prioritaire, T1 comble les trous (circo décidée au T1).
# La colonne `tour` permet de tracer quel tour est affiché par commune.
out <- dplyr::bind_rows(
  agg_t2,
  agg_t1 |> dplyr::anti_join(agg_t2, by = "code_insee")
)

# T1 universel : toutes les communes votent au 1er tour, on garde aussi
# les % par famille au T1 (utile pour afficher T1 ET T2 côte à côte dans
# le panneau Élections).
agg_t1_extra <- agg_t1 |>
  dplyr::transmute(
    code_insee,
    pct_NFP_t1        = pct_NFP,
    pct_ENS_t1        = pct_ENS,
    pct_RN_t1         = pct_RN,
    pct_LR_t1         = pct_LR,
    pct_Divers_t1     = pct_Divers,
    pct_abstention_t1 = pct_abstention,
    nuance_vainqueur_t1  = nuance_vainqueur,
    famille_vainqueur_t1 = famille_vainqueur,
    pct_vainqueur_t1     = pct_vainqueur
  )

out <- out |>
  dplyr::left_join(agg_t1_extra, by = "code_insee")

# ---- Écriture ----

arrow::write_parquet(out, out_path)

# ---- Rapport ----

n_communes <- nrow(out)
by_tour    <- out |> dplyr::count(tour, name = "n_communes")
by_famille <- out |> dplyr::count(famille_vainqueur, sort = TRUE)
by_tour_famille <- out |> dplyr::count(tour, famille_vainqueur) |>
  tidyr::pivot_wider(names_from = tour, values_from = n, values_fill = 0L)

cat(glue::glue("

  ✔ {n_communes} communes traitées → {out_path}
    ({round(file.size(out_path)/1024)} KB)

  Couverture par tour :
"))
print(by_tour)

cat("\n  Famille gagnante × tour :\n")
print(by_tour_famille)
