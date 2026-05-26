# ============================================================
# scripts/13_energie_carbone.R
#
# Bloc G — Empreinte carbone territorialisée par commune.
# Source : RARE (Réseau des Agences Régionales de l'Énergie) / CITEPA,
#          calcul macroéconomique via base EXIOBASE × CITEPA.
#
# data.gouv.fr ID : 698324c8e8ca100aa8807fd2
# Resource CSV (27 MB) : 404b1641-8ad7-4eb1-b70b-51b48a8829eb
#
# Le fichier publie ~7 indicateurs × 35 000 communes en format long
# (CODGEO, NOM_INDIC, VALEUR_INDIC, UNITE_INDIC).
# On pivote en wide pour le dashboard.
#
# Sortie : data/processed/energie/carbone_commune.parquet
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(arrow)
  library(here)
  library(curl)
})

URL_CARBONE <- paste0(
  "https://static.data.gouv.fr/resources/",
  "empreinte-carbone-territorialisee-approche-macroeconomique/",
  "20260204-105246/empreinte-exiobase-2018-citepa-2-group-communes.csv"
)

raw_dir  <- here::here("data", "raw", "energie")
out_dir  <- here::here("data", "processed", "energie")
raw_path <- file.path(raw_dir, "empreinte_carbone_commune.csv")
out_path <- file.path(out_dir, "carbone_commune.parquet")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(raw_path) || file.size(raw_path) < 1e7) {
  message("Téléchargement empreinte carbone (27 MB)…")
  curl::curl_download(URL_CARBONE, raw_path, quiet = FALSE)
}

raw <- readr::read_delim(
  raw_path, delim = ",",
  locale = readr::locale(decimal_mark = ".", encoding = "UTF-8"),
  col_types = readr::cols(
    REG          = readr::col_character(),
    CODGEO       = readr::col_character(),
    ID_INDIC     = readr::col_character(),
    NOM_INDIC    = readr::col_character(),
    VALEUR_INDIC = readr::col_double(),
    UNITE_INDIC  = readr::col_character()
  ),
  show_col_types = FALSE
)

message(sprintf("→ %d lignes, %d communes uniques, %d indicateurs",
                nrow(raw),
                length(unique(raw$CODGEO)),
                length(unique(raw$NOM_INDIC))))
message("\nIndicateurs disponibles :")
print(raw |> dplyr::count(NOM_INDIC, UNITE_INDIC))

# Mapping nom long → colonne courte pour le pivot wide.
# On garde les 5 indicateurs les plus parlants pour le dashboard.
indicator_map <- c(
  "Empreinte Carbone par habitant - Produit de consommation - Tous produits de consommations" =
    "carb_hab_total",
  "Empreinte Carbone par habitant - Produit de consommation - Logement" =
    "carb_hab_logement",
  "Empreinte Carbone par habitant - Produit de consommation - Transport" =
    "carb_hab_transport",
  "Empreinte Carbone par habitant - Produit de consommation - Alimentation" =
    "carb_hab_alimentation",
  "Empreinte Carbone par habitant - Produit de consommation - Biens et services" =
    "carb_hab_biensservices"
)

# Sélectionne et pivote
keep <- raw |>
  dplyr::filter(NOM_INDIC %in% names(indicator_map)) |>
  dplyr::mutate(short = unname(indicator_map[NOM_INDIC]))

wide <- keep |>
  dplyr::select(code_commune = CODGEO, short, VALEUR_INDIC) |>
  tidyr::pivot_wider(names_from = short, values_from = VALEUR_INDIC,
                     values_fn = mean)

arrow::write_parquet(wide, out_path)

cat(sprintf("\n✔ %d communes → %s (%.1f KB)\n",
            nrow(wide), out_path, file.size(out_path) / 1024))
cat("\nAperçu :\n")
print(head(wide, 5))
cat("\nSummary carb_hab_total :\n")
print(summary(wide$carb_hab_total))
