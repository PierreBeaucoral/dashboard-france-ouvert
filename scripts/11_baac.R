# ============================================================
# scripts/11_baac.R
#
# Bloc C/I — BAAC (Bulletin d'Analyse des Accidents Corporels)
#   Accidents corporels de la circulation routière 2024.
#
# Source : data.gouv.fr, dataset 53698f4ca3a729239d2036df
#   (Ministère de l'Intérieur · ONISR Observatoire National
#    Interministériel de la Sécurité Routière)
#
# On retient le fichier "caractéristiques" (1 ligne / accident, avec
# code commune). On compte les accidents par commune sur 2024.
# Normalisation par 1000 hab → taux d'accidentologie comparable.
#
# Sortie : data/processed/baac/accidents_commune.parquet
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(arrow)
  library(here)
  library(stringr)
  library(glue)
  library(tidyr)
  library(curl)
})

raw_path <- here::here("data", "raw", "baac", "caract_2024.csv")
# Téléchargement (audit N1) : ressource « Caract_2024.csv » du jeu ONISR 53698f4ca3a729239d2036df
# (URL « latest » data.gouv.fr, stable), mise en cache ; le refresh part d'un data/raw/ vide.
URL_BAAC <- "https://www.data.gouv.fr/api/1/datasets/r/83f0fb0e-e0ef-47fe-93dd-9aaee851674a"
dir.create(dirname(raw_path), recursive = TRUE, showWarnings = FALSE)
if (!file.exists(raw_path) || file.size(raw_path) < 1e6) {
  message("  Télécharge : ", URL_BAAC)
  curl::curl_download(URL_BAAC, raw_path, quiet = TRUE, handle = curl::new_handle(useragent = "Mozilla/5.0"))
}
out_dir  <- here::here("data", "processed", "baac")
out_path <- file.path(out_dir, "accidents_commune.parquet")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# CSV séparateur ";", quotation "
caract <- readr::read_delim(raw_path, delim = ";",
                            show_col_types = FALSE,
                            locale = readr::locale(encoding = "UTF-8"))
message(glue::glue("BAAC caract 2024 : {nrow(caract)} accidents, ",
                   "cols : {paste(names(caract), collapse=', ')}"))

# BAAC : la colonne `com` contient déjà le code INSEE 5 chars complet
# (inutile de concaténer avec dep). Quelques rares NA possibles.
caract <- caract |>
  dplyr::filter(!is.na(com), nchar(as.character(com)) == 5) |>
  dplyr::mutate(code_commune = as.character(com))

# Harmonisation COG 2026 (audit N3) : le BAAC 2024 est au COG 2024, alors que le dénominateur
# (populations.parquet) est sommé sur le COG 2026. Sans remappage, les accidents d'une commune
# absorbée ou renumérotée (Saint-Denis 93066 + Pierrefitte, Orée d'Anjou 49069 -> 49126...)
# étaient écartés « hors cadre » et la commune affichait 0 accident pour une population réelle.
# On somme les effectifs vers le code 2026 (jamais de taux) avant l'agrégation.
source(here::here("scripts", "cog_helpers.R"))
caract <- caract |>
  dplyr::mutate(code_commune = to_cog2026(code_commune, cog_map("2024-01-01")))

# Compte par commune (communes avec >= 1 accident)
agg <- caract |>
  dplyr::group_by(code_commune) |>
  dplyr::summarise(
    n_accidents_2024 = dplyr::n(),
    n_acc_intersection = sum(int != 1, na.rm = TRUE),
    n_acc_nuit = sum(lum %in% c(3, 4, 5), na.rm = TRUE),  # nuit
    .groups = "drop"
  )

# Cadre = TOUTES les communes (COM + arrondissements, populations INSEE) : une
# commune sans accident corporel enregistré vaut 0, pas NA. Le BAAC couvre
# France entière (métropole + DROM), donc l'absence de ligne = 0 accident.
# (Avant : seules les ~11 300 communes avec >= 1 accident étaient écrites, les
# autres devenaient NA après jointure -> taux conditionnel à >= 1 accident.)
pop_path <- here::here("data", "processed", "insee", "populations.parquet")
frame <- arrow::read_parquet(pop_path) |> dplyr::distinct(code_commune)

# PLM : le BAAC mélange code ville (75056) et arrondissements (751xx). La ville
# = ville + somme de ses arrondissements ; les arrondissements gardent leur compte.
plm_city <- c(`751` = "75056", `693` = "69123", `132` = "13055")
city_tot <- agg |>
  dplyr::mutate(city = dplyr::if_else(code_commune %in% plm_city, code_commune,
                                      unname(plm_city[substr(code_commune, 1, 3)]))) |>
  dplyr::filter(!is.na(city),
                code_commune %in% c(sprintf("751%02d", 1:20), sprintf("693%02d", 81:89),
                                    sprintf("132%02d", 1:16), unname(plm_city))) |>
  dplyr::group_by(code_commune = city) |>
  dplyr::summarise(dplyr::across(dplyr::starts_with("n_"), sum), .groups = "drop")
agg <- dplyr::bind_rows(dplyr::filter(agg, !code_commune %in% city_tot$code_commune),
                        city_tot)

n_hors_cadre <- sum(!agg$code_commune %in% frame$code_commune)
agg_full <- frame |>
  dplyr::left_join(agg, by = "code_commune") |>
  dplyr::mutate(dplyr::across(dplyr::starts_with("n_"), ~ tidyr::replace_na(.x, 0L)))
message(glue::glue("  {n_hors_cadre} codes BAAC hors cadre INSEE (ignorés)"))
n_avec <- sum(agg_full$n_accidents_2024 > 0)
agg <- agg_full

arrow::write_parquet(agg, out_path)

cat(glue::glue("

  ✔ BAAC 2024 : {nrow(agg)} communes (cadre INSEE), dont {n_avec} avec ≥1 accident
    → {out_path}
    ({round(file.size(out_path)/1024)} KB)
    Total accidents (PLM : ville + arrondissements comptés 2 fois) : {sum(agg$n_accidents_2024)}
    Médiane / commune : {median(agg$n_accidents_2024)}
    Max (Paris probablement) : {max(agg$n_accidents_2024)}
"))
