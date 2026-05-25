# ============================================================
# scripts/04_air_atmo.R
#
# Étape 4 — Qualité de l'air : indice ATMO journalier par commune.
#
# Source : Atmo France, dataset "Indice de la qualité de l'air quotidien
#   par commune - indice ATMO" sur data.gouv.fr.
#   Ressource CSV : api/1/datasets/r/d2b9e8e6-8b0b-4bb6-9851-b4fa2efc8201
#
# Limite importante : le CSV publié ne contient que les 2-3 dates les plus
# récentes (snapshot, ~25k communes/jour). Pour de l'historique, il faudrait
# scraper le flux quotidien sur plusieurs mois ou utiliser le WFS avec
# filtre date. À faire dans une itération ultérieure si pertinent.
#
# Décision méthodo : pour cette étape, on traite le SNAPSHOT du jour J comme
# représentatif de la "qualité de l'air courante". Cela suffit pour la
# cartographie et l'analyse croisée Étape 6 (qui restera "à un instant t").
#
# Filtres / agrégation :
#   - On garde la date d'échéance la plus récente complète (où couverture > seuil).
#   - 1 ligne / commune avec : code_qual, code_no2, code_o3, code_pm10,
#     code_pm25, lib_qual, lat, lng, source AASQA.
#
# Indice ATMO : 1 = Bon, 2 = Moyen, 3 = Dégradé, 4 = Mauvais,
#               5 = Très mauvais, 6 = Extrêmement mauvais.
#
# Sortie : data/processed/air/atmo_snapshot.parquet
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(arrow)
  library(here)
  library(stringr)
  library(glue)
  library(curl)
})

URL_CSV <- "https://www.data.gouv.fr/api/1/datasets/r/d2b9e8e6-8b0b-4bb6-9851-b4fa2efc8201"

raw_dir  <- here::here("data", "raw", "air")
out_dir  <- here::here("data", "processed", "air")
raw_path <- file.path(raw_dir, "indice_atmo_quotidien.csv")
out_path <- file.path(out_dir, "atmo_snapshot.parquet")

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Téléchargement (idempotent — récupère la dernière version si absent ou ancien)
if (!file.exists(raw_path) || file.size(raw_path) < 1e6) {
  message(glue::glue("Téléchargement : {URL_CSV}"))
  curl::curl_download(URL_CSV, raw_path, quiet = FALSE)
}

# Lecture (CSV virgule, encodage UTF-8)
df <- readr::read_csv(
  raw_path,
  col_types = readr::cols(
    .default        = readr::col_character(),
    code_no2        = readr::col_integer(),
    code_o3         = readr::col_integer(),
    code_pm10       = readr::col_integer(),
    code_pm25      = readr::col_integer(),
    code_qual      = readr::col_integer(),
    code_so2       = readr::col_integer(),
    x_wgs84        = readr::col_double(),
    y_wgs84        = readr::col_double()
  ),
  show_col_types = FALSE
)

message(glue::glue("Lu : {nrow(df)} lignes ; dates : ",
                   paste(unique(df$date_ech), collapse = ", ")))

# On retient la dernière date d'échéance ayant la couverture maximale
# (les dates de prévision lointaine ont moins de communes couvertes).
coverage_by_date <- df |>
  dplyr::count(date_ech) |>
  dplyr::arrange(dplyr::desc(n))
message("Couverture par date :")
print(coverage_by_date)

best_date <- coverage_by_date$date_ech[1]
message(glue::glue("Date retenue : {best_date}"))

snapshot <- df |>
  dplyr::filter(date_ech == best_date) |>
  dplyr::transmute(
    code_commune = stringr::str_pad(code_zone, 5, "left", "0"),
    nom_commune  = lib_zone,
    date_ech     = as.Date(date_ech),
    qual_indice  = code_qual,
    qual_label   = lib_qual,
    no2_indice   = code_no2,
    o3_indice    = code_o3,
    pm10_indice  = code_pm10,
    pm25_indice  = code_pm25,
    so2_indice   = code_so2,
    lon          = x_wgs84,
    lat          = y_wgs84,
    source_aasqa = source
  ) |>
  dplyr::filter(!is.na(qual_indice))

arrow::write_parquet(snapshot, out_path)

# ---- Rapport ----

cat(glue::glue("

  ✔ {nrow(snapshot)} communes → {out_path}
    Date : {best_date}
    ({round(file.size(out_path)/1024)} KB)

  Distribution indice ATMO :
"))
print(snapshot |> dplyr::count(qual_indice, qual_label, sort = TRUE))

cat("\n  Sources AASQA actives :\n")
print(snapshot |> dplyr::count(source_aasqa, sort = TRUE))
