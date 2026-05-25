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

# Stratégie : agréger les 3 dates disponibles (J-1, J, J+1 selon le moment du
# téléchargement) en prenant la valeur la plus récente par commune. Certaines
# AASQA publient sur des dates différentes — combiner maximise la couverture.
coverage_by_date <- df |>
  dplyr::count(date_ech) |>
  dplyr::arrange(dplyr::desc(date_ech))
message("Couverture par date :")
print(coverage_by_date)

best_date <- max(df$date_ech, na.rm = TRUE)
message(glue::glue("Date la plus récente affichée : {best_date}"))

snapshot <- df |>
  dplyr::filter(!is.na(code_qual)) |>
  # Pour chaque commune, prendre l'observation la plus récente
  dplyr::group_by(code_zone) |>
  dplyr::slice_max(date_ech, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
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

# ---- Fallback : moyenne départementale pour les communes sans valeur directe ----
# La couverture AASQA est très inégale : Air Breizh ne publie que 61 communes
# sur ~1200 de Bretagne, Atmo Pays de la Loire 404/1240, Atmo-Occitanie
# 164/4500. Pour boucher ces trous, on impute la moyenne du département
# (à partir des communes effectivement publiées dans le département) et on
# trace la provenance dans la colonne `imputed`.

snapshot <- snapshot |>
  dplyr::mutate(
    code_dept = ifelse(substr(code_commune, 1, 2) == "97",
                       substr(code_commune, 1, 3),
                       substr(code_commune, 1, 2))
  )

# Mapping dept → région (métropole + DROM) pour le fallback régional
dept_to_region <- c(
  # Auvergne-Rhône-Alpes
  "01"="ARA","03"="ARA","07"="ARA","15"="ARA","26"="ARA","38"="ARA",
  "42"="ARA","43"="ARA","63"="ARA","69"="ARA","73"="ARA","74"="ARA",
  # Bourgogne-Franche-Comté
  "21"="BFC","25"="BFC","39"="BFC","58"="BFC","70"="BFC","71"="BFC",
  "89"="BFC","90"="BFC",
  # Bretagne
  "22"="BRE","29"="BRE","35"="BRE","56"="BRE",
  # Centre-Val de Loire
  "18"="CVL","28"="CVL","36"="CVL","37"="CVL","41"="CVL","45"="CVL",
  # Corse
  "2A"="COR","2B"="COR",
  # Grand Est
  "08"="GES","10"="GES","51"="GES","52"="GES","54"="GES","55"="GES",
  "57"="GES","67"="GES","68"="GES","88"="GES",
  # Hauts-de-France
  "02"="HDF","59"="HDF","60"="HDF","62"="HDF","80"="HDF",
  # Île-de-France
  "75"="IDF","77"="IDF","78"="IDF","91"="IDF","92"="IDF","93"="IDF",
  "94"="IDF","95"="IDF",
  # Normandie
  "14"="NOR","27"="NOR","50"="NOR","61"="NOR","76"="NOR",
  # Nouvelle-Aquitaine
  "16"="NAQ","17"="NAQ","19"="NAQ","23"="NAQ","24"="NAQ","33"="NAQ",
  "40"="NAQ","47"="NAQ","64"="NAQ","79"="NAQ","86"="NAQ","87"="NAQ",
  # Occitanie
  "09"="OCC","11"="OCC","12"="OCC","30"="OCC","31"="OCC","32"="OCC",
  "34"="OCC","46"="OCC","48"="OCC","65"="OCC","66"="OCC","81"="OCC",
  "82"="OCC",
  # Pays de la Loire
  "44"="PDL","49"="PDL","53"="PDL","72"="PDL","85"="PDL",
  # Provence-Alpes-Côte d'Azur
  "04"="PAC","05"="PAC","06"="PAC","13"="PAC","83"="PAC","84"="PAC",
  # DROM
  "971"="GUA","972"="MAR","973"="GUY","974"="REU","976"="MAY"
)

snapshot$region <- unname(dept_to_region[snapshot$code_dept])

dept_means <- snapshot |>
  dplyr::group_by(code_dept) |>
  dplyr::summarise(
    qual_dept = round(mean(qual_indice, na.rm = TRUE)),
    no2_dept  = round(mean(no2_indice,  na.rm = TRUE)),
    o3_dept   = round(mean(o3_indice,   na.rm = TRUE)),
    pm10_dept = round(mean(pm10_indice, na.rm = TRUE)),
    pm25_dept = round(mean(pm25_indice, na.rm = TRUE)),
    n_dept    = dplyr::n(),
    .groups   = "drop"
  )

region_means <- snapshot |>
  dplyr::filter(!is.na(region)) |>
  dplyr::group_by(region) |>
  dplyr::summarise(
    qual_reg = round(mean(qual_indice, na.rm = TRUE)),
    no2_reg  = round(mean(no2_indice,  na.rm = TRUE)),
    o3_reg   = round(mean(o3_indice,   na.rm = TRUE)),
    pm10_reg = round(mean(pm10_indice, na.rm = TRUE)),
    pm25_reg = round(mean(pm25_indice, na.rm = TRUE)),
    n_reg    = dplyr::n(),
    .groups  = "drop"
  )

# Univers de toutes les communes : si possible depuis le parquet élections
# (couverture quasi-totale 35k). Sinon on garde juste snapshot.
elec_path <- here::here("data", "processed", "elections",
                       "legislatives_2024_t2.parquet")

if (file.exists(elec_path)) {
  all_communes <- arrow::read_parquet(elec_path) |>
    dplyr::transmute(
      code_commune = code_insee,
      nom_full     = nom_commune,
      code_dept_e  = code_dept
    )

  full <- all_communes |>
    dplyr::left_join(snapshot, by = "code_commune") |>
    dplyr::mutate(
      code_dept = dplyr::coalesce(code_dept, code_dept_e),
      nom_commune = dplyr::coalesce(nom_commune, nom_full),
      region = unname(dept_to_region[code_dept])
    ) |>
    dplyr::left_join(dept_means,   by = "code_dept") |>
    dplyr::left_join(region_means, by = "region") |>
    dplyr::mutate(
      # 1) direct ; 2) dept-mean ; 3) région-mean
      imputation = dplyr::case_when(
        !is.na(qual_indice)                          ~ "direct",
        !is.na(qual_dept)                            ~ "dept",
        !is.na(qual_reg)                             ~ "region",
        TRUE                                          ~ "na"
      ),
      imputed = imputation != "direct",
      qual_indice = dplyr::coalesce(qual_indice, qual_dept, qual_reg),
      no2_indice  = dplyr::coalesce(no2_indice,  no2_dept,  no2_reg),
      o3_indice   = dplyr::coalesce(o3_indice,   o3_dept,   o3_reg),
      pm10_indice = dplyr::coalesce(pm10_indice, pm10_dept, pm10_reg),
      pm25_indice = dplyr::coalesce(pm25_indice, pm25_dept, pm25_reg),
      qual_label  = dplyr::coalesce(qual_label,
                                    c("0"="Absent","1"="Bon","2"="Moyen",
                                      "3"="Dégradé","4"="Mauvais",
                                      "5"="Très mauvais",
                                      "6"="Extrêmement mauvais")[as.character(qual_indice)])
    ) |>
    # Normaliser la casse des labels (data Atmo mélange "Dégradé"/"dégradé")
    dplyr::mutate(qual_label = stringr::str_to_sentence(qual_label)) |>
    dplyr::select(code_commune, nom_commune, date_ech,
                  qual_indice, qual_label,
                  no2_indice, o3_indice, pm10_indice, pm25_indice,
                  source_aasqa, imputed, imputation) |>
    dplyr::filter(!is.na(qual_indice))

  snapshot <- full
  message(glue::glue("  Après fallback dept-mean : {nrow(snapshot)} communes ",
                     "(dont {sum(snapshot$imputed)} imputées)"))
} else {
  message("⚠ legislatives_2024_t2.parquet non trouvé — pas de fallback dept-mean.")
}

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
