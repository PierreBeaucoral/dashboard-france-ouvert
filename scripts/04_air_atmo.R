# ============================================================
# scripts/04_air_atmo.R
#
# Étape 4 — Qualité de l'air : indice ATMO MOYEN SUR 30 JOURS par commune.
#
# Source : Atmo France, dataset "Indice de la qualité de l'air quotidien
#   par commune - indice ATMO" — accédé via le service WFS GeoServer
#   d'Atmo France pour avoir l'historique (la ressource CSV n'expose
#   qu'une fenêtre glissante de 3 jours).
#
#   WFS URL : https://data.atmo-france.org/geoserver/ind/wfs
#   Layer   : ind:ind_atmo
#   Filtre  : CQL_FILTER=date_ech='YYYY-MM-DD'
#   Format  : CSV
#
# Couverture : 30 derniers jours, ~78k lignes/jour, ~2.3M lignes total.
# Téléchargement jour par jour (caching), 30 requêtes WFS.
#
# Décision clé : on agrège en MOYENNE par commune sur la fenêtre.
#   - Lisse le bruit météorologique du jour J (vent, pluie).
#   - Comble les NAs : les AASQA qui publient en intermittence (Atmo
#     Occitanie, Air Breizh) ont au moins quelques jours de couverture
#     sur 30 jours, là où le snapshot d'un jour J pouvait être totalement
#     absent.
#   - Plus comparable aux autres sources (élections, DVF) pour l'Étape 6.
#
# Sortie : data/processed/air/atmo_snapshot.parquet
#   1 ligne / commune (~30k) avec :
#     - qual_indice : moyenne arrondie sur la période (sert au choroplèthe)
#     - qual_mean : moyenne exacte (1 décimale)
#     - n_days : nombre de jours observés sur 30
#     - pct_bon, pct_mauvais_plus : % de jours en bon vs ≥mauvais
#     - moyennes par polluant (no2, o3, pm10, pm25)
#     - imputed (boolean), imputation (direct/dept/region/na)
#
# Lancement : Rscript scripts/04_air_atmo.R
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(arrow)
  library(here)
  library(stringr)
  library(glue)
  library(curl)
  library(purrr)
})

# ---- Paramètres ----

N_DAYS    <- 30L
WFS_BASE  <- "https://data.atmo-france.org/geoserver/ind/wfs"
WFS_LAYER <- "ind:ind_atmo"

raw_dir   <- here::here("data", "raw", "air")
daily_dir <- file.path(raw_dir, "daily")
out_dir   <- here::here("data", "processed", "air")
out_path  <- file.path(out_dir, "atmo_snapshot.parquet")

dir.create(daily_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir,   recursive = TRUE, showWarnings = FALSE)

# ---- Téléchargement 30 derniers jours via WFS ----

dates <- format(seq.Date(Sys.Date() - (N_DAYS - 1L), Sys.Date(), by = "day"),
                "%Y-%m-%d")

build_url <- function(d) {
  sprintf(
    "%s?service=WFS&version=2.0.0&request=GetFeature&typeName=%s&CQL_FILTER=date_ech=%%27%s%%27&count=100000&outputFormat=csv",
    WFS_BASE, WFS_LAYER, d
  )
}

message(glue::glue("Téléchargement {N_DAYS} jours d'indices ATMO (WFS Atmo France)…"))

for (d in dates) {
  out <- file.path(daily_dir, paste0(d, ".csv"))
  if (file.exists(out) && file.size(out) > 1000L) next
  message(glue::glue("  ↓ {d}"))
  res <- try(curl::curl_download(build_url(d), out, quiet = TRUE),
             silent = TRUE)
  if (inherits(res, "try-error") || file.size(out) < 200L) {
    message(glue::glue("    ⚠ téléchargement raté pour {d}"))
    if (file.exists(out)) file.remove(out)
  }
}

# ---- Lecture & concaténation ----

read_one <- function(d) {
  p <- file.path(daily_dir, paste0(d, ".csv"))
  if (!file.exists(p) || file.size(p) < 1000L) return(NULL)
  suppressWarnings(suppressMessages(
    readr::read_csv(
      p,
      col_types = readr::cols(
        .default        = readr::col_character(),
        code_no2        = readr::col_integer(),
        code_o3         = readr::col_integer(),
        code_pm10       = readr::col_integer(),
        code_pm25       = readr::col_integer(),
        code_qual       = readr::col_integer(),
        code_so2        = readr::col_integer(),
        x_wgs84         = readr::col_double(),
        y_wgs84         = readr::col_double()
      )
    )
  ))
}

raw <- purrr::map_dfr(dates, read_one)

if (nrow(raw) == 0) {
  stop("Aucune donnée téléchargée — vérifier la connectivité au WFS.")
}

n_days_available <- length(unique(raw$date_ech))
message(glue::glue("  {nrow(raw)} lignes lues sur {n_days_available} jours"))
message(glue::glue("  type_zone : ",
                   paste(names(table(raw$type_zone)),
                         table(raw$type_zone), sep="=", collapse=", ")))

# ---- Expansion EPCI → communes ----
# Air Breizh (Bretagne), Atmo-Occitanie, Air Pays de la Loire publient
# certains indices au niveau EPCI (intercommunalité). Sans expansion,
# ces régions apparaissent vides sur la carte commune. On télécharge le
# mapping commune→EPCI depuis geo.api.gouv.fr, on duplique chaque ligne
# EPCI en autant de lignes que de communes-membres.

epci_map_path <- file.path(raw_dir, "communes_to_epci.csv")
if (!file.exists(epci_map_path) || file.size(epci_map_path) < 100000) {
  message("Téléchargement mapping commune → EPCI (geo.api.gouv.fr)…")
  curl::curl_download(
    "https://geo.api.gouv.fr/communes?fields=code,codeEpci&format=json",
    file.path(raw_dir, "communes_to_epci.json"),
    quiet = TRUE
  )
  epci_json <- jsonlite::fromJSON(file.path(raw_dir, "communes_to_epci.json"))
  readr::write_csv(epci_json, epci_map_path)
}
epci_map <- readr::read_csv(epci_map_path, show_col_types = FALSE) |>
  dplyr::filter(!is.na(codeEpci)) |>
  dplyr::transmute(code_commune = code, code_epci = as.character(codeEpci))

# Lignes EPCI : on étend
raw_epci <- raw |>
  dplyr::filter(type_zone == "EPCI", !is.na(code_qual)) |>
  dplyr::rename(code_epci = code_zone) |>
  dplyr::inner_join(epci_map, by = "code_epci", relationship = "many-to-many") |>
  dplyr::mutate(
    code_zone   = code_commune,
    type_zone   = "commune_via_epci"
  ) |>
  dplyr::select(-code_epci, -code_commune)

# Lignes commune (incluant minuscule + MAJUSCULE)
raw_commune <- raw |>
  dplyr::filter(type_zone %in% c("commune", "COMMUNE"), !is.na(code_qual))

raw <- dplyr::bind_rows(raw_commune, raw_epci)
message(glue::glue("  Après expansion EPCI : {nrow(raw)} lignes commune-level"))

# ---- Agrégation par commune sur la fenêtre 30 jours ----

monthly <- raw |>
  dplyr::filter(!is.na(code_qual)) |>
  dplyr::mutate(code_commune = stringr::str_pad(code_zone, 5, "left", "0")) |>
  dplyr::group_by(code_commune) |>
  dplyr::summarise(
    nom_commune      = dplyr::first(lib_zone),
    n_days           = dplyr::n_distinct(date_ech),
    qual_mean        = round(mean(code_qual, na.rm = TRUE), 1),
    qual_indice      = as.integer(round(qual_mean)),
    no2_indice       = as.integer(round(mean(code_no2,  na.rm = TRUE))),
    o3_indice        = as.integer(round(mean(code_o3,   na.rm = TRUE))),
    pm10_indice      = as.integer(round(mean(code_pm10, na.rm = TRUE))),
    pm25_indice      = as.integer(round(mean(code_pm25, na.rm = TRUE))),
    pct_bon          = round(mean(code_qual == 1L, na.rm = TRUE) * 100, 1),
    pct_mauvais_plus = round(mean(code_qual >= 4L, na.rm = TRUE) * 100, 1),
    source_aasqa    = dplyr::first(source),
    .groups          = "drop"
  ) |>
  dplyr::mutate(
    qual_label = c("0"="Absent","1"="Bon","2"="Moyen","3"="Dégradé",
                   "4"="Mauvais","5"="Très mauvais",
                   "6"="Extrêmement mauvais")[as.character(qual_indice)]
  )

message(glue::glue("Après agrégation 30j : {nrow(monthly)} communes (directes)"))

# ---- Fallback hiérarchique : dept → région ----

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

monthly <- monthly |>
  dplyr::mutate(
    code_dept = ifelse(substr(code_commune, 1, 2) == "97",
                       substr(code_commune, 1, 3),
                       substr(code_commune, 1, 2)),
    region    = unname(dept_to_region[code_dept])
  )

dept_means <- monthly |>
  dplyr::group_by(code_dept) |>
  dplyr::summarise(
    qual_dept  = round(mean(qual_indice, na.rm = TRUE)),
    no2_dept   = round(mean(no2_indice,  na.rm = TRUE)),
    o3_dept    = round(mean(o3_indice,   na.rm = TRUE)),
    pm10_dept  = round(mean(pm10_indice, na.rm = TRUE)),
    pm25_dept  = round(mean(pm25_indice, na.rm = TRUE)),
    .groups    = "drop"
  )

region_means <- monthly |>
  dplyr::filter(!is.na(region)) |>
  dplyr::group_by(region) |>
  dplyr::summarise(
    qual_reg = round(mean(qual_indice, na.rm = TRUE)),
    no2_reg  = round(mean(no2_indice,  na.rm = TRUE)),
    o3_reg   = round(mean(o3_indice,   na.rm = TRUE)),
    pm10_reg = round(mean(pm10_indice, na.rm = TRUE)),
    pm25_reg = round(mean(pm25_indice, na.rm = TRUE)),
    .groups  = "drop"
  )

# ---- Expansion à toutes les communes ----

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
    dplyr::left_join(monthly, by = "code_commune") |>
    dplyr::mutate(
      code_dept   = dplyr::coalesce(code_dept, code_dept_e),
      nom_commune = dplyr::coalesce(nom_commune, nom_full),
      region      = unname(dept_to_region[code_dept])
    ) |>
    dplyr::left_join(dept_means,   by = "code_dept") |>
    dplyr::left_join(region_means, by = "region") |>
    dplyr::mutate(
      imputation = dplyr::case_when(
        !is.na(qual_indice)                    ~ "direct",
        !is.na(qual_dept)                      ~ "dept",
        !is.na(qual_reg)                       ~ "region",
        TRUE                                    ~ "na"
      ),
      imputed     = imputation != "direct",
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
    dplyr::select(code_commune, nom_commune,
                  qual_indice, qual_mean, qual_label,
                  n_days, pct_bon, pct_mauvais_plus,
                  no2_indice, o3_indice, pm10_indice, pm25_indice,
                  source_aasqa, imputed, imputation) |>
    dplyr::filter(!is.na(qual_indice))

  monthly <- full
}

arrow::write_parquet(monthly, out_path)

# ---- Rapport ----

cat(glue::glue("

  ✔ {nrow(monthly)} communes → {out_path}
    ({round(file.size(out_path)/1024)} KB)
    Fenêtre : {dates[1]} → {dates[length(dates)]} ({n_days_available} jours observés)

  Imputation :
"))
print(monthly |> dplyr::count(imputation))

cat("\n  Distribution indice ATMO moyen :\n")
print(monthly |> dplyr::count(qual_indice, qual_label))

cat("\n  Sources AASQA actives (sur la période) :\n")
print(monthly |> dplyr::filter(!is.na(source_aasqa)) |>
        dplyr::count(source_aasqa, sort = TRUE))
