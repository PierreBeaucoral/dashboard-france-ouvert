# ============================================================
# scripts/01_geo_boundaries.R
#
# Étape 1 — Téléchargement des contours administratifs français
#
# Source : geo.api.gouv.fr (API officielle Découpage administratif).
# Produit : GeoJSON simplifié pour départements (métropole + DROM).
#
# Lancement : Rscript scripts/01_geo_boundaries.R
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(curl)
  library(here)
  library(glue)
  library(arrow)
})

# ---- Paramètres ----

DROM_CODES <- c("971", "972", "973", "974", "976")

# Compromis poids/qualité pour la simplification (Visvalingam-Whyatt)
KEEP_DEPARTEMENTS <- 0.20   # ~101 polygones, simplification douce
KEEP_COMMUNES     <- 0.04   # ~35 000 polygones, simplification agressive

# ---- Chemins ----

raw_dir <- here::here("data", "raw", "geo")
out_dir <- here::here("data", "processed", "geo")

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Téléchargement ----

# Source : dépôt gregoiredavid/france-geojson (référence largement utilisée
# pour les contours administratifs français en GeoJSON, dérivée de l'IGN /
# OpenStreetMap).
#
# Particularité : le fichier `departements.geojson` du dépôt ne contient que
# la France métropolitaine. Les DROM sont fournis individuellement, en
# sous-dossiers, et doivent être téléchargés séparément puis fusionnés.

base_url <- "https://france-geojson.gregoiredavid.fr/repo"

# Métropole : un seul fichier, 96 départements
url_metropole <- file.path(base_url, "departements.geojson")
metropole_path <- file.path(raw_dir, "departements_metropole.geojson")

# DROM : un fichier par département, à fusionner
drom_files <- list(
  "971" = list(slug = "971-guadeloupe", name = "guadeloupe"),
  "972" = list(slug = "972-martinique", name = "martinique"),
  "973" = list(slug = "973-guyane",     name = "guyane"),
  "974" = list(slug = "974-la-reunion", name = "la-reunion"),
  "976" = list(slug = "976-mayotte",    name = "mayotte")
)

download_if_missing <- function(url, dest, min_bytes = 1000L) {
  if (file.exists(dest) && file.size(dest) >= min_bytes) {
    message(glue::glue("  Cache : {basename(dest)}"))
    return(invisible(NULL))
  }
  message(glue::glue("  Télécharge : {url}"))
  curl::curl_download(url, dest, quiet = TRUE)
}

# 1) Métropole
message("→ Téléchargement métropole")
download_if_missing(url_metropole, metropole_path, min_bytes = 100000L)

# 2) Chaque DROM
message("→ Téléchargement DROM")
drom_paths <- character()
for (code in names(drom_files)) {
  slug <- drom_files[[code]]$slug
  name <- drom_files[[code]]$name
  url  <- file.path(base_url, "departements", slug,
                    sprintf("departement-%s-%s.geojson", code, name))
  dest <- file.path(raw_dir, sprintf("departement_%s.geojson", code))
  download_if_missing(url, dest)
  drom_paths[code] <- dest
}

# ---- Lecture & fusion ----

metropole <- sf::read_sf(metropole_path)
message(glue::glue("  Métropole : {nrow(metropole)} entités"))

# Lire chaque DROM (1 polygone par fichier)
drom_list <- lapply(names(drom_paths), function(code) {
  one <- sf::read_sf(drom_paths[[code]])
  # Les fichiers DROM peuvent avoir des schémas légèrement différents :
  # on harmonise sur (nom, code).
  if (!"code" %in% names(one)) one$code <- code
  if (!"nom"  %in% names(one)) one$nom  <- drom_files[[code]]$name
  one |> dplyr::select(any_of(c("nom", "code")))
})

drom_sf <- do.call(rbind, drom_list)
message(glue::glue("  DROM     : {nrow(drom_sf)} entités"))

# Aligner les colonnes avant rbind
metropole <- metropole |> dplyr::select(any_of(c("nom", "code")))
drom_sf   <- drom_sf   |> dplyr::select(any_of(c("nom", "code")))

dep_raw <- rbind(metropole, drom_sf)

# Si pas en WGS84, on reprojette
if (sf::st_crs(dep_raw)$epsg != 4326L) {
  dep_raw <- sf::st_transform(dep_raw, 4326L)
}

# ---- Annotation DROM ----

dep_raw <- dep_raw |>
  mutate(
    is_drom = code %in% DROM_CODES,
    nom     = as.character(nom),
    code    = as.character(code)
  )

message(glue::glue("Départements totaux : {nrow(dep_raw)} (dont {sum(dep_raw$is_drom)} DROM)"))

# ---- Simplification ----

if (!requireNamespace("rmapshaper", quietly = TRUE)) {
  stop("Package 'rmapshaper' requis. Installer via :\n",
       "  install.packages('rmapshaper')")
}

message(glue::glue("Simplification (keep = {KEEP_DEPARTEMENTS})…"))
dep_simpl <- rmapshaper::ms_simplify(
  dep_raw,
  keep        = KEEP_DEPARTEMENTS,
  keep_shapes = TRUE,
  method      = "vis"  # Visvalingam-Whyatt
)

# ---- Écriture ----
#
# COORDINATE_PRECISION=5 → 5 décimales ≈ 1 m de précision au sol, largement
# suffisant pour une choroplèthe nationale. Sans cette option, GDAL écrit
# 13 décimales (précision 0,1 micron, absurde) qui gonflent inutilement le
# poids embarqué dans chaque page leaflet. Gain ~30-45 % par fichier.

out_path <- file.path(out_dir, "departements.geojson")
sf::write_sf(dep_simpl, out_path, delete_dsn = TRUE, quiet = TRUE,
             layer_options = "COORDINATE_PRECISION=5")

# Sous-extraction DROM seuls (utile pour les cartouches)
drom_path <- file.path(out_dir, "drom_departements.geojson")
sf::write_sf(
  dep_simpl |> filter(is_drom),
  drom_path,
  delete_dsn = TRUE,
  quiet = TRUE,
  layer_options = "COORDINATE_PRECISION=5"
)

# ---- Rapport ----

size_kb <- function(p) round(file.size(p) / 1024)

cat(glue::glue("

  ✔ Départements : {nrow(dep_simpl)} entités → {out_path} ({size_kb(out_path)} KB)
  ✔ DROM seuls   : {sum(dep_simpl$is_drom)} entités → {drom_path} ({size_kb(drom_path)} KB)
  ℹ Reprojection : EPSG:4326 (WGS84)
  ℹ Simplification : Visvalingam-Whyatt, keep = {KEEP_DEPARTEMENTS}

"))

# ============================================================
# Partie 2 — Contours communaux (pour le choroplèthe Étape 2)
#
# Source (audit N6) : Etalab « contours-administratifs », édition 2026 (Admin Express COG
# Carto de l'IGN, géographie COG 2026 = celle du cadre de communes, simplifiée à 1 000 m) :
#   https://etalab-datasets.geo.data.gouv.fr/contours-administratifs/2026/geojson/communes-1000m.geojson
# Avant : gregoiredavid/france-geojson, géographie antérieure (1 015 polygones à d'anciens
# codes, 5 communes COG 2026 sans polygone : Orée d'Anjou, Conques-en-Rouergue, Saint-Lucien,
# L'Oie, Sainte-Florence). Le polygone d'une commune absorbée n'était alors jamais celui de la
# commune qui porte les données -> trous gris sur toutes les cartes.
# Contrat conservé : colonnes (nom, code, is_drom) ; Paris/Lyon/Marseille représentées par leurs
# 45 arrondissements (le polygone « ville » est écarté : il recouvrirait les arrondissements).
# Prérequis : scripts/06_insee_populations.R (cadre COG 2026 dans populations.parquet).
# Lors d'une nouvelle édition du COG : changer l'année dans l'URL ci-dessous.
# ============================================================

COM_GEO_YEAR <- 2026L
url_com <- sprintf("https://etalab-datasets.geo.data.gouv.fr/contours-administratifs/%d/geojson/communes-1000m.geojson",
                   COM_GEO_YEAR)
com_path <- file.path(raw_dir, sprintf("communes_etalab_%d.geojson", COM_GEO_YEAR))
message("\n→ Téléchargement contours communaux Etalab ", COM_GEO_YEAR, " (~10 MB)")
download_if_missing(url_com, com_path, min_bytes = 5e6)

com_raw <- sf::read_sf(com_path)
if (sf::st_crs(com_raw)$epsg != 4326L) com_raw <- sf::st_transform(com_raw, 4326L)

# Cadre attendu = COM + ARM du COG 2026 moins les 3 villes PLM (représentées par arrondissements)
frame <- arrow::read_parquet(here::here("data", "processed", "insee", "populations.parquet"))
plm_city <- c("75056", "69123", "13055")
frame_codes <- setdiff(frame$code_commune, plm_city)

com_out_sf <- com_raw |>
  dplyr::filter(code %in% frame_codes) |>
  dplyr::transmute(nom = as.character(nom), code = as.character(code),
                   is_drom = substr(code, 1, 3) %in% DROM_CODES)
manquants <- setdiff(frame_codes, com_out_sf$code)
stopifnot(length(manquants) == 0L, !anyDuplicated(com_out_sf$code))   # chaque commune COG 2026 a un polygone
message(glue::glue("Communes : {nrow(com_out_sf)} polygones (dont {sum(com_out_sf$is_drom)} DROM), ",
                   "{nrow(com_raw) - nrow(com_out_sf)} entités hors cadre écartées (villes PLM, COM 975-989)"))

# Écriture — précision 5 décimales (cf. note plus haut)
com_out <- file.path(out_dir, "communes.geojson")
sf::write_sf(com_out_sf, com_out, delete_dsn = TRUE, quiet = TRUE,
             layer_options = "COORDINATE_PRECISION=5")

cat(glue::glue("

  ✔ Communes : {nrow(com_out_sf)} entités → {com_out} ({size_kb(com_out)} KB)
    (dont {sum(com_out_sf$is_drom)} DROM)

"))
