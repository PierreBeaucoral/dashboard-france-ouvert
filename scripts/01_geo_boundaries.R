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
})

# ---- Paramètres ----

DROM_CODES <- c("971", "972", "973", "974", "976")

# Compromis poids/qualité pour la simplification (Visvalingam-Whyatt)
KEEP_DEPARTEMENTS <- 0.20   # ~96 polygones, simplification douce

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

out_path <- file.path(out_dir, "departements.geojson")
sf::write_sf(dep_simpl, out_path, delete_dsn = TRUE, quiet = TRUE)

# Sous-extraction DROM seuls (utile pour les cartouches)
drom_path <- file.path(out_dir, "drom_departements.geojson")
sf::write_sf(
  dep_simpl |> filter(is_drom),
  drom_path,
  delete_dsn = TRUE,
  quiet = TRUE
)

# ---- Rapport ----

size_kb <- function(p) round(file.size(p) / 1024)

cat(glue::glue("

  ✔ Départements : {nrow(dep_simpl)} entités → {out_path} ({size_kb(out_path)} KB)
  ✔ DROM seuls   : {sum(dep_simpl$is_drom)} entités → {drom_path} ({size_kb(drom_path)} KB)
  ℹ Reprojection : EPSG:4326 (WGS84)
  ℹ Simplification : Visvalingam-Whyatt, keep = {KEEP_DEPARTEMENTS}

"))

# ---- Note : communes ----
# Les contours communaux (~35 000 polygones) seront téléchargés en Étape 2,
# juste avant qu'on en ait besoin pour le choroplèthe Législatives.
# Source pressentie : geo.api.gouv.fr/communes (avec fields=contour) ou
# IGN ADMIN-EXPRESS (Shapefile, plus lourd). À trancher après inspection.
