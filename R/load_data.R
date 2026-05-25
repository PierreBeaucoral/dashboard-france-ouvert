# ============================================================
# R/load_data.R
# Fonctions de chargement des artefacts pré-calculés
# (à brancher progressivement Étapes 1 → 6)
# ============================================================

# Chemins canoniques vers les artefacts produits par scripts/
paths <- list(
  geo_dep        = "data/processed/geo/departements.geojson",
  geo_com        = "data/processed/geo/communes.geojson",
  geo_drom       = "data/processed/geo/drom_insets.geojson",
  elections      = "data/processed/elections/legislatives_2024_t2.parquet",
  dvf            = "data/processed/dvf/dvf_aggregated.parquet",
  air_stations   = "data/processed/air/stations.parquet",
  air_to_commune = "data/processed/air/commune_station_link.parquet",
  correlations   = "data/processed/explorer/correlation_matrices.parquet",
  bivariate      = "data/processed/explorer/bivariate_bins.parquet"
)

# ---- Stubs : à implémenter dès que les artefacts existent ----

load_geo_departements <- function() {
  if (!file.exists(paths$geo_dep)) {
    stop("Artefact manquant : ", paths$geo_dep,
         "\nLancer scripts/01_geo_boundaries.R d'abord.")
  }
  sf::read_sf(paths$geo_dep)
}

load_elections <- function() {
  if (!file.exists(paths$elections)) {
    stop("Artefact manquant : ", paths$elections,
         "\nLancer scripts/02_elections.R d'abord.")
  }
  arrow::read_parquet(paths$elections)
}

# load_dvf(year)            — Étape 3
# load_air_stations()       — Étape 4
# load_commune_air_link()   — Étape 4
# load_correlation_matrix() — Étape 6
# load_bivariate_bins()     — Étape 5
