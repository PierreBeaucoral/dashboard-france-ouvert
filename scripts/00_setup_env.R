# ============================================================
# scripts/00_setup_env.R
# Initialisation de l'environnement R via renv
# Lancement : Rscript scripts/00_setup_env.R
# ============================================================

# Packages requis pour l'ensemble du projet
required_pkgs <- c(
  # Cœur tidyverse / data
  "dplyr", "tidyr", "readr", "purrr", "stringr", "tibble", "lubridate",
  # Tables performantes
  "data.table", "arrow",
  # Base de données embarquée pour DVF
  "duckdb", "DBI",
  # Spatial
  "sf", "rmapshaper",
  # Carto et widgets
  "leaflet", "htmltools", "htmlwidgets",
  # Charts
  "ggplot2", "plotly", "ggiraph",
  # Tables interactives
  "reactable", "DT",
  # Interactivité statique
  "crosstalk",
  # Quarto helpers
  "quarto",
  # Téléchargements et HTTP
  "curl", "httr2",
  # Misc
  "fs", "glue", "janitor", "scales"
)

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}

# Initialise renv si pas déjà fait
if (!file.exists("renv.lock")) {
  message("Initialisation de renv (première fois)…")
  renv::init(bare = TRUE)
}

message("Installation des packages…")
renv::install(required_pkgs)

message("Snapshot renv.lock…")
renv::snapshot(prompt = FALSE)

message("✔ Environnement prêt. Vérifier renv.lock.")
