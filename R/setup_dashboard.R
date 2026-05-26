# ============================================================
# R/setup_dashboard.R
# Setup partagé entre toutes les pages du dashboard multi-page.
# Sourcé en début de chaque pages/*.qmd.
#
# Charge :
#   - Librairies + helpers
#   - Contours géographiques (départements, communes, DROM)
#   - Élections + lookup commune
#   - DVF (data + breaks)
#   - Air (atmo)
#   - Sécurité (ssmsi + baac fusionnés)
#   - Finances (dgfip)
#   - Santé (apl 5 pros)
#   - Revenus (filosofi)
#   - Explorer (merged + bivariate + correlations)
#
# Chaque page n'utilise que les objets qui la concernent ; htmlwidgets
# n'embarque dans le HTML que ce qui est passé aux widgets de la page.
# ============================================================

suppressPackageStartupMessages({
  library(leaflet)
  library(sf)
  library(dplyr)
  library(arrow)
  library(htmltools)
  library(htmlwidgets)
  library(jsonlite)
  library(here)
})

source(here::here("R", "theme.R"))
source(here::here("R", "map_helpers.R"))
source(here::here("R", "dvf_helpers.R"))
source(here::here("R", "air_helpers.R"))
source(here::here("R", "socio_helpers.R"))

# ---- Contours (Bloc géo) ----
geo_dep_path <- here::here("data", "processed", "geo", "departements.geojson")
geo_com_path <- here::here("data", "processed", "geo", "communes.geojson")
elec_path    <- here::here("data", "processed", "elections",
                           "legislatives_2024_t2.parquet")

DROM_CODES <- c("971", "972", "973", "974", "976")

data_ready <- file.exists(geo_dep_path)
if (data_ready) {
  dep <- sf::read_sf(geo_dep_path, quiet = TRUE) |>
    mutate(is_drom = code %in% DROM_CODES)
  dep_metro <- dep |> filter(!is_drom)
  dep_drom  <- dep |> filter( is_drom)
}

# ---- Élections + join PLM ----
elec_ready <- file.exists(geo_com_path) && file.exists(elec_path)
if (elec_ready) {
  com  <- sf::read_sf(geo_com_path, quiet = TRUE)
  elec <- arrow::read_parquet(elec_path)

  pop_path <- here::here("data", "processed", "insee", "populations.parquet")
  if (file.exists(pop_path)) {
    pop_insee <- arrow::read_parquet(pop_path) |>
      dplyr::transmute(
        code_insee = code_commune,
        pop_latest,
        strate_pop = as.character(strate_pop)
      )
    elec <- elec |> dplyr::left_join(pop_insee, by = "code_insee")
  }

  ssmsi_path  <- here::here("data", "processed", "ssmsi",
                            "delinquance_commune.parquet")
  ssmsi_ready <- file.exists(ssmsi_path)
  if (ssmsi_ready) ssmsi_data <- arrow::read_parquet(ssmsi_path)

  baac_path_d <- here::here("data", "processed", "baac",
                            "accidents_commune.parquet")
  if (file.exists(baac_path_d) && ssmsi_ready) {
    baac_d <- arrow::read_parquet(baac_path_d)
    ssmsi_data <- ssmsi_data |>
      dplyr::left_join(baac_d, by = "code_commune") |>
      dplyr::mutate(
        acc_pour_mille = ifelse(is.na(ssmsi_pop_ref) | ssmsi_pop_ref == 0,
                                NA_real_,
                                n_accidents_2024 / ssmsi_pop_ref * 1000)
      )
  }

  dgfip_path  <- here::here("data", "processed", "dgfip",
                            "comptes_communes.parquet")
  dgfip_ready <- file.exists(dgfip_path)
  if (dgfip_ready) dgfip_data <- arrow::read_parquet(dgfip_path)

  apl_path  <- here::here("data", "processed", "drees", "apl_medecins.parquet")
  apl_ready <- file.exists(apl_path)
  if (apl_ready) apl_data <- arrow::read_parquet(apl_path)

  filo_path_d <- here::here("data", "processed", "filosofi",
                            "revenus_communes.parquet")
  filo_ready  <- file.exists(filo_path_d)
  if (filo_ready) filo_data <- arrow::read_parquet(filo_path_d)

  plm_map <- tibble::tibble(
    arrond = c(
      sprintf("751%02d", 1:20),
      sprintf("693%02d", 81:89),
      sprintf("132%02d", 1:16)
    ),
    city = c(rep("75056", 20), rep("69123",  9), rep("13055", 16))
  )

  com_results <- com |>
    mutate(
      join_code = dplyr::coalesce(
        plm_map$city[match(code, plm_map$arrond)],
        code
      )
    ) |>
    left_join(elec, by = c("join_code" = "code_insee"))

  com_metro_results <- com_results |>
    filter(!substr(code, 1, 3) %in% DROM_CODES)
  com_drom_results  <- com_results |>
    filter( substr(code, 1, 3) %in% DROM_CODES)

  commune_lookup_df <- com_metro_results |>
    sf::st_drop_geometry() |>
    dplyr::transmute(
      code,
      nom_commune       = dplyr::coalesce(nom_commune, nom),
      libelle_dept,
      code_dept,
      tour,
      famille_vainqueur,
      nuance_vainqueur,
      pct_vainqueur     = round(pct_vainqueur, 2),
      marge_vainqueur   = round(marge_vainqueur, 2),
      pct_abstention    = round(pct_abstention, 2),
      pct_NFP           = round(pct_NFP, 2),
      pct_ENS           = round(pct_ENS, 2),
      pct_RN            = round(pct_RN, 2),
      pct_LR            = round(pct_LR, 2),
      pct_Divers        = round(pct_Divers, 2),
      pct_NFP_t1        = round(pct_NFP_t1, 2),
      pct_ENS_t1        = round(pct_ENS_t1, 2),
      pct_RN_t1         = round(pct_RN_t1, 2),
      pct_LR_t1         = round(pct_LR_t1, 2),
      pct_Divers_t1     = round(pct_Divers_t1, 2),
      pct_abstention_t1 = round(pct_abstention_t1, 2),
      voix_vainqueur,
      voix_second,
      inscrits,
      exprimes,
      abstentions,
      pop_latest = if ("pop_latest" %in% names(com_metro_results)) pop_latest else NA_integer_,
      strate_pop = if ("strate_pop" %in% names(com_metro_results)) strate_pop else NA_character_
    ) |>
    dplyr::filter(!is.na(famille_vainqueur))
}

# ---- DVF ----
dvf_path  <- here::here("data", "processed", "dvf", "dvf_aggregated.parquet")
dvf_ready <- file.exists(dvf_path) && elec_ready
if (dvf_ready) {
  dvf       <- arrow::read_parquet(dvf_path)
  dvf_breaks <- compute_dvf_breaks(dvf)

  YEAR_DEFAULT <- max(dvf$annee)
  TYPE_DEFAULT <- "Appartement"

  dvf_default_year <- dvf |>
    dplyr::filter(annee == YEAR_DEFAULT, type_local == TYPE_DEFAULT,
                  !estimation_insuffisante) |>
    dplyr::select(code_commune, prix_m2_median, n_transactions)

  com_metro_only <- com |>
    dplyr::filter(!substr(code, 1, 3) %in% DROM_CODES)

  dvf_nested <- dvf |>
    dplyr::filter(!estimation_insuffisante) |>
    dplyr::transmute(
      code = code_commune,
      type = type_local,
      annee,
      pm = round(prix_m2_median),
      n  = n_transactions
    ) |>
    dplyr::arrange(code, type, annee)
}

# ---- Air ----
atmo_path  <- here::here("data", "processed", "air", "atmo_snapshot.parquet")
atmo_ready <- file.exists(atmo_path) && elec_ready
if (atmo_ready) {
  atmo       <- arrow::read_parquet(atmo_path)
  atmo_date  <- unique(atmo$date_ech)[1]
}

# ---- Explorer ----
explorer_dir <- here::here("data", "processed", "explorer")
merged_path  <- file.path(explorer_dir, "commune_merged.parquet")
corr_path    <- file.path(explorer_dir, "correlations.parquet")
biv_path     <- file.path(explorer_dir, "bivariate_bins.parquet")
explorer_ready <- file.exists(merged_path) && file.exists(corr_path) &&
                  file.exists(biv_path)
if (explorer_ready) {
  merged       <- arrow::read_parquet(merged_path)
  correlations <- arrow::read_parquet(corr_path)
  bivariate    <- arrow::read_parquet(biv_path)
}
