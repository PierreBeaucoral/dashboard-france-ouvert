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

# ---- Lookup partagé : code commune → {nom, dept} ----------------
# Sourcé par toutes les pages. Sert de fallback pour les panels cliquables
# quand window.communeLookup (défini uniquement par pages/elections.qmd)
# n'est pas disponible. Couvre COM + ARM (arrondissements PLM) + alias
# consolidés Paris/Lyon/Marseille.
.pop_path_for_names <- here::here("data", "processed", "insee", "populations.parquet")
if (file.exists(.pop_path_for_names)) {
  commune_names_df <- arrow::read_parquet(.pop_path_for_names) |>
    dplyr::filter(type_com %in% c("COM", "ARM")) |>
    dplyr::distinct(code_commune, nom_commune, code_dept) |>
    dplyr::transmute(code = code_commune,
                     nom  = nom_commune,
                     dept = code_dept)
  # Alias consolidés PLM (75056 Paris, 69123 Lyon, 13055 Marseille)
  commune_names_df <- dplyr::bind_rows(
    commune_names_df,
    tibble::tibble(
      code = c("75056", "69123", "13055"),
      nom  = c("Paris", "Lyon",   "Marseille"),
      dept = c("75",    "69",     "13")
    )
  ) |> dplyr::distinct(code, .keep_all = TRUE)
}

#' Émet une balise `<script>` qui peuple `window.communeNames` côté navigateur.
#' À appeler depuis un chunk `results: asis, echo: false` sur chaque page qui
#' affiche un panneau commune.
emit_commune_names_js <- function() {
  if (!exists("commune_names_df")) return(invisible())
  json <- jsonlite::toJSON(commune_names_df, auto_unbox = TRUE, na = "null")
  cat('<script>\n',
      'window.communeNames = {};\n',
      'var _cn_init = ', as.character(json), ';\n',
      '_cn_init.forEach(function(r) { window.communeNames[r.code] = { nom: r.nom, dept: r.dept }; });\n',
      '</script>\n',
      sep = "")
}


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

  # Espérance de vie INSEE → joint à APL pour partager la page Santé
  ev_path <- here::here("data", "processed", "insee_extra", "esperance_vie.parquet")
  if (file.exists(ev_path) && apl_ready) {
    ev_data <- arrow::read_parquet(ev_path) |>
      dplyr::select(code_commune, esperance_vie)
    apl_data <- apl_data |> dplyr::left_join(ev_data, by = "code_commune")
  }

  filo_path_d <- here::here("data", "processed", "filosofi",
                            "revenus_communes.parquet")
  filo_ready  <- file.exists(filo_path_d)
  if (filo_ready) filo_data <- arrow::read_parquet(filo_path_d)

  # ---- Démographie & social (population, densité, chômage) ----
  densite_path <- here::here("data", "processed", "insee_densite",
                             "grille_densite.parquet")
  densite_ready <- file.exists(densite_path)
  if (densite_ready) densite_data <- arrow::read_parquet(densite_path)

  defm_path  <- here::here("data", "processed", "dares",
                           "defm_commune.parquet")
  defm_ready <- file.exists(defm_path)
  if (defm_ready) {
    defm_raw <- arrow::read_parquet(defm_path)
    # Le fichier publie 1 trimestre — on prend le plus récent par commune.
    defm_data <- defm_raw |>
      dplyr::group_by(code_commune) |>
      dplyr::slice_max(trimestre, n = 1L, with_ties = FALSE) |>
      dplyr::ungroup()
  }

  # Population : on a déjà pop_insee chargé plus haut, mais on en re-extrait
  # une copie autonome pour la page Démographie (peut tourner sans elec).
  pop_path_alt <- here::here("data", "processed", "insee", "populations.parquet")
  pop_ready_alt <- file.exists(pop_path_alt)
  if (pop_ready_alt) {
    pop_data <- arrow::read_parquet(pop_path_alt) |>
      dplyr::filter(!is.na(code_commune)) |>
      dplyr::group_by(code_commune) |>
      dplyr::slice_max(pop_latest, n = 1L, with_ties = FALSE,
                       na_rm = TRUE) |>
      dplyr::ungroup() |>
      dplyr::transmute(code_commune, pop_latest, strate_pop)
  }
  demo_ready <- pop_ready_alt && densite_ready  # DEFM optionnel

  # ---- Bloc G : Empreinte carbone (RARE/CITEPA) ----
  carb_path  <- here::here("data", "processed", "energie",
                           "carbone_commune.parquet")
  carb_ready <- file.exists(carb_path)
  if (carb_ready) carb_data <- arrow::read_parquet(carb_path)

  # ---- Bloc I : Mobilité domicile-travail (INSEE MOBPRO 2019) ----
  mob_path  <- here::here("data", "processed", "mobilite",
                          "mobilite_commune.parquet")
  mob_ready_dt <- file.exists(mob_path)
  if (mob_ready_dt) mob_data <- arrow::read_parquet(mob_path)

  # Flux top-200 résidence × destination (script 16, pour Sankey)
  flows_path  <- here::here("data", "processed", "mobilite",
                            "flows_top.parquet")
  flows_ready <- file.exists(flows_path)
  if (flows_ready) flows_data <- arrow::read_parquet(flows_path)

  # Top 5 entrants + top 5 sortants par commune (script 17)
  fpc_path  <- here::here("data", "processed", "mobilite",
                          "flows_per_commune.parquet")
  fpc_ready <- file.exists(fpc_path)
  if (fpc_ready) fpc_data <- arrow::read_parquet(fpc_path)

  # ---- Bloc K : Agriculture biologique (Agence Bio) ----
  bio_path  <- here::here("data", "processed", "agriculture",
                          "bio_commune.parquet")
  bio_ready <- file.exists(bio_path)
  if (bio_ready) bio_data <- arrow::read_parquet(bio_path)


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

  # com_metro_only / com_drom_only : versions sans données élections
  # (utiles pour les pages socio qui ont leurs propres jointures).
  com_metro_only <- com |>
    dplyr::filter(!substr(code, 1, 3) %in% DROM_CODES)
  com_drom_only  <- com |>
    dplyr::filter( substr(code, 1, 3) %in% DROM_CODES)

  # Municipales 2026 (T1 + T2 combinés, ~3 300 communes ≥ 1 000 hab.)
  mun_path <- here::here("data", "processed", "municipales_2026",
                        "resultats.parquet")
  mun_ready <- file.exists(mun_path)
  # Élus 2026 par commune (script 18, couvre 34 836 communes y compris < 1 000 hab.)
  elus_path  <- here::here("data", "processed", "municipales_2026",
                           "elus_commune.parquet")
  elus_ready <- file.exists(elus_path)
  if (elus_ready) elus_data <- arrow::read_parquet(elus_path)
  if (mun_ready) {
    mun_data <- arrow::read_parquet(mun_path)
    com_mun_results <- com |>
      mutate(
        join_code = dplyr::coalesce(
          plm_map$city[match(code, plm_map$arrond)],
          code
        )
      ) |>
      dplyr::left_join(mun_data, by = c("join_code" = "code_insee"))
    com_metro_mun <- com_mun_results |>
      filter(!substr(code, 1, 3) %in% DROM_CODES)
  }

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

  # com_metro_only / com_drom_only désormais définis plus haut (elec_ready).

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
