# ============================================================
# scripts/06_insee_populations.R
#
# Bloc 1 (partiel) — Données INSEE socio-démographiques.
#
# Cette session livre :
#   1. COG 2026 — référentiel des codes communaux 2026, pour corriger
#      les ~950 trous de vintage (communes du geo absents des données
#      MI/DVF/ATMO parce que fusionnées entre 2023 et 2026).
#   2. Populations légales 2017-2021 — parquet d'icem7 (mirror INSEE).
#      Permet la normalisation par 1 000 hab. et une stratification par
#      taille réelle (pas le proxy d'inscrits).
#
# À faire en session suivante (MCP datagouv actif pour discovery) :
#   - FiLoSoFi récent (revenu médian + taux pauvreté par commune)
#   - Grille communale de densité INSEE (4 ou 7 classes)
#   - SSMSI délinquance par commune
#   - BAAC accidents
#   - APL médecins DREES
#   - Enedis consommation électrique
#   - GRDF gaz
#   - ADEME bilan carbone
#   - Etc.
#
# Sortie : data/processed/insee/populations.parquet
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(here)
  library(curl)
  library(readr)
  library(glue)
})

# ---- Sources ----

URL_COG_COMMUNES <- "https://www.insee.fr/fr/statistiques/fichier/8740222/v_commune_2026.csv"
URL_COG_MVTS     <- "https://www.insee.fr/fr/statistiques/fichier/8740222/v_mvt_commune_2026.csv"
URL_POP          <- "https://static.data.gouv.fr/resources/populations-legales-communales-2017-2021/20240122-151058/poplegales2017-2021.parquet"

# ---- Chemins ----

raw_dir <- here::here("data", "raw", "insee")
out_dir <- here::here("data", "processed", "insee")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cog_path  <- file.path(raw_dir, "v_commune_2026.csv")
mvt_path  <- file.path(raw_dir, "v_mvt_commune_2026.csv")
pop_path  <- file.path(raw_dir, "populations_2017_2021.parquet")

# ---- Téléchargement ----

dl_if_missing <- function(url, dest, min_bytes = 1000L) {
  if (file.exists(dest) && file.size(dest) >= min_bytes) {
    message(glue::glue("  Cache : {basename(dest)}"))
    return(invisible(NULL))
  }
  message(glue::glue("  Télécharge : {url}"))
  curl::curl_download(url, dest, quiet = TRUE)
}

message("→ Téléchargement INSEE")
dl_if_missing(URL_COG_COMMUNES, cog_path, min_bytes = 1e5)
dl_if_missing(URL_COG_MVTS,     mvt_path, min_bytes = 1e5)
dl_if_missing(URL_POP,          pop_path, min_bytes = 1e5)

# ---- Lecture COG ----

cog <- readr::read_csv(cog_path, show_col_types = FALSE)
message(glue::glue("COG : {nrow(cog)} entités"))

# Le COG contient COMMUNES (TYPECOM == 'COM'), arrondissements municipaux
# (ARM, codes 75101-75120, 69381-69389, 13201-13216), et communes déléguées
# (COMD). Pour la jointure avec elec/dvf, on garde COMs + ARMs.
cog_main <- cog |>
  dplyr::filter(TYPECOM %in% c("COM", "ARM")) |>
  dplyr::transmute(
    code_commune = COM,
    nom_commune  = LIBELLE,
    type_com     = TYPECOM,
    code_dept    = DEP,
    code_region  = REG
  )
message(glue::glue("  Communes + arrondissements actifs : {nrow(cog_main)}"))

# ---- Lecture mouvements (pour vintage correction) ----

mvts <- readr::read_csv(mvt_path, show_col_types = FALSE)
# Colonnes : MOD (type de mouvement), DATE_EFF, TYPECOM_AV, COM_AV (ancien),
# TYPECOM_AP, COM_AP (nouveau). MOD 32/33/34 = fusions.
fusions <- mvts |>
  dplyr::filter(MOD %in% c("31","32","33","34","41","50","70")) |>
  dplyr::transmute(
    code_avant   = COM_AV,
    code_apres   = COM_AP,
    date_effet   = DATE_EFF,
    type_mvt     = MOD
  ) |>
  dplyr::distinct()
message(glue::glue("  Mouvements de communes (fusions/scissions) : {nrow(fusions)}"))

# ---- Lecture populations ----

pop <- arrow::read_parquet(pop_path)
names(pop) <- tolower(names(pop))
message(glue::glue("Populations : {nrow(pop)} lignes (long format), cols : ",
                   paste(names(pop), collapse=", ")))

# Format LONG : 1 ligne par (commune × annee_rp). Colonnes :
#   codgeo, libgeo, coddep, codreg, pmun, ptot, pcap, annee_rp
# On pivote en wide : pop_2017, pop_2018, ..., pop_latest = pmun max(annee_rp).

populations_wide <- pop |>
  dplyr::filter(!is.na(pmun)) |>
  dplyr::select(codgeo, libgeo, annee_rp, pmun) |>
  tidyr::pivot_wider(
    names_from   = annee_rp,
    values_from  = pmun,
    names_prefix = "pop_"
  )

# Identifier la dernière année disponible
year_cols <- grep("^pop_\\d{4}$", names(populations_wide), value = TRUE)
latest_year <- max(as.integer(sub("pop_", "", year_cols)))
latest_col  <- paste0("pop_", latest_year)
message(glue::glue("  Année RP la plus récente : {latest_year}"))

populations <- populations_wide |>
  dplyr::transmute(
    code_commune = codgeo,
    nom_pop      = libgeo,
    pop_2017     = if ("pop_2017" %in% year_cols) pop_2017 else NA_integer_,
    pop_2018     = if ("pop_2018" %in% year_cols) pop_2018 else NA_integer_,
    pop_2019     = if ("pop_2019" %in% year_cols) pop_2019 else NA_integer_,
    pop_2020     = if ("pop_2020" %in% year_cols) pop_2020 else NA_integer_,
    pop_2021     = if ("pop_2021" %in% year_cols) pop_2021 else NA_integer_,
    pop_latest   = .data[[latest_col]]
  )

# ---- Strates de taille de commune (vraie) ----
# 4 quartiles approximatifs de population (INSEE-like) :
populations <- populations |>
  dplyr::mutate(
    strate_pop = dplyr::case_when(
      pop_latest < 500              ~ "Très petite (< 500 hab.)",
      pop_latest < 2000             ~ "Rurale (500-2 000)",
      pop_latest < 10000            ~ "Bourg / périurbain (2 000-10 000)",
      pop_latest < 50000            ~ "Ville moyenne (10 000-50 000)",
      pop_latest >= 50000           ~ "Grande ville (≥ 50 000)",
      TRUE                          ~ NA_character_
    ),
    strate_pop = factor(strate_pop,
      levels = c("Très petite (< 500 hab.)", "Rurale (500-2 000)",
                 "Bourg / périurbain (2 000-10 000)",
                 "Ville moyenne (10 000-50 000)",
                 "Grande ville (≥ 50 000)"))
  )

# ---- Jointure COG + populations ----

out <- cog_main |>
  dplyr::left_join(populations, by = "code_commune") |>
  dplyr::mutate(
    nom_commune = dplyr::coalesce(nom_commune, nom_pop)
  ) |>
  dplyr::select(-nom_pop)

# ---- Écriture ----

arrow::write_parquet(out, file.path(out_dir, "populations.parquet"))
arrow::write_parquet(fusions, file.path(out_dir, "commune_fusions.parquet"))

# ---- Rapport ----

cat(glue::glue("

  ✔ {nrow(out)} communes (COG 2026 + pop 2017-2021)
    → {file.path(out_dir, 'populations.parquet')}
  ✔ {nrow(fusions)} mouvements de communes
    → {file.path(out_dir, 'commune_fusions.parquet')}

  Strates de population :
"))
print(out |> dplyr::count(strate_pop))

cat("\n  Distribution pop (latest) :\n")
print(summary(out$pop_latest))
