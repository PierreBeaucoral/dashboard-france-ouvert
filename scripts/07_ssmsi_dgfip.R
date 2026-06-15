# ============================================================
# scripts/07_ssmsi_dgfip.R
#
# Bloc 4 (SSMSI délinquance) + Bloc 8 (DGFiP comptes communaux)
#
# Sources data.gouv.fr :
#   - SSMSI : bases-statistiques-communale-departementale-et-regionale-
#             de-la-delinquance-enregistree-par-la-police-et-la-gendarmerie-
#             nationales (parquet 15 MB, format long 5.2M lignes)
#   - DGFiP : comptes-individuels-des-communes-fichier-global-2023-2024
#             (CSV 62 MB, ~200 colonnes financières)
#
# Sortie :
#   - data/processed/ssmsi/delinquance_commune.parquet (1 ligne/commune)
#   - data/processed/dgfip/comptes_communes.parquet (1 ligne/commune)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(arrow)
  library(readr)
  library(here)
  library(glue)
  library(stringr)
})

# ---- Chemins ----

ssmsi_raw <- here::here("data", "raw", "ssmsi", "base_communale.parquet")
dgfip_raw <- here::here("data", "raw", "dgfip", "comptes_communes_2023_2024.csv")
out_dir   <- here::here("data", "processed")
dir.create(file.path(out_dir, "ssmsi"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "dgfip"), recursive = TRUE, showWarnings = FALSE)

# ============================================================
# SSMSI — Délinquance
# ============================================================

message("→ Lecture SSMSI...")
ssmsi <- arrow::read_parquet(ssmsi_raw)
message(glue::glue("  {nrow(ssmsi)} lignes, années : ",
                   paste(sort(unique(ssmsi$annee)), collapse=", ")))

latest_year <- max(ssmsi$annee, na.rm = TRUE)
message(glue::glue("  Année retenue : {latest_year}"))

# Indicateurs principaux (les "diff" sont publiés ; "ndiff" sous secret stat)
indicators_short <- c(
  "Violences physiques intrafamiliales"     = "del_violences_intra",
  "Violences physiques hors cadre familial" = "del_violences_extra",
  "Violences sexuelles"                     = "del_violences_sex",
  "Vols sans violence contre des personnes" = "del_vols_pers",
  "Vols avec armes"                         = "del_vols_armes",
  "Vols violents sans arme"                 = "del_vols_violents",
  "Cambriolages de logement"                = "del_cambriolages",
  "Vols de véhicules"                       = "del_vols_vehicules",
  "Vols dans les véhicules"                 = "del_vols_dans_vehicules",
  "Vols d'accessoires sur véhicules"        = "del_vols_accessoires",
  "Destructions et dégradations volontaires" = "del_degradations",
  "Trafic de stupéfiants"                   = "del_trafic_stup",
  "Usage de stupéfiants"                    = "del_usage_stup"
)

ssmsi_filt <- ssmsi |>
  dplyr::filter(annee == latest_year, est_diffuse == "diff") |>
  dplyr::filter(indicateur %in% names(indicators_short)) |>
  dplyr::mutate(
    var = unname(indicators_short[indicateur]),
    code_commune = stringr::str_pad(CODGEO_2025, 5, "left", "0")
  ) |>
  dplyr::select(code_commune, var, taux_pour_mille, nombre, insee_pop)

ssmsi_wide <- ssmsi_filt |>
  dplyr::select(code_commune, var, taux_pour_mille) |>
  tidyr::pivot_wider(
    names_from   = var,
    values_from  = taux_pour_mille,
    values_fn    = mean
  )

# Garde : si une nuance n'a pas de libellé court dans `indicators_short`,
# pivot_wider crée une colonne au nom NA (artefact). On la supprime — elle
# n'est pas utilisée par la page et fausserait le contrôle qualité.
na_named <- is.na(names(ssmsi_wide)) | names(ssmsi_wide) == "NA"
if (any(na_named)) {
  message(glue::glue("  ⚠ {sum(na_named)} colonne(s) au nom NA supprimée(s) après pivot"))
  ssmsi_wide <- ssmsi_wide[, !na_named, drop = FALSE]
}

# Taux global agrégé : somme des taux (vols + violences + dégradations + drogue)
ssmsi_wide <- ssmsi_wide |>
  dplyr::mutate(
    del_total_pour_mille = rowSums(
      dplyr::across(dplyr::starts_with("del_")),
      na.rm = TRUE
    )
  )

# Pop référence
pops <- ssmsi_filt |>
  dplyr::distinct(code_commune, insee_pop)
ssmsi_out <- ssmsi_wide |>
  dplyr::left_join(pops, by = "code_commune") |>
  dplyr::rename(ssmsi_pop_ref = insee_pop) |>
  dplyr::mutate(annee_ssmsi = latest_year)

arrow::write_parquet(ssmsi_out, file.path(out_dir, "ssmsi", "delinquance_commune.parquet"))
message(glue::glue("  ✔ SSMSI : {nrow(ssmsi_out)} communes → ssmsi/delinquance_commune.parquet"))

# ============================================================
# DGFiP — Comptes communaux
# ============================================================

message("\n→ Lecture DGFiP...")
# CSV séparateur ; — décimales "." mais certains champs sont vides
dgfip <- readr::read_delim(
  dgfip_raw,
  delim = ";",
  locale = readr::locale(decimal_mark = ".", encoding = "UTF-8"),
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
)
message(glue::glue("  {nrow(dgfip)} lignes, {ncol(dgfip)} colonnes"))

# Construire code INSEE commune : dep (3 chars) + icom (3 chars, padded)
# dep = "001", "075", "971" ; icom = "001" à "999"
clean_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

dgfip_clean <- dgfip |>
  dplyr::filter(an == max(an, na.rm = TRUE)) |>
  dplyr::mutate(
    # Construire code commune INSEE 5 chars : drop leading zero du dept si <96,
    # garder 3 chars pour DROM
    dep_norm = ifelse(substr(dep, 1, 1) == "0" & nchar(dep) == 3 & !substr(dep, 2, 2) == "9",
                      substr(dep, 2, 3), dep),
    icom_norm = stringr::str_pad(icom, 3, "left", "0"),
    code_commune = paste0(dep_norm, icom_norm),
    # Pad to 5 chars
    code_commune = stringr::str_pad(code_commune, 5, "left", "0")
  ) |>
  dplyr::transmute(
    code_commune,
    annee_dgfip = as.integer(an),
    fin_pop_ref       = clean_num(pop1),
    fin_recettes      = clean_num(prod),     # € par hab.
    fin_charges       = clean_num(charge),
    fin_dette         = clean_num(dette),
    fin_caf           = clean_num(caf),       # capacité autofinancement
    fin_invest        = clean_num(depinv),    # dépenses investissement
    fin_emprunt       = clean_num(emp),
    # Taux des taxes locales (en %)
    fin_taux_taxe_hab = clean_num(tth),
    fin_taux_fonc_bati  = clean_num(tfb),
    fin_taux_fonc_nbati = clean_num(tfnb),
    # Bases imposables
    fin_base_th         = clean_num(bth),
    fin_base_fb         = clean_num(bfb)
  )

arrow::write_parquet(dgfip_clean, file.path(out_dir, "dgfip", "comptes_communes.parquet"))
message(glue::glue("  ✔ DGFiP : {nrow(dgfip_clean)} communes → dgfip/comptes_communes.parquet"))

# ---- Rapport ----

cat(glue::glue("

  RAPPORT
  =======
  SSMSI {latest_year} :
    {nrow(ssmsi_out)} communes diffusables
    Top corrélation taux/mille : à vérifier dans Explorer

  DGFiP {max(dgfip$an, na.rm=TRUE)} :
    {nrow(dgfip_clean)} communes
"))
print(dgfip_clean |> dplyr::select(fin_recettes, fin_charges, fin_dette, fin_taux_taxe_hab) |> summary())
