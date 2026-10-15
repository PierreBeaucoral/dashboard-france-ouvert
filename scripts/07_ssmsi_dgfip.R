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
  "Vols de véhicule"                        = "del_vols_vehicules",   # libellé éd. 2025
  "Vols de véhicules"                       = "del_vols_vehicules",   # ancien libellé
  "Vols dans les véhicules"                 = "del_vols_dans_vehicules",
  "Vols d'accessoires sur véhicules"        = "del_vols_accessoires",
  "Destructions et dégradations volontaires" = "del_degradations",
  "Trafic de stupéfiants"                   = "del_trafic_stup",
  "Usage de stupéfiants"                    = "del_usage_stup"
)

# Le millésime du code géo change à chaque édition (CODGEO_2025, _2026...)
codgeo_col <- grep("^CODGEO", names(ssmsi), value = TRUE)[1]

ssmsi_filt <- ssmsi |>
  dplyr::filter(annee == latest_year, est_diffuse == "diff") |>
  dplyr::filter(indicateur %in% names(indicators_short)) |>
  dplyr::mutate(
    # as.character : `indicateur` est un facteur ; indexer par le facteur utiliserait
    # ses codes entiers (mauvais libellés -> colonnes permutées)
    var = unname(indicators_short[as.character(indicateur)]),
    code_commune = stringr::str_pad(.data[[codgeo_col]], 5, "left", "0")
  ) |>
  dplyr::select(code_commune, var, taux_pour_mille, nombre, insee_pop, insee_log)

ssmsi_wide <- ssmsi_filt |>
  dplyr::select(code_commune, var, taux_pour_mille) |>
  tidyr::pivot_wider(
    names_from   = var,
    values_from  = taux_pour_mille,
    values_fn    = mean
  )

# Effectifs bruts (nombre) : base du total et des comptes affichés.
counts_wide <- ssmsi_filt |>
  dplyr::select(code_commune, var, nombre) |>
  tidyr::pivot_wider(names_from = var, values_from = nombre,
                     names_prefix = "n_", values_fn = mean)

# Garde : si une nuance n'a pas de libellé court dans `indicators_short`,
# pivot_wider crée une colonne au nom NA (artefact). On la supprime — elle
# n'est pas utilisée par la page et fausserait le contrôle qualité.
na_named <- is.na(names(ssmsi_wide)) | names(ssmsi_wide) == "NA"
if (any(na_named)) {
  message(glue::glue("  ⚠ {sum(na_named)} colonne(s) au nom NA supprimée(s) après pivot"))
  ssmsi_wide <- ssmsi_wide[, !na_named, drop = FALSE]
}

# Règle du secret statistique (SSMSI) : un décompte n'est diffusé que s'il vaut
# 0 ou >= 5 ; les valeurs 1-4 sont "ndiff" (NA ici, jamais 0). Un 0 "diff" est
# donc un vrai zéro. Conséquence : dans les petites communes, seules celles à
# 0 apparaissent -> biais de sélection (voir docs/decisions.qmd).
# Total = somme des 13 composantes SEULEMENT si toutes sont diffusées, sinon NA
# (l'ancien rowSums(na.rm = TRUE) sommait des sous-ensembles différents).
# UNITÉ (audit C4) : `taux_pour_mille` est rapporté aux HABITANTS (insee_pop) pour
# tous les postes SAUF « Cambriolages de logement », rapporté aux LOGEMENTS
# (insee_log). Les additionner mélangeait deux dénominateurs. Le total est donc
# reconstruit à partir des EFFECTIFS sur un dénominateur unique (population),
# et le taux de cambriolages reste « pour 1 000 logements » (colonne
# del_cambriolages, jamais additionnée). Les postes ont aussi des unités de
# compte différentes (victimes, infractions, mis en cause) : le total est un
# indice agrégé de volumes enregistrés, pas un nombre de faits.
comp_cols <- unique(unname(indicators_short))
comp_cols <- intersect(comp_cols, names(ssmsi_wide))
n_cols <- paste0("n_", comp_cols)
ssmsi_wide <- ssmsi_wide |>
  dplyr::left_join(counts_wide, by = "code_commune")

# Pop et logements référence
pops <- ssmsi_filt |>
  dplyr::distinct(code_commune, insee_pop, insee_log)
ssmsi_out <- ssmsi_wide |>
  dplyr::left_join(pops, by = "code_commune") |>
  dplyr::rename(ssmsi_pop_ref = insee_pop, ssmsi_log_ref = insee_log) |>
  dplyr::mutate(
    del_n_publies = rowSums(!is.na(dplyr::across(dplyr::all_of(comp_cols)))),
    del_n_total = ifelse(del_n_publies == length(comp_cols),
                         rowSums(dplyr::across(dplyr::all_of(n_cols))), NA_real_),
    del_total_pour_mille = ifelse(!is.na(del_n_total) & ssmsi_pop_ref > 0,
                                  del_n_total / ssmsi_pop_ref * 1000, NA_real_),
    annee_ssmsi = latest_year
  ) |>
  dplyr::rename(del_cambriolages_n = n_del_cambriolages) |>
  dplyr::select(-dplyr::starts_with("n_del_"))
stopifnot(all(c("del_cambriolages_n", "ssmsi_log_ref", "del_n_total") %in% names(ssmsi_out)))

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
    # Construire code commune INSEE 5 chars : drop le zéro initial du département (001 -> 01, 02A -> 2A, 091 -> 91) ;
    # les DROM (101-106) sont recodés plus bas
    dep_norm = ifelse(substr(dep, 1, 1) == "0" & nchar(dep) == 3,
                      substr(dep, 2, 3), dep),
    # DROM : la DGFiP code les départements d'outre-mer 101 à 106 (et non 971 à 976) ;
    # icom DGFiP = 3 derniers chiffres du code INSEE (code = 97 + icom) (vérifié sur les noms : Matoury =
    # 102/307 -> 97307, Pamandzi = 106/615 -> 97615). 101 Guadeloupe, 102 Guyane,
    # 103 Martinique, 104 La Réunion, 106 Mayotte.
    dep_norm = dplyr::recode(dep_norm, "101" = "971", "103" = "972", "102" = "973",
                             "104" = "974", "106" = "976"),
    icom_norm = stringr::str_pad(icom, 3, "left", "0"),
    # DROM : code INSEE = "97" + icom (icom DGFiP = chiffre du département + rang à 2 chiffres)
    code_commune = ifelse(substr(dep_norm, 1, 2) == "97", paste0("97", icom_norm),
                          paste0(dep_norm, icom_norm)),
    # Pad to 5 chars
    code_commune = stringr::str_pad(code_commune, 5, "left", "0")
  ) |>
  dplyr::transmute(
    code_commune,
    annee_dgfip = as.integer(an),
    fin_pop_ref       = clean_num(pop1),
    # Colonnes DGFiP préfixées "f" = EUROS PAR HABITANT (vérifié : prod*1000/pop1
    # = fprod pour toutes les communes). Les montants bruts (prod, charge...) sont
    # des TOTAUX EN MILLIERS D'EUROS -> conservés sous *_total_keur.
    fin_recettes      = clean_num(fprod),    # €/hab (produits de fonctionnement)
    fin_charges       = clean_num(fcharge),  # €/hab
    fin_dette         = clean_num(fdette),   # €/hab (encours de dette)
    fin_caf           = clean_num(fcaf),     # €/hab (capacité d'autofinancement)
    fin_invest        = clean_num(fdepinv),  # €/hab (dépenses d'investissement)
    fin_emprunt       = clean_num(femp),     # €/hab (emprunts)
    fin_recettes_total_keur = clean_num(prod),
    fin_charges_total_keur  = clean_num(charge),
    fin_dette_total_keur    = clean_num(dette),
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
stopifnot(median(dgfip_clean$fin_recettes, na.rm = TRUE) < 5000)  # €/hab, pas k€ totaux
