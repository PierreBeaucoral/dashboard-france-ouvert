# ============================================================
# scripts/05_merge_and_explore.R
#
# Étapes 5 + 6 — Préparation des artefacts pour le mode bivarié et
# l'onglet Explorer.
#
# Produits :
#   data/processed/explorer/commune_merged.parquet
#     1 ligne / commune, toutes les variables croisées
#   data/processed/explorer/correlations.parquet
#     paires de variables × (méthode, strate) → coefficient
#   data/processed/explorer/bivariate_bins.parquet
#     pour chaque commune, terciles des paires sélectionnées
#
# Stratification : quartiles d'inscrits (proxy de la taille de commune,
# faute de pouvoir télécharger la grille de densité INSEE ici).
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(arrow)
  library(here)
  library(glue)
})

# ---- Chemins ----
elec_path <- here::here("data", "processed", "elections",
                        "legislatives_2024_t2.parquet")
dvf_path  <- here::here("data", "processed", "dvf", "dvf_aggregated.parquet")
air_path  <- here::here("data", "processed", "air", "atmo_snapshot.parquet")
pop_path  <- here::here("data", "processed", "insee", "populations.parquet")
ssmsi_path <- here::here("data", "processed", "ssmsi", "delinquance_commune.parquet")
dgfip_path <- here::here("data", "processed", "dgfip", "comptes_communes.parquet")
filo_path  <- here::here("data", "processed", "filosofi", "revenus_communes.parquet")
dens_path  <- here::here("data", "processed", "insee_densite", "grille_densite.parquet")
defm_path  <- here::here("data", "processed", "dares", "defm_commune.parquet")
apl_path   <- here::here("data", "processed", "drees", "apl_medecins.parquet")
baac_path  <- here::here("data", "processed", "baac", "accidents_commune.parquet")
carb_path  <- here::here("data", "processed", "energie", "carbone_commune.parquet")
mob_path   <- here::here("data", "processed", "mobilite", "mobilite_commune.parquet")
bio_path   <- here::here("data", "processed", "agriculture", "bio_commune.parquet")
# ---- Nouvelles sources (vague 2) ----
newsrc <- function(...) here::here("data", "processed", ...)
part_path  <- newsrc("elections", "participation_commune.parquet")
rp22_path  <- newsrc("insee_extra", "rp2022_logement_emploi.parquet")
cat_path   <- newsrc("risques", "catnat_commune.parquet")
bpe_path   <- newsrc("insee_extra", "bpe_equipements.parquet")
caf_path   <- newsrc("insee_extra", "caf_allocataires.parquet")
arc_path   <- newsrc("insee_extra", "arcep_fibre.parquet")
enaf_path  <- newsrc("risques", "enaf_commune.parquet")
dpe_path   <- newsrc("insee_extra", "dpe_commune.parquet")
ore_path   <- newsrc("energie_ore", "energie_ore_commune.parquet")
filo23_path <- newsrc("filosofi_2023", "filosofi_2023_communes.parquet")
out_dir   <- here::here("data", "processed", "explorer")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Chargement ----
elec <- arrow::read_parquet(elec_path)
dvf  <- arrow::read_parquet(dvf_path)
air  <- arrow::read_parquet(air_path)
pop  <- if (file.exists(pop_path)) arrow::read_parquet(pop_path) else NULL
ssmsi <- if (file.exists(ssmsi_path)) arrow::read_parquet(ssmsi_path) else NULL
dgfip <- if (file.exists(dgfip_path)) arrow::read_parquet(dgfip_path) else NULL
filo  <- if (file.exists(filo_path))  arrow::read_parquet(filo_path)  else NULL
dens  <- if (file.exists(dens_path))  arrow::read_parquet(dens_path)  else NULL
defm  <- if (file.exists(defm_path))  arrow::read_parquet(defm_path)  else NULL
apl   <- if (file.exists(apl_path))   arrow::read_parquet(apl_path)   else NULL
baac  <- if (file.exists(baac_path))  arrow::read_parquet(baac_path)  else NULL
carb  <- if (file.exists(carb_path))  arrow::read_parquet(carb_path)  else NULL
mob   <- if (file.exists(mob_path))   arrow::read_parquet(mob_path)   else NULL
bio   <- if (file.exists(bio_path))   arrow::read_parquet(bio_path)   else NULL

# ---- Codes renommés du COG (audit N6) : UN SEUL endroit pour toutes les sources ----
# Un changement de code SANS changement de territoire (MOD 41 changement de département,
# MOD 50 changement de code : Orée d'Anjou 49069 -> 49126, Conques-en-Rouergue 12076 -> 12218,
# Aurseulles 14011 -> 14581, Porte des Pierres Dorées 69159 -> 69114...) est sans perte pour
# des niveaux (prix, taux, indices) : on renomme la ligne vers le code COG 2026. Les FUSIONS
# (plusieurs codes -> un) ne sont pas traitées ici : un niveau ne se somme pas ; les effectifs
# sont déjà sommés dans les scripts amont (cog_helpers.R, 06, 11, 19-26).
# Si le code COG 2026 a déjà sa propre ligne dans la source, c'est elle qui est conservée.
source(here::here("scripts", "cog_helpers.R"))
ren_map <- cog_map("2015-01-01",
                   fus = arrow::read_parquet(here::here("data", "processed", "insee", "commune_fusions.parquet")) |>
                     dplyr::filter(as.character(type_mvt) %in% c("41", "50")))
cog_rename <- function(df) {
  if (is.null(df)) return(df)
  new <- to_cog2026(df$code_commune, ren_map)
  moved <- new != df$code_commune
  if (!any(moved)) return(df)
  clash <- moved & new %in% df$code_commune[!moved]
  df$code_commune <- new
  df[!clash, , drop = FALSE]
}
dvf <- cog_rename(dvf); air <- cog_rename(air); ssmsi <- cog_rename(ssmsi)
dgfip <- cog_rename(dgfip); filo <- cog_rename(filo); dens <- cog_rename(dens)
defm <- cog_rename(defm); apl <- cog_rename(apl); baac <- cog_rename(baac)
carb <- cog_rename(carb); mob <- cog_rename(mob); bio <- cog_rename(bio)


# ---- PLM : sources publiées à l'ARRONDISSEMENT (751xx/693xx/132xx) ----
# commune_merged est indexée sur le code VILLE (75056/69123/13055, clé des
# élections). RÈGLE (audit C3) : si la source publie la ligne ville OFFICIELLE
# (FiLoSoFi, par ex.), on la GARDE telle quelle ; on ne reconstruit la ville depuis les
# arrondissements que lorsque la ligne ville manque (DVF, APL, DEFM) : somme (effectifs),
# moyenne pondérée par la population légale (APL) ou par le nb de transactions (DVF).
# APPROXIMATION documentée dans ce second cas (une médiane de médianes n'est pas la
# médiane de la ville ; un Gini moyen ignore l'inégalité entre arrondissements).
plm_city_of <- function(code) {
  unname(c(`751` = "75056", `693` = "69123", `132` = "13055")[substr(code, 1, 3)])
}
plm_arr_codes <- c(sprintf("751%02d", 1:20), sprintf("693%02d", 81:89),
                   sprintf("132%02d", 1:16))
#' @param sum_cols colonnes numériques à SOMMER même si how = "wmean" (effectifs)
plm_consolidate <- function(df, w, how = c("wmean", "sum"), sum_cols = character()) {
  how <- match.arg(how)
  arr <- df[df$code_commune %in% plm_arr_codes, , drop = FALSE]
  arr$.city <- plm_city_of(arr$code_commune)
  # villes dont la source publie déjà une ligne : on n'y touche pas
  arr <- arr[!arr$.city %in% df$code_commune, , drop = FALSE]
  if (nrow(arr) == 0) return(df)
  arr$.w <- w[match(arr$code_commune, names(w))]
  num <- setdiff(names(arr)[vapply(arr, is.numeric, logical(1))], ".w")
  city <- arr |>
    dplyr::group_by(code_commune = .city) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(num), ~ if (how == "sum" || dplyr::cur_column() %in% sum_cols)
                      sum(.x, na.rm = TRUE)
                    else stats::weighted.mean(.x, .w, na.rm = TRUE)),
      .groups = "drop")
  city[num] <- lapply(city[num], function(v) ifelse(is.nan(v), NA_real_, v))
  dplyr::bind_rows(df, city)
}
pop_w <- if (!is.null(pop)) stats::setNames(pop$pop_latest, pop$code_commune) else NULL

# ---- Préparer DVF : pivot 2024 App + Maison ----
dvf_2024 <- dvf |>
  dplyr::filter(annee == 2024, !estimation_insuffisante) |>
  dplyr::select(code_commune, type_local, prix_m2_median, n_transactions) |>
  tidyr::pivot_wider(
    names_from = type_local,
    values_from = c(prix_m2_median, n_transactions),
    names_glue = "{.value}_{type_local}"
  ) |>
  dplyr::rename(
    dvf_prix_app = prix_m2_median_Appartement,
    dvf_prix_mai = prix_m2_median_Maison,
    dvf_n_app    = n_transactions_Appartement,
    dvf_n_mai    = n_transactions_Maison
  )

dvf_w <- stats::setNames(dplyr::coalesce(dvf_2024$dvf_n_app, 0) + dplyr::coalesce(dvf_2024$dvf_n_mai, 0) + 1,
                         dvf_2024$code_commune)
dvf_2024 <- plm_consolidate(dvf_2024, dvf_w, sum_cols = c("dvf_n_app", "dvf_n_mai"))

# ---- Préparer Air ----
air_clean <- air |>
  dplyr::transmute(
    code_commune,
    air_qual  = qual_indice,
    air_no2   = no2_indice,
    air_o3    = o3_indice,
    air_pm10  = pm10_indice,
    air_pm25  = pm25_indice
  )

# ---- Préparer Élections ----
elec_clean <- elec |>
  dplyr::transmute(
    code_commune    = code_insee,
    nom_commune,
    code_dept,
    libelle_dept,
    tour_decisif    = tour,
    # Mesure de soutien = parts du 1er TOUR (% des exprimés, famille RN = RN +
    # UXD + EXD ; NFP, ENS, LR, Divers idem). Les parts du T2 (pct_RN...)
    # dépendent de la configuration du duel (désistements, triangulaires) et
    # ne sont définies que là où la famille s'est qualifiée : biais de sélection.
    elec_pct_nfp    = pct_NFP_t1,
    elec_pct_ens    = pct_ENS_t1,
    elec_pct_rn     = pct_RN_t1,
    elec_pct_lr     = pct_LR_t1,
    elec_pct_divers = pct_Divers_t1,
    elec_abstention = pct_abstention_t1,
    elec_marge      = marge_vainqueur,  # T2 : marge du vainqueur (qui a gagné)
    famille_vainqueur,
    ctx_inscrits    = inscrits
  )

# ---- Préparer populations (Bloc 1 INSEE) ----
if (!is.null(pop)) {
  pop_clean <- pop |>
    dplyr::transmute(
      code_commune,
      ctx_pop_latest = pop_latest,
      ctx_pop_2017   = pop_2017,
      ctx_pop_2021   = pop_2021,
      ctx_strate_pop = strate_pop
    )
}

# ---- Merge ----
# TABLE DES COMMUNES = COG 2026 (audit M1) : une ligne par commune du COG 2026
# (populations.parquet, type_com == "COM" : y compris Paris/Lyon/Marseille sous le code
# ville, sans arrondissements, sans circonscriptions consulaires ni COM). Les résultats
# électoraux (déjà au COG 2026, scripts/02_) y sont rattachés.
stopifnot(!is.null(pop))
dept_lbl <- elec_clean |> dplyr::distinct(code_dept, libelle_dept) |>
  dplyr::filter(!is.na(code_dept), !is.na(libelle_dept)) |> dplyr::distinct(code_dept, .keep_all = TRUE)
frame <- pop |>
  dplyr::filter(type_com == "COM") |>
  dplyr::transmute(code_commune, nom_cog = nom_commune, dept_cog = code_dept)
merged <- frame |>
  dplyr::left_join(dplyr::select(elec_clean, -code_dept, -libelle_dept), by = "code_commune") |>
  dplyr::mutate(nom_commune = dplyr::coalesce(nom_cog, nom_commune), code_dept = dept_cog,
                nom_cog = NULL, dept_cog = NULL) |>
  dplyr::left_join(dept_lbl, by = "code_dept") |>
  dplyr::relocate(code_dept, libelle_dept, .after = nom_commune) |>
  dplyr::left_join(dvf_2024,  by = "code_commune") |>
  dplyr::left_join(air_clean, by = "code_commune") |>
  dplyr::left_join(pop_clean, by = "code_commune")
stopifnot(!anyDuplicated(merged$code_commune), nrow(merged) == nrow(frame))

# Bloc 4 — Délinquance SSMSI
if (!is.null(ssmsi)) {
  ssmsi_clean <- ssmsi |>
    dplyr::select(code_commune,
                  dplyr::any_of(c(
                    "del_violences_sex", "del_cambriolages", "del_vols_pers",
                    "del_vols_violents", "del_vols_vehicules",
                    "del_degradations", "del_trafic_stup", "del_usage_stup",
                    "del_total_pour_mille"
                  )))
  merged <- merged |> dplyr::left_join(ssmsi_clean, by = "code_commune")
}

# Bloc 8 — Finances communales DGFiP
if (!is.null(dgfip)) {
  dgfip_clean <- dgfip |>
    dplyr::select(code_commune,
                  fin_recettes, fin_charges, fin_dette, fin_caf,
                  fin_taux_taxe_hab, fin_taux_fonc_bati)
  merged <- merged |> dplyr::left_join(dgfip_clean, by = "code_commune")
}

# Bloc 1 complément — FiLoSoFi 2021 revenus
if (!is.null(filo)) {
  filo_clean <- filo |>
    dplyr::select(code_commune, revenu_median, revenu_d1, revenu_d9, gini,
                  pct_imposes, part_prestations) |>
    plm_consolidate(pop_w)
  merged <- merged |> dplyr::left_join(filo_clean, by = "code_commune")
}

# Bloc 1 complément — Grille densité INSEE 2024
if (!is.null(dens)) {
  dens_clean <- dens |>
    dplyr::select(code_commune, densite_insee, densite_rang)
  merged <- merged |> dplyr::left_join(dens_clean, by = "code_commune")
}

# Bloc 3 — DEFM DARES (chômage absolu, à normaliser par pop)
if (!is.null(defm)) {
  defm_clean <- defm |>
    dplyr::select(code_commune, defm_abc) |>
    plm_consolidate(pop_w, "sum")
  merged <- merged |> dplyr::left_join(defm_clean, by = "code_commune")
}

# Bloc 5 — APL 5 professions de santé (désert médical)
if (!is.null(apl)) {
  apl_clean <- apl |>
    dplyr::select(code_commune,
                  dplyr::any_of(c("apl_medecins", "apl_medecins_jeunes",
                                  "apl_infirmieres", "apl_sagefemmes",
                                  "apl_dentistes", "apl_kines"))) |>
    plm_consolidate(pop_w)
  merged <- merged |> dplyr::left_join(apl_clean, by = "code_commune")
}

# Bloc C/I — BAAC accidents corporels (taux pour 1000 hab).
# Le fichier BAAC contient toutes les communes (0 = aucun accident enregistré).
# Une seule définition du taux : acc_taux_pop = n_accidents_2024 / pop INSEE * 1000.
if (!is.null(baac)) {
  merged <- merged |> dplyr::left_join(baac, by = "code_commune") |>
    dplyr::mutate(
      acc_taux_pop = ifelse(is.na(ctx_pop_latest) | ctx_pop_latest == 0,
                            NA_real_,
                            n_accidents_2024 / ctx_pop_latest * 1000)
    )
}

# Bloc G — Empreinte carbone par habitant (RARE/CITEPA)
if (!is.null(carb)) {
  merged <- merged |>
    dplyr::left_join(carb |> dplyr::select(code_commune, carb_hab_total),
                     by = "code_commune")
}

# Bloc I — Mobilité INSEE MOBPRO 2019 (modes + emploi local)
if (!is.null(mob)) {
  merged <- merged |>
    dplyr::left_join(
      mob |> dplyr::select(code_commune, mob_pct_emploi_local,
                           mob_pct_voiture, mob_pct_tc,
                           mob_pct_velo, mob_pct_pied),
      by = "code_commune"
    )
}

# Bloc K — Agriculture biologique (Agence Bio)
if (!is.null(bio)) {
  merged <- merged |>
    dplyr::left_join(
      bio |> dplyr::select(code_commune, bio_surface_ha,
                           bio_n_exploit, bio_n_operateurs),
      by = "code_commune"
    )
}

# Normaliser DEFM par population (taux chômage approx)
if ("defm_abc" %in% names(merged) && "ctx_pop_latest" %in% names(merged)) {
  merged <- merged |>
    dplyr::mutate(
      # DARES arrondit les effectifs à 5 : sous 100 hab. l'erreur d'arrondi
      # dépasse ±25 ‰ (médiane ~60 ‰) -> taux masqué (NA), effectif conservé.
      defm_taux_pop = ifelse(is.na(ctx_pop_latest) | ctx_pop_latest < 100,
                             NA_real_,
                             defm_abc / ctx_pop_latest * 1000)
    )
}


# ============================================================
# Vague 2 : nouvelles sources. Règles de publication (voir decisions.qmd) :
#   CAF : taux seulement si pop >= 1 000 (comptes arrondis à 5, MSA exclue)
#   DPE : part F/G seulement si n >= 30 diagnostics (échantillon non représentatif)
#   ORE : évolution 2019->dernière année seulement si aucune cellule RÉSIDENTIELLE
#         masquée aux deux dates (sinon borne basse ; plus fin que secret_tot), séries rupturées en 2018 : pas de pré-2018
#   Filosofi 2023 : niveau seul, NON comparable à Filosofi 2021 (jamais de delta)
# Seuls des taux / parts / densités sont ajoutés : aucun total brut (tautologie de taille).
# ============================================================
rd <- function(p) if (file.exists(p)) cog_rename(arrow::read_parquet(p)) else NULL
join_new <- function(merged, df, cols) {
  if (is.null(df)) return(merged)
  stopifnot(!anyDuplicated(df$code_commune))
  merged |> dplyr::left_join(dplyr::select(df, code_commune, dplyr::all_of(cols)),
                             by = "code_commune")
}

merged <- join_new(merged, rd(part_path),
  c("abst_pres22_t1", "abst_euro24", "abst_muni26_t1", "abst_delta_pres02_pres22"))

merged <- join_new(merged, rd(rp22_path),
  c("pct_res_secondaires", "pct_vacants", "pct_proprietaires",
    "pct_menages_sans_voiture", "taux_chomage_rp"))

merged <- join_new(merged, rd(cat_path),
  c("n_catnat_since_2015", "n_catnat_secheresse", "pprn_inondation_opposable"))

merged <- join_new(merged, rd(bpe_path), c("bpe_panier_score", "bpe_sante_pour1000"))

caf <- rd(caf_path)
if (!is.null(caf)) {                       # re-garde : taux seulement si pop >= 1000
  caf <- caf |> dplyr::mutate(
    caf_pct_pop_rsa = ifelse(caf_taux_affiche, caf_pct_pop_rsa, NA_real_),
    caf_pct_pop_al  = ifelse(caf_taux_affiche, caf_pct_pop_al,  NA_real_))
}
merged <- join_new(merged, caf, c("caf_pct_pop_rsa", "caf_pct_pop_al"))

merged <- join_new(merged, rd(arc_path), "arcep_pct_ftth")
merged <- join_new(merged, rd(enaf_path),
  c("enaf_ha_par_1000hab_2011_2025", "enaf_variation_vs_ref_pct"))

dpe <- rd(dpe_path)
if (!is.null(dpe)) dpe <- dpe |> dplyr::mutate(dpe_pct_fg = ifelse(dpe_n >= 30, dpe_pct_fg, NA_real_))
merged <- join_new(merged, dpe, "dpe_pct_fg")

ore <- rd(ore_path)
ore_long <- rd(newsrc("energie_ore", "conso_commune_long.parquet"))
if (!is.null(ore)) {
  # Masquage SPÉCIFIQUE au résidentiel (secret_tot, lui, agrège tous les secteurs
  # et écarterait des communes dont le résidentiel est intact). Une évolution est
  # une borne basse si une cellule résidentielle est masquée en 2019 ou en dernière année.
  masq <- function(en) {
    x <- ore_long[ore_long$secteur == "residentiel" & ore_long$energie == en &
                    ore_long$annee %in% c(2019L, ore$annee_ore[1]), ]
    unique(x$code_commune[x$secret_partiel])
  }
  ore <- ore |>
    dplyr::mutate(
      evol_elec_res_19_last_pct = ifelse(code_commune %in% masq("elec"), NA_real_, evol_elec_res_19_last_pct),
      evol_gaz_res_19_last_pct  = ifelse(code_commune %in% masq("gaz") | !gaz_res_publie,
                                         NA_real_, evol_gaz_res_19_last_pct),
      gaz_res_mwh_hab = ifelse(gaz_res_publie, gaz_res_mwh_hab, NA_real_),
      part_gaz_res    = ifelse(gaz_res_publie, part_gaz_res, NA_real_)) |>
    dplyr::rename(ore_elec_res_mwh_hab = elec_res_mwh_hab,
                  ore_elec_res_mwh_pdl = elec_res_mwh_pdl,
                  ore_gaz_res_mwh_hab  = gaz_res_mwh_hab,
                  ore_part_gaz_res     = part_gaz_res,
                  ore_evol_elec_res_19_24_pct = evol_elec_res_19_last_pct,
                  ore_evol_gaz_res_19_24_pct  = evol_gaz_res_19_last_pct)
}
merged <- join_new(merged, ore,
  c("ore_elec_res_mwh_hab", "ore_elec_res_mwh_pdl", "ore_gaz_res_mwh_hab",
    "ore_part_gaz_res", "ore_evol_elec_res_19_24_pct", "ore_evol_gaz_res_19_24_pct"))

merged <- join_new(merged, rd(filo23_path), c("rev_median_2023", "taux_pauvrete_2023"))

# Strates : quartiles de log(inscrits) — proxy taille (legacy)
# + strate_pop INSEE (réelle, depuis recensement)
merged <- merged |>
  dplyr::mutate(
    ctx_log_inscrits = log10(pmax(ctx_inscrits, 1)),
    ctx_log_pop      = log10(pmax(ctx_pop_latest, 1)),
    # Strate "taille commune" : INSEE pop si dispo, sinon proxy inscrits
    strate_taille = dplyr::case_when(
      !is.na(ctx_strate_pop)         ~ as.character(ctx_strate_pop),
      ctx_inscrits < 200             ~ "Très petite (< 500 hab.)",
      ctx_inscrits < 1000            ~ "Rurale (500-2 000)",
      ctx_inscrits < 5000            ~ "Bourg / périurbain (2 000-10 000)",
      ctx_inscrits >= 5000           ~ "Ville moyenne (10 000-50 000)",
      TRUE                           ~ NA_character_   # ni population ni inscrits : pas de strate inventée (audit N7)
    ),
    strate_taille = factor(strate_taille,
      levels = c("Très petite (< 500 hab.)", "Rurale (500-2 000)",
                 "Bourg / périurbain (2 000-10 000)",
                 "Ville moyenne (10 000-50 000)",
                 "Grande ville (≥ 50 000)"))
  )

stopifnot(!anyDuplicated(merged$code_commune))  # 1 ligne / commune
arrow::write_parquet(merged, file.path(out_dir, "commune_merged.parquet"))
message(glue::glue("✔ commune_merged.parquet : {nrow(merged)} communes"))


# ---- Registre des indicateurs (SOURCE UNIQUE) ----
# 80 indicateurs numériques : libellé, groupe thématique, unité et décimales d'affichage.
# Lu par l'Explorer (heatmap, nuage), la Carte bivariée et les Classements, et par
# site_numbers.R (nombre d'indicateurs). Deux variables d'un même groupe = corrélation
# triviale (filtrée du top inter-thématiques de l'Explorer). Filosofi 2023 est dans
# « Revenus » : sa corrélation avec Filosofi 2021 (rho 0,93) est une tautologie.
# Uniquement des taux, parts et densités (aucun total brut).
INDICATOR_META <- tibble::tribble(
  ~var,                            ~label,                          ~group,           ~unit,      ~dig,
  "elec_pct_nfp",                  "% NFP (législ. 2024, T1)",      "Élections",      "%",        1,
  "elec_pct_ens",                  "% Ensemble (législ. 2024, T1)", "Élections",      "%",        1,
  "elec_pct_rn",                   "% RN (législ. 2024, T1)",       "Élections",      "%",        1,
  "elec_pct_lr",                   "% LR (législ. 2024, T1)",       "Élections",      "%",        1,
  "elec_pct_divers",               "% Divers (législ. 2024, T1)",   "Élections",      "%",        1,
  "elec_abstention",               "Abstention (législ. 2024, T1)", "Élections",      "%",        1,
  "elec_marge",                    "Marge du vainqueur (T2)",       "Élections",      "pts",      1,
  "abst_pres22_t1",                "Abstention prés. 2022 T1",      "Élections",      "%",        1,
  "abst_euro24",                   "Abstention européennes 2024",   "Élections",      "%",        1,
  "abst_muni26_t1",                "Abstention municipales 2026 T1","Élections",      "%",        1,
  "abst_delta_pres02_pres22",      "Δ abstention 2002→2022 (pts)",  "Élections",      "pts",      1,
  "dvf_prix_app",                  "Prix m² appartement",           "Immobilier",     "€/m²",     0,
  "dvf_prix_mai",                  "Prix m² maison",                "Immobilier",     "€/m²",     0,
  "air_qual",                      "Indice ATMO global",            "Air",            "",         1,
  "air_no2",                       "NO₂",                           "Air",            "µg/m³",    1,
  "air_o3",                        "O₃",                            "Air",            "µg/m³",    1,
  "air_pm10",                      "PM₁₀",                          "Air",            "µg/m³",    1,
  "air_pm25",                      "PM₂.₅",                         "Air",            "µg/m³",    1,
  "ctx_log_inscrits",              "log(inscrits)",                 "Démographie",    "",         2,
  "ctx_log_pop",                   "log(population)",               "Démographie",    "",         2,
  "densite_rang",                  "Densité INSEE (1 rural à 6 centre)", "Démographie", "",       0,
  "del_total_pour_mille",          "Délinquance totale (13 postes) ‰ hab.", "Sécurité", "‰ hab.",   1,
  "del_cambriolages",              "Cambriolages ‰ logements",     "Sécurité",       "‰ log.",   2,
  "del_violences_sex",             "Violences sexuelles ‰",         "Sécurité",       "‰",        2,
  "del_trafic_stup",               "Trafic de stupéfiants ‰",       "Sécurité",       "‰",        2,
  "del_degradations",              "Dégradations ‰",                "Sécurité",       "‰",        2,
  "del_vols_pers",                 "Vols sur personnes ‰",          "Sécurité",       "‰",        2,
  "acc_taux_pop",                  "Accidents corporels BAAC ‰",    "Sécurité",       "‰",        2,
  "fin_recettes",                  "Recettes €/hab",                "Finances",       "€/hab",    0,
  "fin_charges",                   "Charges €/hab",                 "Finances",       "€/hab",    0,
  "fin_dette",                     "Dette €/hab",                   "Finances",       "€/hab",    0,
  "fin_caf",                       "CAF €/hab",                     "Finances",       "€/hab",    0,
  "fin_taux_taxe_hab",             "Taxe d'habitation (%)",         "Finances",       "%",        1,
  "fin_taux_fonc_bati",            "Foncier bâti (%)",              "Finances",       "%",        1,
  "revenu_median",                 "Revenu médian €",               "Revenus",        "€",        0,
  "revenu_d1",                     "Revenu D1 €",                   "Revenus",        "€",        0,
  "revenu_d9",                     "Revenu D9 €",                   "Revenus",        "€",        0,
  "gini",                          "Indice de Gini",                "Revenus",        "",         3,
  "pct_imposes",                   "% ménages imposés",             "Revenus",        "%",        1,
  "part_prestations",              "Part prestations (%)",          "Revenus",        "%",        1,
  "caf_pct_pop_rsa",               "RSA (% pop.)",                  "Revenus",        "%",        1,
  "caf_pct_pop_al",                "Aides au logement (% pop.)",    "Revenus",        "%",        1,
  "rev_median_2023",               "Revenu médian 2023 €",          "Revenus",        "€",        0,
  "taux_pauvrete_2023",            "Pauvreté 2023 (%)",             "Revenus",        "%",        1,
  "apl_medecins",                  "APL médecins",                  "Santé",          "c/an/hab", 2,
  "apl_medecins_jeunes",           "APL médecins ≤65 ans",          "Santé",          "c/an/hab", 2,
  "apl_infirmieres",               "APL infirmières",               "Santé",          "c/an/hab", 2,
  "apl_sagefemmes",                "APL sages-femmes",              "Santé",          "c/an/hab", 2,
  "apl_dentistes",                 "APL dentistes",                 "Santé",          "c/an/hab", 2,
  "apl_kines",                     "APL kinésithérapeutes",         "Santé",          "c/an/hab", 2,
  "defm_taux_pop",                 "Demandeurs d'emploi (DEFM) ‰ hab.", "Chômage",        "‰",        1,
  "taux_chomage_rp",               "Chômage RP 2022 (%)",           "Chômage",        "%",        1,
  "carb_hab_total",                "Empreinte CO₂ (t/hab)",         "Énergie",        "tCO₂/hab", 2,
  "ore_elec_res_mwh_hab",          "Électricité rés. MWh/hab.",     "Énergie",        "MWh/hab.", 2,
  "ore_elec_res_mwh_pdl",          "Électricité rés. MWh/PDL",      "Énergie",        "MWh/PDL",  2,
  "ore_gaz_res_mwh_hab",           "Gaz rés. MWh/hab.",             "Énergie",        "MWh/hab.", 2,
  "ore_part_gaz_res",              "Part du gaz (rés.)",            "Énergie",        "%",        1,
  "ore_evol_elec_res_19_24_pct",   "Évol. élec. rés. 2019-24 (%)",  "Énergie",        "%",        1,
  "ore_evol_gaz_res_19_24_pct",    "Évol. gaz rés. 2019-24 (%)",    "Énergie",        "%",        1,
  "mob_pct_emploi_local",          "% emploi local",                "Mobilité",       "%",        1,
  "mob_pct_voiture",               "% voiture",                     "Mobilité",       "%",        1,
  "mob_pct_tc",                    "% transports en commun",        "Mobilité",       "%",        1,
  "mob_pct_velo",                  "% vélo",                        "Mobilité",       "%",        1,
  "mob_pct_pied",                  "% marche",                      "Mobilité",       "%",        1,
  "bio_surface_ha",                "Surface bio (ha)",              "Agriculture",    "ha",       0,
  "bio_n_exploit",                 "Nb exploitations bio",          "Agriculture",    "",         0,
  "bio_n_operateurs",              "Nb opérateurs bio",             "Agriculture",    "",         0,
  "pct_res_secondaires",           "% résidences secondaires",      "Logement",       "%",        1,
  "pct_vacants",                   "% logements vacants",           "Logement",       "%",        1,
  "pct_proprietaires",             "% propriétaires",               "Logement",       "%",        1,
  "pct_menages_sans_voiture",      "% ménages sans voiture",        "Logement",       "%",        1,
  "dpe_pct_fg",                    "% DPE F/G (échantillon)",       "Logement",       "%",        1,
  "n_catnat_since_2015",           "Arrêtés CatNat depuis 2015",    "Risques et sols","",         0,
  "n_catnat_secheresse",           "Arrêtés sécheresse",            "Risques et sols","",         0,
  "pprn_inondation_opposable",     "PPRN inondation opposable",     "Risques et sols","",         0,
  "enaf_ha_par_1000hab_2011_2025", "ENAF ha / 1 000 hab.",          "Risques et sols","ha",       2,
  "enaf_variation_vs_ref_pct",     "ENAF vs référence (%)",         "Risques et sols","%",        1,
  "bpe_panier_score",              "Panier de services (0-10)",     "Services",       "",         1,
  "bpe_sante_pour1000",            "Équip. santé / 1 000 hab.",     "Services",       "",         2,
  "arcep_pct_ftth",                "Fibre FttH (%)",                "Services",       "%",        1
)
stopifnot(nrow(INDICATOR_META) == 80, !anyDuplicated(INDICATOR_META$var),
          all(INDICATOR_META$var %in% names(merged)))
arrow::write_parquet(INDICATOR_META, file.path(out_dir, "indicator_meta.parquet"))

# ---- Corrélations ----
NUMERIC_VARS <- INDICATOR_META$var

corr_long <- function(df, method, strate_label = "Toutes") {
  num_df <- df[, NUMERIC_VARS, drop = FALSE]
  # Conversion en matrice + complete.obs par paire
  pairs <- expand.grid(v1 = NUMERIC_VARS, v2 = NUMERIC_VARS,
                       stringsAsFactors = FALSE)
  pairs <- pairs[pairs$v1 < pairs$v2, ]  # garder paires uniques
  out <- purrr::map_dfr(seq_len(nrow(pairs)), function(i) {
    x <- num_df[[pairs$v1[i]]]
    y <- num_df[[pairs$v2[i]]]
    valid <- !is.na(x) & !is.na(y)
    if (sum(valid) < 30) return(tibble::tibble(
      v1 = pairs$v1[i], v2 = pairs$v2[i], method = method,
      strate = strate_label, n = sum(valid), rho = NA_real_
    ))
    suppressWarnings({
      r <- cor(x[valid], y[valid], method = method)
    })
    tibble::tibble(
      v1 = pairs$v1[i], v2 = pairs$v2[i], method = method,
      strate = strate_label, n = sum(valid), rho = r
    )
  })
  out
}

message("Calcul des matrices de corrélation (Spearman + Pearson, global + 4 strates)...")
library(purrr)
corr_list <- list(
  corr_long(merged, "spearman", "Toutes"),
  corr_long(merged, "pearson",  "Toutes")
)
for (s in levels(merged$strate_taille)) {
  sub <- dplyr::filter(merged, strate_taille == s)
  if (nrow(sub) >= 50) {
    corr_list <- c(corr_list,
                   list(corr_long(sub, "spearman", s),
                        corr_long(sub, "pearson",  s)))
  }
}
correlations <- dplyr::bind_rows(corr_list)
arrow::write_parquet(correlations, file.path(out_dir, "correlations.parquet"))
message(glue::glue("✔ correlations.parquet : {nrow(correlations)} lignes"))

# ---- Bivariate bins (terciles) ----
# Pour quelques paires "intéressantes" pré-définies, terciles indépendants
# sur X et sur Y, puis classe 1-9 par commune.
bivariate_pairs <- list(
  c("elec_pct_rn", "dvf_prix_app"),
  c("elec_pct_rn", "air_pm25"),
  c("elec_pct_nfp", "dvf_prix_app"),
  c("elec_abstention", "dvf_prix_app"),
  c("dvf_prix_app", "air_qual"),
  # Croisements introduits avec la page Démographie + nouvelles sources
  c("defm_taux_pop", "elec_pct_rn"),
  c("defm_taux_pop", "revenu_median"),
  c("densite_rang", "dvf_prix_app"),
  c("densite_rang", "elec_pct_rn"),
  c("apl_medecins", "ctx_log_pop"),
  c("revenu_median", "elec_pct_rn"),
  c("gini", "del_total_pour_mille"),
  # Blocs G / I / K (énergie, mobilité, agriculture)
  c("carb_hab_total", "revenu_median"),
  c("mob_pct_voiture", "densite_rang"),
  c("mob_pct_emploi_local", "ctx_log_pop"),
  c("bio_surface_ha", "elec_pct_nfp")
)

bin_pair <- function(df, x, y) {
  xv <- df[[x]]; yv <- df[[y]]
  brk_x <- quantile(xv, c(1/3, 2/3), na.rm = TRUE)
  brk_y <- quantile(yv, c(1/3, 2/3), na.rm = TRUE)
  bx <- findInterval(xv, brk_x, left.open = FALSE) + 1L  # 1, 2, or 3
  by <- findInterval(yv, brk_y, left.open = FALSE) + 1L
  bx[is.na(xv)] <- NA_integer_
  by[is.na(yv)] <- NA_integer_
  tibble::tibble(
    code_commune = df$code_commune,
    pair = paste(x, y, sep = " × "),
    x_var = x, y_var = y,
    bin_x = bx, bin_y = by,
    classe = ifelse(is.na(bx) | is.na(by), NA_integer_, (by - 1L) * 3L + bx),
    brk_x_lo = brk_x[1], brk_x_hi = brk_x[2],
    brk_y_lo = brk_y[1], brk_y_hi = brk_y[2]
  )
}

bivariate <- dplyr::bind_rows(
  lapply(bivariate_pairs, function(p) bin_pair(merged, p[1], p[2]))
)
arrow::write_parquet(bivariate, file.path(out_dir, "bivariate_bins.parquet"))
message(glue::glue("✔ bivariate_bins.parquet : {nrow(bivariate)} lignes ({length(bivariate_pairs)} paires)"))

# ---- Rapport ----
cat("\n→ Aperçu des 10 corrélations Spearman globales les plus fortes (|rho|):\n")
print(correlations |>
        dplyr::filter(method == "spearman", strate == "Toutes") |>
        dplyr::arrange(dplyr::desc(abs(rho))) |>
        head(10))

cat("\n→ Spearman %RN vs autres :\n")
print(correlations |>
        dplyr::filter(method == "spearman", strate == "Toutes",
                      v1 == "elec_pct_rn" | v2 == "elec_pct_rn") |>
        dplyr::arrange(dplyr::desc(abs(rho))))

# ---- Agrégats départementaux PONDÉRÉS par la population (C11) ----
# pages/departements.qmd agrégeait chaque commune à poids égal (une commune de
# 23 hab pesait autant que Marseille). Ici : médiane pondérée (prix, revenu, APL,
# chômage, carbone, voiture), taux = Σcompte/Σpop (accidents), moyenne pondérée
# (délinquance, restreinte aux communes où le total est publié ; la part de
# population couverte est fournie), somme (bio).
wmedian <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  x <- x[ok]; w <- w[ok]; o <- order(x); x <- x[o]; w <- w[o]
  x[which(cumsum(w) >= sum(w) / 2)[1]]
}
dept_agg_w <- merged |>
  dplyr::filter(!is.na(code_dept)) |>
  dplyr::group_by(code_dept, libelle_dept) |>
  dplyr::summarise(
    n_communes = dplyr::n(),
    pop_dept   = sum(ctx_pop_latest, na.rm = TRUE),
    prix_app   = wmedian(dvf_prix_app,    ctx_pop_latest),
    revenu     = wmedian(revenu_median,   ctx_pop_latest),
    apl        = wmedian(apl_medecins,    ctx_pop_latest),
    chomage    = wmedian(defm_taux_pop,   ctx_pop_latest),
    delinq     = {ok <- !is.na(del_total_pour_mille) & !is.na(ctx_pop_latest)
                  if (any(ok)) weighted.mean(del_total_pour_mille[ok], ctx_pop_latest[ok]) else NA_real_},
    delinq_pop_couverte = sum(ctx_pop_latest[!is.na(del_total_pour_mille)], na.rm = TRUE) /
                          sum(ctx_pop_latest, na.rm = TRUE),
    accidents  = ifelse(pop_dept > 0, sum(n_accidents_2024, na.rm = TRUE) / pop_dept * 1000, NA_real_),
    carbone    = wmedian(carb_hab_total,  ctx_pop_latest),
    voiture    = wmedian(mob_pct_voiture, ctx_pop_latest),
    bio        = sum(bio_surface_ha,      na.rm = TRUE),
    .groups = "drop"
  )
arrow::write_parquet(dept_agg_w, file.path(out_dir, "departements_agg.parquet"))
message(glue::glue("✔ departements_agg.parquet : {nrow(dept_agg_w)} départements (pondérés population)"))
