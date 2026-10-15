# ============================================================
# scripts/99_validate_data.R
#
# Garde-fou qualité : vérifie que chaque parquet dont dépendent les pages
# existe, a un nombre de communes plausible, les colonnes attendues, et des
# valeurs dans des bornes saines.
#
# Lancé dans le CI AVANT `quarto render`. Sortie non-nulle = build bloqué
# → on ne déploie jamais un site dont les données sont cassées (schéma
# changé en amont, fichier tronqué, colonne disparue, parquet vide).
#
# Lancement local : Rscript scripts/99_validate_data.R
# ============================================================

suppressPackageStartupMessages({
  library(arrow)
  library(here)
})

# ---- Spécification des contrôles ----
# Pour chaque parquet "critique" (utilisé par une page) :
#   min_rows / max_rows : bande plausible du nombre de lignes
#   cols                : colonnes obligatoires
#   ranges (option)     : liste colonne -> c(min, max) bornes des valeurs
#                         finies non-NA (au moins une valeur doit exister)
SPEC <- list(
  "geo/communes.geojson" = list(kind = "file", min_kb = 5000, max_kb = 20000),
  "geo/departements.geojson" = list(kind = "file", min_kb = 80, max_kb = 600),

  "elections/legislatives_2024_t2.parquet" = list(
    min_rows = 30000, max_rows = 40000,
    cols = c("code_insee", "nom_commune", "famille_vainqueur", "pct_abstention"),
    ranges = list(pct_abstention = c(0, 100))
  ),
  "municipales_2026/resultats.parquet" = list(
    min_rows = 2500, max_rows = 4000,
    cols = c("code_insee", "famille_vainqueur", "tour")
  ),
  "municipales_2026/elus_commune.parquet" = list(
    min_rows = 30000, max_rows = 40000,
    cols = c("code_commune", "n_elus", "pct_femmes", "age_median"),
    ranges = list(pct_femmes = c(0, 100), age_median = c(18, 100))
  ),
  "dvf/dvf_aggregated.parquet" = list(
    min_rows = 100000, max_rows = 250000,
    cols = c("code_commune", "annee", "type_local", "prix_m2_median"),
    ranges = list(prix_m2_median = c(50, 40000))
  ),
  "air/atmo_snapshot.parquet" = list(
    min_rows = 20000, max_rows = 36000,
    cols = c("code_commune", "qual_indice", "qual_mean", "date_fenetre"),
    ranges = list(qual_indice = c(1, 6))
  ),
  "ssmsi/delinquance_commune.parquet" = list(
    min_rows = 30000, max_rows = 36000,
    cols = c("code_commune", "del_total_pour_mille", "del_n_publies"),
    ranges = list(del_total_pour_mille = c(0, 2000)),
    unique_key = "code_commune"
  ),
  "baac/accidents_commune.parquet" = list(
    min_rows = 8000, max_rows = 36000,
    cols = c("code_commune", "n_accidents_2024"),
    no_na = "n_accidents_2024", unique_key = "code_commune"
  ),
  "dgfip/comptes_communes.parquet" = list(
    min_rows = 30000, max_rows = 36000,
    cols = c("code_commune", "fin_recettes", "fin_charges", "fin_dette"),
    # €/hab : un total en k€ dépasserait largement (Paris = 7 745 164)
    ranges = list(fin_recettes = c(0, 100000), fin_charges = c(0, 100000)),
    unique_key = "code_commune"
  ),
  "drees/apl_medecins.parquet" = list(
    min_rows = 30000, max_rows = 36000,
    cols = c("code_commune", "apl_medecins"),
    ranges = list(apl_medecins = c(0, 60))
  ),
  "filosofi/revenus_communes.parquet" = list(
    min_rows = 30000, max_rows = 36000,
    cols = c("code_commune", "revenu_median", "gini"),
    ranges = list(revenu_median = c(5000, 80000), gini = c(0, 1))
  ),
  "insee/populations.parquet" = list(
    min_rows = 34000, max_rows = 37000,
    cols = c("code_commune", "pop_latest", "strate_pop"),
    unique_key = "code_commune"
  ),
  "insee_densite/grille_densite.parquet" = list(
    min_rows = 33000, max_rows = 36000,
    cols = c("code_commune", "densite_insee", "densite_rang"),
    ranges = list(densite_rang = c(1, 6)), no_na = "densite_rang",
    unique_key = "code_commune"
  ),
  "insee_extra/esperance_vie.parquet" = list(
    min_rows = 33000, max_rows = 36000,
    cols = c("code_commune", "esperance_vie", "esperance_vie_niveau"),
    ranges = list(esperance_vie = c(70, 95)), unique_key = "code_commune"
  ),
  "insee_extra/esperance_vie_dept.parquet" = list(
    min_rows = 96, max_rows = 110,
    cols = c("code_dept", "esperance_vie"),
    ranges = list(esperance_vie = c(70, 95))
  ),
  "energie/carbone_commune.parquet" = list(
    min_rows = 33000, max_rows = 36000,
    cols = c("code_commune", "carb_hab_total"),
    ranges = list(carb_hab_total = c(1, 200))
  ),
  "mobilite/mobilite_commune.parquet" = list(
    min_rows = 33000, max_rows = 36000,
    cols = c("code_commune", "mob_pct_voiture", "mob_pct_emploi_local"),
    ranges = list(mob_pct_voiture = c(0, 100))
  ),
  "mobilite/flows_per_commune.parquet" = list(
    min_rows = 200000, max_rows = 400000,
    cols = c("code_focal", "direction", "code_counter", "flux")
  ),
  "agriculture/bio_commune.parquet" = list(
    min_rows = 18000, max_rows = 30000,
    cols = c("code_commune", "bio_surface_ha", "bio_n_operateurs")
  ),
  "explorer/commune_merged.parquet" = list(
    min_rows = 34000, max_rows = 37000,
    cols = c("code_commune", "elec_pct_rn", "dvf_prix_app"),
    unique_key = "code_commune"
  ),
  "explorer/correlations.parquet" = list(
    min_rows = 5000, max_rows = 60000,
    cols = c("v1", "v2", "method", "strate", "rho")
  ),
  "explorer/bivariate_bins.parquet" = list(
    min_rows = 300000, max_rows = 800000,
    cols = c("code_commune", "pair", "bin_x", "bin_y")
  ),

  # ---- Vague 2 (2026-10-14) : nouvelles sources ----
  "elections/participation_commune.parquet" = list(
    min_rows = 34000, max_rows = 36000, unique_key = "code_commune",
    cols = c("code_commune", "abst_pres22_t1", "abst_euro24", "abst_muni26_t1", "abst_delta_pres02_pres22"),
    ranges = list(abst_pres22_t1 = c(0, 100), abst_euro24 = c(0, 100), abst_muni26_t1 = c(0, 100),
                  abst_delta_pres02_pres22 = c(-100, 100))
  ),
  "insee_extra/rp2022_logement_emploi.parquet" = list(
    min_rows = 34000, max_rows = 36000, unique_key = "code_commune",
    cols = c("code_commune", "pct_res_secondaires", "pct_vacants", "pct_proprietaires",
             "pct_menages_sans_voiture", "taux_chomage_rp"),
    ranges = list(pct_res_secondaires = c(0, 100), pct_vacants = c(0, 100),
                  pct_proprietaires = c(0, 100), pct_menages_sans_voiture = c(-0.01, 100),
                  taux_chomage_rp = c(0, 100))
  ),
  "risques/catnat_commune.parquet" = list(
    min_rows = 34000, max_rows = 36000, unique_key = "code_commune", no_na = "n_catnat_since_2015",
    cols = c("code_commune", "n_catnat_since_2015", "n_catnat_secheresse", "pprn_inondation_opposable"),
    ranges = list(n_catnat_since_2015 = c(0, 60), pprn_inondation_opposable = c(0, 1))
  ),
  "insee_extra/bpe_equipements.parquet" = list(
    min_rows = 34000, max_rows = 36000, unique_key = "code_commune", no_na = "bpe_panier_score",
    cols = c("code_commune", "bpe_panier_score", "bpe_sante_pour1000"),
    ranges = list(bpe_panier_score = c(0, 10), bpe_sante_pour1000 = c(0, 1000))
  ),
  "insee_extra/caf_allocataires.parquet" = list(
    min_rows = 34000, max_rows = 36000, unique_key = "code_commune",
    cols = c("code_commune", "caf_pct_pop_rsa", "caf_pct_pop_al", "caf_taux_affiche"),
    ranges = list(caf_pct_pop_rsa = c(0, 100), caf_pct_pop_al = c(0, 100))
  ),
  "insee_extra/arcep_fibre.parquet" = list(
    min_rows = 34000, max_rows = 36000, unique_key = "code_commune",
    cols = c("code_commune", "arcep_pct_ftth"), ranges = list(arcep_pct_ftth = c(0, 100))
  ),
  "risques/enaf_commune.parquet" = list(
    min_rows = 34000, max_rows = 36000, unique_key = "code_commune",
    cols = c("code_commune", "enaf_ha_par_1000hab_2011_2025", "enaf_variation_vs_ref_pct"),
    ranges = list(enaf_ha_par_1000hab_2011_2025 = c(0, 5000), enaf_variation_vs_ref_pct = c(-100, 10000))
  ),
  "insee_extra/dpe_commune.parquet" = list(
    min_rows = 34000, max_rows = 36000, unique_key = "code_commune",
    cols = c("code_commune", "dpe_n", "dpe_pct_fg"), ranges = list(dpe_pct_fg = c(0, 100))
  ),
  "energie_ore/energie_ore_commune.parquet" = list(
    min_rows = 34000, max_rows = 36000, unique_key = "code_commune",
    cols = c("code_commune", "elec_res_mwh_hab", "gaz_res_mwh_hab", "part_gaz_res", "gaz_res_publie", "secret_tot"),
    ranges = list(elec_res_mwh_hab = c(0, 100), part_gaz_res = c(0, 1))
  ),
  "filosofi_2023/filosofi_2023_communes.parquet" = list(
    min_rows = 34000, max_rows = 36000, unique_key = "code_commune",
    cols = c("code_commune", "rev_median_2023", "taux_pauvrete_2023"),
    ranges = list(rev_median_2023 = c(5000, 100000), taux_pauvrete_2023 = c(0, 100))
  )
)

failures <- character(0)
fail <- function(...) failures[[length(failures) + 1L]] <<- paste0(...)

for (rel in names(SPEC)) {
  spec <- SPEC[[rel]]
  path <- here::here("data", "processed", rel)

  # --- fichier brut (geojson) : taille seulement ---
  if (!is.null(spec$kind) && spec$kind == "file") {
    if (!file.exists(path)) { fail(rel, " : ABSENT"); next }
    kb <- file.size(path) / 1024
    if (kb < spec$min_kb) fail(rel, " : trop petit (", round(kb), " KB < ", spec$min_kb, ")")
    if (kb > spec$max_kb) fail(rel, " : trop gros (", round(kb), " KB > ", spec$max_kb, ")")
    next
  }

  # --- parquet ---
  if (!file.exists(path)) { fail(rel, " : ABSENT"); next }
  d <- tryCatch(arrow::read_parquet(path), error = function(e) NULL)
  if (is.null(d)) { fail(rel, " : illisible"); next }

  n <- nrow(d)
  if (n < spec$min_rows) fail(rel, " : ", n, " lignes < min ", spec$min_rows)
  if (n > spec$max_rows) fail(rel, " : ", n, " lignes > max ", spec$max_rows)

  missing <- setdiff(spec$cols, names(d))
  if (length(missing)) fail(rel, " : colonnes manquantes [", paste(missing, collapse = ", "), "]")

  # Colonne literalement nommée "NA" = artefact de parsing
  if ("NA" %in% names(d)) fail(rel, " : colonne fantôme nommée 'NA'")

  if (!is.null(spec$unique_key) && anyDuplicated(d[[spec$unique_key]]))
    fail(rel, " : ", sum(duplicated(d[[spec$unique_key]])), " ", spec$unique_key, " dupliqués")
  for (col in spec$no_na)
    if (col %in% names(d) && anyNA(d[[col]])) fail(rel, " : ", col, " contient ", sum(is.na(d[[col]])), " NA")

  # Bornes de valeurs : au moins une valeur finie, toutes dans [min,max]
  if (!is.null(spec$ranges)) {
    for (col in names(spec$ranges)) {
      if (!col %in% names(d)) next  # déjà signalé plus haut
      v <- suppressWarnings(as.numeric(d[[col]]))
      vf <- v[is.finite(v)]
      if (length(vf) == 0L) { fail(rel, " : ", col, " entièrement NA/vide"); next }
      lo <- spec$ranges[[col]][1]; hi <- spec$ranges[[col]][2]
      out <- sum(vf < lo | vf > hi)
      if (out > 0L) fail(rel, " : ", col, " a ", out, " valeurs hors [", lo, ", ", hi, "]")
    }
  }
}

# ---- Contrôles sémantiques (régressions des bugs C1-C11 de l'audit 2026-10-14) ----
rd <- function(rel) arrow::read_parquet(here::here("data", "processed", rel))
chk <- function(ok, msg) if (!isTRUE(ok)) fail("SEMANTIQUE : ", msg)
tryCatch({
  mg  <- rd("explorer/commune_merged.parquet")
  den <- rd("insee_densite/grille_densite.parquet")
  pop <- rd("insee/populations.parquet")
  dg  <- rd("dgfip/comptes_communes.parquet")
  ss  <- rd("ssmsi/delinquance_commune.parquet")
  ba  <- rd("baac/accidents_commune.parquet")

  # C1 : densite_rang monotone avec la taille (médiane de log10 pop par rang)
  dp <- merge(den, pop[, c("code_commune", "pop_latest")], by = "code_commune")
  med <- tapply(log10(pmax(dp$pop_latest, 1)), dp$densite_rang, median, na.rm = TRUE)
  chk(identical(sort(unique(den$densite_rang)), 1:6), "densite_rang doit valoir exactement 1..6")
  chk(all(diff(as.numeric(med)) > 0), paste0("densite_rang non monotone avec log(pop) : ",
      paste(round(med, 2), collapse = " < ")))
  chk(cor(dp$densite_rang, log10(pmax(dp$pop_latest, 1)), method = "spearman", use = "complete.obs") > 0.4,
      "rho(densite_rang, log pop) <= 0.4 (était -0.12 avec le bug)")

  # C2 : finances par habitant (Paris : quelques milliers d'€/hab, pas 7,7 M)
  p <- dg[dg$code_commune == "75056", ]
  chk(nrow(p) == 1 && p$fin_recettes > 1000 && p$fin_recettes < 10000, "Paris fin_recettes hors [1000, 10000] €/hab")
  chk(median(dg$fin_recettes, na.rm = TRUE) < 3000, "médiane fin_recettes > 3000 : totaux k€ ?")
  chk(abs(cor(dg$fin_recettes, dg$fin_pop_ref, method = "spearman", use = "complete.obs")) < 0.5,
      "fin_recettes quasi colinéaire à la population (totaux ?)")

  # C3 : total délinquance NA dès qu'une composante est non diffusée
  comp <- grep("^del_(?!total|n_)", names(ss), value = TRUE, perl = TRUE)
  incomplet <- rowSums(is.na(ss[, comp])) > 0
  chk(all(is.na(ss$del_total_pour_mille[incomplet])), "del_total renseigné alors qu'une composante est NA")
  chk(all(ss$del_n_publies == rowSums(!is.na(ss[, comp]))), "del_n_publies incohérent")
  # Paris : vols_pers (victimes ~45 pour mille) et non violences_extra (~5)
  pa <- ss[ss$code_commune == "75056", ]
  chk(pa$del_vols_pers > 20, "Paris del_vols_pers < 20 : colonnes SSMSI permutées ?")

  # C4 : accidents = 0 (pas NA) pour les communes couvertes
  chk(!anyNA(ba$n_accidents_2024), "accidents NA")
  chk(sum(ba$n_accidents_2024 == 0) > 15000, "trop peu de communes à 0 accident")
  chk(!anyNA(mg$acc_taux_pop[!is.na(mg$ctx_pop_latest) & mg$ctx_pop_latest > 0]), "acc_taux_pop NA pour commune couverte")

  # C7 / PLM : clés ville présentes avec une population et des données
  plm <- c("75056", "69123", "13055")
  chk(all(plm %in% mg$code_commune), "PLM absentes de commune_merged")
  pm <- mg[mg$code_commune %in% plm, ]
  chk(!anyNA(pm$ctx_pop_latest) && !anyNA(pm$dvf_prix_app) && !anyNA(pm$revenu_median) && !anyNA(pm$apl_medecins),
      "PLM : population/DVF/revenu/APL manquants")
  chk(all(c(sprintf("751%02d", 1:20)) %in% pop$code_commune), "arrondissements parisiens absents de populations")

  # C8 : un seul code par commune
  chk(!anyDuplicated(pop$code_commune), "populations : codes dupliqués")
  chk(!anyDuplicated(mg$code_commune), "commune_merged : codes dupliqués")

  # C10 : %RN du Explorer = 1er tour (identique à pct_RN_t1)
  el <- rd("elections/legislatives_2024_t2.parquet")
  j <- merge(mg[, c("code_commune", "elec_pct_rn")], el[, c("code_insee", "pct_RN_t1")],
             by.x = "code_commune", by.y = "code_insee")
  chk(isTRUE(all.equal(j$elec_pct_rn, j$pct_RN_t1)), "elec_pct_rn n'est pas le %RN du T1")

  # C11 : agrégats départementaux pondérés disponibles
  da <- rd("explorer/departements_agg.parquet")
  chk(all(c("pop_dept", "accidents", "delinq_pop_couverte") %in% names(da)), "departements_agg incomplet")

  # DGFiP : DROM (code dept 101-106) et départements 90-95 (code « 09x ») doivent être joints
  chk(sum(!is.na(mg$fin_recettes[substr(mg$code_commune, 1, 2) == "97"])) >= 100, "DGFiP : DROM sans comptes (recodage 101-106)")
  chk(sum(!is.na(mg$fin_recettes[substr(mg$code_commune, 1, 2) %in% c("91", "92", "93", "94", "95")])) >= 450, "DGFiP : Île-de-France sans comptes (code 09x)")

  # ---- Vague 2 : règles de publication et couverture des 28 nouveaux indicateurs ----
  meta <- rd("explorer/indicator_meta.parquet")
  chk(nrow(meta) == 80 && !anyDuplicated(meta$var), "indicator_meta : 80 variables uniques attendues")
  chk(all(meta$var %in% names(mg)), "indicator_meta : variable absente de commune_merged")
  cov_min <- c(abst_pres22_t1 = 34000, pct_res_secondaires = 32000, pct_proprietaires = 30000,
               n_catnat_since_2015 = 34000, bpe_panier_score = 34000, arcep_pct_ftth = 34000,
               enaf_ha_par_1000hab_2011_2025 = 33000, dpe_pct_fg = 22000,
               caf_pct_pop_rsa = 9500, rev_median_2023 = 30000, taux_pauvrete_2023 = 5000,
               ore_elec_res_mwh_hab = 34000, ore_gaz_res_mwh_hab = 9000,
               ore_evol_elec_res_19_24_pct = 33000, ore_evol_gaz_res_19_24_pct = 9000)
  for (v in names(cov_min))
    chk(sum(is.finite(mg[[v]])) >= cov_min[[v]], paste0("couverture ", v, " < ", cov_min[[v]]))
  # CAF : taux seulement pour pop >= 1000 (comptes arrondis à 5)
  chk(!any(!is.na(mg$caf_pct_pop_rsa) & mg$ctx_pop_latest < 1000, na.rm = TRUE), "CAF : taux publié sous 1 000 hab.")
  chk(!any(!is.na(mg$caf_pct_pop_al)  & mg$ctx_pop_latest < 1000, na.rm = TRUE), "CAF AL : taux publié sous 1 000 hab.")
  # DPE : part F/G seulement si n >= 30
  dpe <- rd("insee_extra/dpe_commune.parquet")
  dj <- merge(mg[, c("code_commune", "dpe_pct_fg")], dpe[, c("code_commune", "dpe_n")], by = "code_commune")
  chk(!any(!is.na(dj$dpe_pct_fg) & dj$dpe_n < 30), "DPE : part F/G publiée avec n < 30")
  # ORE : gaz seulement où publié ; pas d'évolution sur cellule résidentielle masquée
  ore <- rd("energie_ore/energie_ore_commune.parquet")
  oj <- merge(mg[, c("code_commune", "ore_gaz_res_mwh_hab", "ore_part_gaz_res")],
              ore[, c("code_commune", "gaz_res_publie")], by = "code_commune")
  chk(!any(!is.na(oj$ore_gaz_res_mwh_hab) & !oj$gaz_res_publie, na.rm = TRUE), "ORE : gaz affiché hors communes gaz")
  chk(all(is.na(mg$ore_part_gaz_res) | (mg$ore_part_gaz_res >= 0 & mg$ore_part_gaz_res <= 1)), "ORE : part du gaz hors [0,1]")
  ol <- rd("energie_ore/conso_commune_long.parquet")
  mask_e <- unique(ol$code_commune[ol$secteur == "residentiel" & ol$energie == "elec" & ol$secret_partiel &
                                     ol$annee %in% c(2019L, max(ol$annee))])
  chk(all(is.na(mg$ore_evol_elec_res_19_24_pct[mg$code_commune %in% mask_e])), "ORE : évolution élec sur cellule masquée")
  chk(sum(ore$annee_ore == 2024, na.rm = TRUE) > 34000, "ORE : dernière année != 2024")
  # Filosofi 2023 : niveau seul, jamais de delta 2021-2023 dans les données
  chk(!any(grepl("delta.*2023|2023.*delta|evol.*2023", names(mg))), "Filosofi 2023 : variable de variation 2021-2023 interdite")
  # Panier BPE : entier 0-10
  chk(all(mg$bpe_panier_score[!is.na(mg$bpe_panier_score)] %in% 0:10), "bpe_panier_score hors 0..10")
  # PLM : villes présentes dans les nouvelles sources
  pm2 <- mg[mg$code_commune %in% c("75056", "69123", "13055"), ]
  chk(!anyNA(pm2$pct_vacants) && !anyNA(pm2$arcep_pct_ftth) && !anyNA(pm2$abst_pres22_t1), "PLM : logement/ARCEP/participation manquants")
}, error = function(e) fail("SEMANTIQUE : erreur d'exécution : ", conditionMessage(e)))

# ---- Rapport ----
cat("\n==================== VALIDATION DONNÉES ====================\n")
cat(sprintf("Contrôles : %d fichiers\n", length(SPEC)))
if (length(failures) == 0L) {
  cat("✔ Tous les contrôles passent.\n")
  quit(status = 0L)
} else {
  cat(sprintf("✗ %d ÉCHEC(S) :\n", length(failures)))
  for (f in failures) cat("  ✗ ", f, "\n", sep = "")
  cat("\nBuild bloqué : corriger les données avant de déployer.\n")
  quit(status = 1L)
}
