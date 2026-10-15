# ============================================================
# R/indicator_sources.R
# Source et millésime de chaque indicateur du registre
# (explorer/indicator_meta.parquet : libellé, groupe, unité, décimales).
# Le registre ne porte pas le millésime : cette table le complète, pour que
# toute valeur affichée (page Constellation) soit accompagnée de son année.
# Règles ordonnées : la première qui correspond gagne. `indicator_source()`
# échoue si une variable n'a aucune règle (un nouvel indicateur doit être daté).
# ============================================================
IND_SOURCE_RULES <- c(
  "^elec_marge$"                 = "Législatives 2024 (T2)",
  "^elec_"                       = "Législatives 2024 (T1)",
  "^abst_pres22"                 = "Présidentielle 2022 (T1)",
  "^abst_euro24"                 = "Européennes 2024",
  "^abst_muni26"                 = "Municipales 2026 (T1)",
  "^abst_delta"                  = "Présidentielles 2002 et 2022",
  "^dvf_"                        = "DVF 2024",
  "^air_"                        = "Atmo, 30 derniers jours (sept.-oct. 2026)",
  "^ctx_log_pop$"                = "INSEE, population 2021",
  "^ctx_log_inscrits$"           = "Inscrits, élections 2024",
  "^densite_rang$"               = "INSEE, catégories d'espace 2024",
  "^del_"                        = "SSMSI 2025",
  "^acc_"                        = "BAAC 2024",
  "^fin_"                        = "DGFiP, comptes 2024",
  "^(rev_median_2023|taux_pauvrete_2023)$" = "FiLoSoFi 2023",
  "^(revenu_|gini$|pct_imposes$|part_prestations$)" = "FiLoSoFi 2021",
  "^caf_"                        = "CAF, décembre 2024",
  "^apl_"                        = "DREES, APL 2023",
  "^defm_"                       = "DARES, DEFM 2024-T4",
  "^taux_chomage_rp$"            = "INSEE, RP 2022",
  "^carb_"                       = "RARE/CITEPA, 2018",
  "^ore_evol_"                   = "Agence ORE, 2019 à 2024",
  "^ore_"                        = "Agence ORE 2024 (population INSEE 2021)",
  "^mob_"                        = "INSEE MOBPRO, RP 2022",
  "^bio_"                        = "Agence Bio, 2024",
  "^(pct_res_secondaires|pct_vacants|pct_proprietaires|pct_menages_sans_voiture)$" = "INSEE, RP 2022",
  "^dpe_"                        = "ADEME DPE, depuis 07/2021",
  "^(n_catnat_|pprn_)"           = "GASPAR 2025",
  "^enaf_"                       = "Cerema, ENAF 2011-2025",
  "^bpe_"                        = "INSEE BPE 2025",
  "^arcep_"                      = "ARCEP, 2026 T2"
)

indicator_source <- function(vars) {
  out <- vapply(vars, function(v) {
    i <- which(vapply(names(IND_SOURCE_RULES), function(p) grepl(p, v), logical(1)))
    if (length(i)) unname(IND_SOURCE_RULES[i[1]]) else NA_character_
  }, character(1))
  if (anyNA(out)) stop("Indicateurs sans source/millésime : ", paste(vars[is.na(out)], collapse = ", "))
  unname(out)
}
