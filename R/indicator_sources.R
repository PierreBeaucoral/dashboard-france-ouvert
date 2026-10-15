# ============================================================
# R/indicator_sources.R
# Source et millésime de chaque indicateur du registre
# (explorer/indicator_meta.parquet : libellé, groupe, unité, décimales).
# Le registre ne porte pas le millésime : cette table le complète, pour que
# toute valeur affichée (page Constellation) soit accompagnée de son année.
# Règles ordonnées : la première qui correspond gagne. `indicator_source()`
# échoue si une variable n'a aucune règle (un nouvel indicateur doit être daté).
# ============================================================
# Millésimes portés par les données (audit M4) : lus dans les parquets, jamais figés ici,
# pour qu'un rafraîchissement trimestriel/mensuel ne contredise pas l'étiquette. Repli
# (parquet absent) = libellé générique sans date.
.vint <- function(rel, col, fmt = identity, fallback = "") {
  f <- file.path("data", "processed", rel)
  if (!requireNamespace("arrow", quietly = TRUE) || !file.exists(f)) return(fallback)
  v <- unique(stats::na.omit(arrow::read_parquet(f, col_select = dplyr::all_of(col))[[col]]))
  if (length(v) == 0L) fallback else fmt(max(v))
}
.v_defm  <- .vint("dares/defm_commune.parquet", "trimestre")
.v_air   <- .vint("air/atmo_snapshot.parquet", "date_fenetre", identity, "moyenne 30 jours")
.v_ssmsi <- .vint("ssmsi/delinquance_commune.parquet", "annee_ssmsi")
.v_dgfip <- .vint("dgfip/comptes_communes.parquet", "annee_dgfip")
.v_caf   <- .vint("insee_extra/caf_allocataires.parquet", "caf_annee")
.v_arcep <- .vint("insee_extra/arcep_fibre.parquet", "arcep_trimestre")
.v_apl   <- .vint("drees/apl_medecins.parquet", "apl_millesime")

IND_SOURCE_RULES <- c(
  "^elec_marge$"                 = "Législatives 2024 (T2)",
  "^elec_"                       = "Législatives 2024 (T1)",
  "^abst_pres22"                 = "Présidentielle 2022 (T1)",
  "^abst_euro24"                 = "Européennes 2024",
  "^abst_muni26"                 = "Municipales 2026 (T1)",
  "^abst_delta"                  = "Présidentielles 2002 et 2022",
  "^dvf_"                        = "DVF 2024",
  "^air_"                        = paste0("Atmo, ", .v_air),
  "^ctx_log_pop$"                = "INSEE, population légale (RP 2021 ; Mayotte 2017)",
  "^ctx_log_inscrits$"           = "Inscrits, élections 2024",
  "^densite_rang$"               = "INSEE, catégories d'espace 2024",
  "^del_"                        = paste("SSMSI", .v_ssmsi),
  "^acc_"                        = "BAAC 2024 (Mayotte : population 2017)",
  "^fin_"                        = paste("DGFiP, comptes", .v_dgfip),
  "^(rev_median_2023|taux_pauvrete_2023)$" = "FiLoSoFi 2023",
  "^(revenu_|gini$|pct_imposes$|part_prestations$)" = "FiLoSoFi 2021",
  "^caf_"                        = paste("CAF, fin", .v_caf, "(Mayotte : population 2017)"),
  "^apl_"                        = paste("DREES, APL", .v_apl),
  "^defm_"                       = paste("DARES, DEFM", .v_defm, "(Mayotte : population 2017)"),
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
  "^arcep_"                      = paste("ARCEP,", .v_arcep)
)

indicator_source <- function(vars) {
  out <- vapply(vars, function(v) {
    i <- which(vapply(names(IND_SOURCE_RULES), function(p) grepl(p, v), logical(1)))
    if (length(i)) unname(IND_SOURCE_RULES[i[1]]) else NA_character_
  }, character(1))
  if (anyNA(out)) stop("Indicateurs sans source/millésime : ", paste(vars[is.na(out)], collapse = ", "))
  unname(out)
}
