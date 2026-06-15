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
    cols = c("code_commune", "qual_indice", "qual_mean"),
    ranges = list(qual_indice = c(1, 6))
  ),
  "ssmsi/delinquance_commune.parquet" = list(
    min_rows = 30000, max_rows = 36000,
    cols = c("code_commune", "del_total_pour_mille"),
    ranges = list(del_total_pour_mille = c(0, 2000))
  ),
  "baac/accidents_commune.parquet" = list(
    min_rows = 8000, max_rows = 36000,
    cols = c("code_commune", "n_accidents_2024")
  ),
  "dgfip/comptes_communes.parquet" = list(
    min_rows = 30000, max_rows = 36000,
    cols = c("code_commune", "fin_recettes", "fin_charges")
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
    cols = c("code_commune", "pop_latest", "strate_pop")
  ),
  "insee_densite/grille_densite.parquet" = list(
    min_rows = 33000, max_rows = 36000,
    cols = c("code_commune", "densite_insee")
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
    cols = c("code_commune", "elec_pct_rn", "dvf_prix_app")
  ),
  "explorer/correlations.parquet" = list(
    min_rows = 5000, max_rows = 30000,
    cols = c("v1", "v2", "method", "strate", "rho")
  ),
  "explorer/bivariate_bins.parquet" = list(
    min_rows = 300000, max_rows = 800000,
    cols = c("code_commune", "pair", "bin_x", "bin_y")
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
