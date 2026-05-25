# ============================================================
# R/corr_helpers.R
# Helpers pour les corrélations et la stratification
# (à brancher en Étape 6)
# ============================================================

# Liste des variables candidates au pré-calcul de corrélations.
# Toute paire (X, Y) parmi cette liste est calculable et sélectionnable
# dans l'onglet Explorer.
correlation_variables <- c(
  # Élections (Législatives 2024 T2)
  "elec_pct_nfp",
  "elec_pct_ens",
  "elec_pct_rn",
  "elec_pct_lr",
  "elec_abstention",
  "elec_marge_vainqueur",
  # Immobilier (DVF)
  "dvf_prix_m2_appart",
  "dvf_prix_m2_maison",
  "dvf_n_transactions",
  # Qualité de l'air
  "air_no2_moy",
  "air_pm25_moy",
  "air_pm10_moy",
  # Contexte
  "ctx_pop_commune",
  "ctx_densite"
)

# Strates pour stratification (grille densité INSEE — 4 classes)
density_strata_levels <- c(
  "Commune densément peuplée",
  "Commune de densité intermédiaire",
  "Commune peu dense",
  "Commune très peu dense"
)

# compute_correlation_matrix(df, method = c("spearman", "pearson"))  — Étape 6
# bivariate_bin(df, x, y, n = 3)                                     — Étape 5
