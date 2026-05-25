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
out_dir   <- here::here("data", "processed", "explorer")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Chargement ----
elec <- arrow::read_parquet(elec_path)
dvf  <- arrow::read_parquet(dvf_path)
air  <- arrow::read_parquet(air_path)
pop  <- if (file.exists(pop_path)) arrow::read_parquet(pop_path) else NULL

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
    elec_pct_nfp    = pct_NFP,
    elec_pct_ens    = pct_ENS,
    elec_pct_rn     = pct_RN,
    elec_pct_lr     = pct_LR,
    elec_pct_divers = pct_Divers,
    elec_abstention = pct_abstention,
    elec_marge      = marge_vainqueur,
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
merged <- elec_clean |>
  dplyr::left_join(dvf_2024,  by = "code_commune") |>
  dplyr::left_join(air_clean, by = "code_commune")

if (!is.null(pop)) {
  merged <- merged |> dplyr::left_join(pop_clean, by = "code_commune")
} else {
  merged <- merged |> dplyr::mutate(
    ctx_pop_latest = NA_integer_,
    ctx_strate_pop = NA_character_
  )
}

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
      TRUE                            ~ "Ville moyenne (10 000-50 000)"
    ),
    strate_taille = factor(strate_taille,
      levels = c("Très petite (< 500 hab.)", "Rurale (500-2 000)",
                 "Bourg / périurbain (2 000-10 000)",
                 "Ville moyenne (10 000-50 000)",
                 "Grande ville (≥ 50 000)"))
  )

arrow::write_parquet(merged, file.path(out_dir, "commune_merged.parquet"))
message(glue::glue("✔ commune_merged.parquet : {nrow(merged)} communes"))

# ---- Corrélations ----
NUMERIC_VARS <- c(
  "elec_pct_nfp", "elec_pct_ens", "elec_pct_rn", "elec_pct_lr",
  "elec_pct_divers", "elec_abstention", "elec_marge",
  "dvf_prix_app", "dvf_prix_mai",
  "air_qual", "air_no2", "air_o3", "air_pm10", "air_pm25",
  "ctx_log_inscrits", "ctx_log_pop"
)

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
  c("dvf_prix_app", "air_qual")
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
