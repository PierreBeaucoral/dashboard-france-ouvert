# ============================================================
# scripts/cog_helpers.R — harmonisation des codes communes vers le COG 2026
#
# Sourcé par les scripts 14, 16, 17, 19-26 :  source(here::here("scripts", "cog_helpers.R"))
# Entrées : data/processed/insee/{commune_fusions,populations}.parquet (scripts/06_).
#
# Règle : on harmonise les EFFECTIFS (on les somme par code 2026), jamais les
# taux ; les taux se calculent ensuite avec la population INSEE.
# ============================================================

#' Table de passage code ancien -> code COG 2026.
#'
#' @param since date (chr "YYYY-MM-DD") : on n'applique que les mouvements
#'   effectifs à partir de cette date (= début du millésime géographique de la
#'   source). Évite de réécrire des codes réutilisés avant le millésime.
#' @param fus,valid  mouvements et codes vivants en 2026 ; par défaut lus dans
#'   data/processed/insee/ (écrits par 06_). 06_ les fournit lui-même, car il
#'   produit populations.parquet (évite la dépendance circulaire).
#' @return tibble(code_avant, code_2026) ; ne contient que les codes à changer.
cog_map <- function(since, fus = NULL, valid = NULL) {
  if (is.null(fus)) fus <- arrow::read_parquet(here::here("data", "processed", "insee", "commune_fusions.parquet"))
  fus <- fus |>
    dplyr::filter(date_effet >= as.Date(since), code_avant != code_apres) |>
    dplyr::arrange(dplyr::desc(date_effet)) |>
    dplyr::distinct(code_avant, .keep_all = TRUE) |>
    dplyr::select(code_avant, code_final = code_apres)
  if (is.null(valid)) valid <- arrow::read_parquet(here::here("data", "processed", "insee", "populations.parquet"))$code_commune
  # un code encore vivant en 2026 n'est jamais remappé (défusions / codes réutilisés)
  fus <- dplyr::filter(fus, !code_avant %in% valid)
  # chaînage jusqu'au point fixe (a -> b -> c), 10 passes max
  for (i in 1:10) {
    nxt <- fus |>
      dplyr::left_join(fus, by = c("code_final" = "code_avant"), suffix = c("", ".n")) |>
      dplyr::mutate(code_final = dplyr::coalesce(code_final.n, code_final)) |>
      dplyr::select(code_avant, code_final)
    if (identical(nxt$code_final, fus$code_final)) break
    fus <- nxt
  }
  dplyr::transmute(fus, code_avant, code_2026 = code_final) |>
    dplyr::filter(code_2026 %in% valid)
}

#' Remplace les codes par leur équivalent COG 2026 (inchangés si absents de la table).
to_cog2026 <- function(x, map) {
  m <- match(x, map$code_avant)
  ifelse(is.na(m), x, map$code_2026[m])
}

#' Communes rétablies 2024-2025 et leurs parents (pop_latest = populations légales 2023, cf. 06_).
#' Leur population est scindée mais les numérateurs des sources restent sur la commune fusionnée :
#' tout taux calculé avec `pop_latest` doit être masqué pour ces codes (audit round 3, B).
retablies_codes <- function() {
  p <- arrow::read_parquet(here::here("data", "processed", "insee", "populations.parquet"))
  p$code_commune[p$pop_millesime %in% 2023L]
}
