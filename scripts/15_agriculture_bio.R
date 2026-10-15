# ============================================================
# scripts/15_agriculture_bio.R
#
# Bloc K — Surfaces et opérateurs en agriculture biologique par commune.
#
# Sources (Agence Bio, data.gouv.fr dataset 61a6250d8660681353681fa8, permaliens stables
# « /datasets/r/<id> » qui suivent la dernière version de chaque ressource) :
#   - SAU bio, surfaces 2008-2025 : 0b668cd6-3f43-4c5a-a411-d59035e77cb7 (CSV 45 MB)
#   - Opérateurs bio 2008-2025   : d8d704fb-61ec-4035-acb3-773b5a63c23d (CSV 136 MB, virgule)
#
# Pour chaque commune on garde l'année la plus récente disponible.
# Sortie : data/processed/agriculture/bio_commune.parquet
#
# ATTENTION (décision 2026-10-15, voir docs/decisions.qmd) : la republication de septembre 2026
# applique le secret statistique (« NC ») à ~60 % des lignes commune-année de surface ; sommer ces
# NC comme zéro retirerait ~31 % des hectares nationaux 2024 (1,86 contre 2,71 Mha dans le
# parquet commité, millésime 2024 de l'ancienne publication). Ce n'est pas le même indicateur :
# le script s'ARRÊTE (sans écrire) tant que les surfaces sont masquées, et le parquet commité
# (2024, ancienne publication) est conservé. Hors du refresh automatique pour cette raison.
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(arrow)
  library(here)
  library(curl)
  library(stringr)
})

URL_SAU <- "https://www.data.gouv.fr/fr/datasets/r/0b668cd6-3f43-4c5a-a411-d59035e77cb7"
URL_OPS <- "https://www.data.gouv.fr/fr/datasets/r/d8d704fb-61ec-4035-acb3-773b5a63c23d"
# Millésime des libellés (« Agence Bio, 2024 » dans R/indicator_sources.R) : à changer ensemble.
ANNEE_ATTENDUE <- 2024L
# Part maximale tolérée de lignes de surface masquées (« NC ») ; l'ancienne publication : 0 %.
MAX_NC <- 0.05

raw_dir  <- here::here("data", "raw", "agriculture")
out_dir  <- here::here("data", "processed", "agriculture")
sau_path <- file.path(raw_dir, "bio_sau_commune.csv")
ops_path <- file.path(raw_dir, "bio_operateurs_commune.csv")
out_path <- file.path(out_dir, "bio_commune.parquet")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(sau_path) || file.size(sau_path) < 1e7) {
  message("Téléchargement Agence Bio SAU (33 MB)…")
  curl::curl_download(URL_SAU, sau_path, handle = curl::new_handle(useragent = "Mozilla/5.0", followlocation = TRUE), quiet = TRUE)
}
if (!file.exists(ops_path) || file.size(ops_path) < 1e7) {
  message("Téléchargement Agence Bio opérateurs (89 MB)…")
  curl::curl_download(URL_OPS, ops_path, handle = curl::new_handle(useragent = "Mozilla/5.0", followlocation = TRUE), quiet = TRUE)
}

# ---- Parsing SAU (CSV séparateur virgule, « NC » = secret statistique) ----
# Colonnes exposées : annee, coderegion, region, codedepartement, departement,
# codeepci, epci, codeinseecommune, codepostalcommune, commune, nb_exp,
# surfab, surfc1, surfc2, surfc3, surfc123, surfbio.
sau <- readr::read_csv(sau_path,
                        locale = readr::locale(decimal_mark = ".",
                                               encoding = "UTF-8"),
                        col_types = readr::cols(.default = readr::col_character()),
                        show_col_types = FALSE)
message("\nColonnes SAU : ", paste(names(sau), collapse = ", "))
stopifnot("colonnes SAU attendues" = all(c("annee", "codeinseecommune", "nb_exp", "surfab", "surfbio") %in% names(sau)))

sau_clean <- sau |>
  dplyr::transmute(
    code_commune = codeinseecommune,
    annee        = suppressWarnings(as.integer(annee)),
    nb_exp_bio   = suppressWarnings(as.integer(nb_exp)),
    surface_bio  = suppressWarnings(as.numeric(surfbio)),
    surface_ab   = suppressWarnings(as.numeric(surfab))
  ) |>
  dplyr::filter(!is.na(code_commune), !is.na(annee))

last_year_sau <- max(sau_clean$annee, na.rm = TRUE)
message(sprintf("→ Dernière année SAU bio : %d (%d communes)",
                last_year_sau,
                length(unique(sau_clean$code_commune[sau_clean$annee == last_year_sau]))))

# Garde « secret statistique » : part de lignes de l'année retenue dont la surface est « NC ».
nc_share <- mean(sau$surfbio[sau$annee == as.character(last_year_sau)] %in% "NC")
message(sprintf("→ Lignes de surface masquées (NC) en %d : %.1f %%", last_year_sau, 100 * nc_share))
if (nc_share > MAX_NC)
  stop(sprintf(paste0("15_agriculture_bio.R : %.0f %% des surfaces %d sont masquées (NC, secret ",
                      "statistique) : indicateur non comparable au parquet commité (2024, ancienne ",
                      "publication). Parquet conservé, rien écrit. Voir docs/decisions.qmd."),
               100 * nc_share, last_year_sau))
stopifnot("millésime SAU != ANNEE_ATTENDUE : mettre à jour les libellés (R/indicator_sources.R) et la constante" =
            last_year_sau == ANNEE_ATTENDUE)

sau_latest <- sau_clean |>
  dplyr::filter(annee == last_year_sau) |>
  dplyr::group_by(code_commune) |>
  dplyr::summarise(
    bio_surface_ha   = sum(surface_bio, na.rm = TRUE),
    bio_surface_ab   = sum(surface_ab,  na.rm = TRUE),
    bio_n_exploit    = sum(nb_exp_bio,  na.rm = TRUE),
    .groups = "drop"
  )

# ---- Parsing Opérateurs (séparateur détecté : « ; » avant 2026, « , » depuis la republication) ----
ops_delim <- if (grepl(";", readLines(ops_path, n = 1L, warn = FALSE), fixed = TRUE)) ";" else ","
ops <- readr::read_delim(ops_path, delim = ops_delim,
                          locale = readr::locale(decimal_mark = ".",
                                                 encoding = "UTF-8"),
                          col_types = readr::cols(.default = readr::col_character()),
                          show_col_types = FALSE)
message("\nColonnes opérateurs : ", paste(names(ops), collapse = ", "))

ops_clean <- ops |>
  dplyr::transmute(
    code_commune = codeinseecommune,
    annee        = suppressWarnings(as.integer(annee)),
    nb_op        = suppressWarnings(as.integer(nboperateur))
  ) |>
  dplyr::filter(!is.na(code_commune), !is.na(annee))

last_year_ops <- max(ops_clean$annee, na.rm = TRUE)
message(sprintf("→ Dernière année opérateurs : %d", last_year_ops))

# Le fichier est ventilé par production×activité — on somme sur commune×année.
ops_latest <- ops_clean |>
  dplyr::filter(annee == last_year_ops) |>
  dplyr::group_by(code_commune) |>
  dplyr::summarise(bio_n_operateurs = sum(nb_op, na.rm = TRUE),
                   .groups = "drop")

# ---- Merge SAU + Opérateurs ----
out <- sau_latest |>
  dplyr::full_join(ops_latest, by = "code_commune") |>
  dplyr::mutate(
    bio_surface_ha   = ifelse(is.na(bio_surface_ha), 0, bio_surface_ha),
    bio_n_operateurs = ifelse(is.na(bio_n_operateurs), 0L, bio_n_operateurs)
  )

arrow::write_parquet(out, out_path)

cat(sprintf("\n✔ %d communes → %s (%.1f KB)\n",
            nrow(out), out_path, file.size(out_path) / 1024))
cat("\nAperçu :\n")
print(head(out, 8))
cat("\nSurface bio (ha) :\n"); print(summary(out$bio_surface_ha))
cat("\nNb opérateurs :\n"); print(summary(out$bio_n_operateurs))
