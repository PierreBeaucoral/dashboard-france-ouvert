# ============================================================
# scripts/15_agriculture_bio.R
#
# Bloc K — Surfaces et opérateurs en agriculture biologique par commune.
#
# Sources :
#   - Agence Bio · data.gouv.fr ID 61a6250d8660681353681fa8
#     * SAU bio (CSV 33 MB) : b7ce51bf-5675-4843-b618-247ef209416d
#     * Opérateurs bio (CSV 89 MB) : 130c2031-0b6a-45b2-aa42-23114f21a730
#
# Pour chaque commune on garde l'année la plus récente disponible (2024).
# Sortie : data/processed/agriculture/bio_commune.parquet
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(arrow)
  library(here)
  library(curl)
  library(stringr)
})

URL_SAU <- paste0(
  "https://static.data.gouv.fr/resources/",
  "historique-detaille-des-surfaces-cheptels-et-nombre-doperateurs-par-commune/",
  "20250701-134135/donnees-communes-sau.csv"
)
URL_OPS <- paste0(
  "https://static.data.gouv.fr/resources/",
  "historique-detaille-des-surfaces-cheptels-et-nombre-doperateurs-par-commune/",
  "20260305-140950/donnees-communes-operateur.csv"
)

raw_dir  <- here::here("data", "raw", "agriculture")
out_dir  <- here::here("data", "processed", "agriculture")
sau_path <- file.path(raw_dir, "bio_sau_commune.csv")
ops_path <- file.path(raw_dir, "bio_operateurs_commune.csv")
out_path <- file.path(out_dir, "bio_commune.parquet")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(sau_path) || file.size(sau_path) < 1e7) {
  message("Téléchargement Agence Bio SAU (33 MB)…")
  curl::curl_download(URL_SAU, sau_path, quiet = FALSE)
}
if (!file.exists(ops_path) || file.size(ops_path) < 1e7) {
  message("Téléchargement Agence Bio opérateurs (89 MB)…")
  curl::curl_download(URL_OPS, ops_path, quiet = FALSE)
}

# ---- Parsing SAU (CSV séparateur virgule) ----
# Colonnes exposées : annee, coderegion, region, codedepartement, departement,
# codeepci, epci, codeinseecommune, codepostalcommune, commune, nb_exp,
# surfab, surfc1, surfc2, surfc3, surfc123, surfbio.
sau <- readr::read_csv(sau_path,
                        locale = readr::locale(decimal_mark = ".",
                                               encoding = "UTF-8"),
                        col_types = readr::cols(.default = readr::col_character()),
                        show_col_types = FALSE)
message("\nColonnes SAU : ", paste(names(sau), collapse = ", "))

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

sau_latest <- sau_clean |>
  dplyr::filter(annee == last_year_sau) |>
  dplyr::group_by(code_commune) |>
  dplyr::summarise(
    bio_surface_ha   = sum(surface_bio, na.rm = TRUE),
    bio_surface_ab   = sum(surface_ab,  na.rm = TRUE),
    bio_n_exploit    = sum(nb_exp_bio,  na.rm = TRUE),
    .groups = "drop"
  )

# ---- Parsing Opérateurs (séparateur ; agrégé par ligne production) ----
ops <- readr::read_delim(ops_path, delim = ";",
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
