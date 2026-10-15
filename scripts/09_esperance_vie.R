# ============================================================
# scripts/09_esperance_vie.R
#
# Bloc F (Santé suite) — Espérance de vie à la naissance, NIVEAU DÉPARTEMENT.
#
# Source : INSEE, "Espérance de vie en 2025 — comparaisons régionales et
# départementales" (https://www.insee.fr/fr/statistiques/2012749,
# fichier TCRD_050.xlsx ; État civil + estimations de population, provisoire).
# Hommes et femmes séparément, par département (96 + 5 DROM).
#
# REMPLACE l'ancienne source (jeu data.gouv "Espérance de vie par régions,
# départements et villes", 1 371 communes) : ses valeurs allaient de 11 à
# 105 ans (ce n'était pas une espérance de vie) -> abandonnée (2026-10-14).
#
# L'INSEE ne publie PAS d'espérance de vie communale fiable. Chaque commune
# reçoit donc la valeur de son département ; la colonne `esperance_vie_niveau`
# = "departement" l'indique. Les cartes montrent donc un aplat départemental.
#
# esperance_vie = moyenne simple hommes/femmes (approximation de l'ensemble
# des sexes : l'INSEE ne publie que H et F ; écart < 0,1 an vs pondérée).
#
# Sorties (data/processed/insee_extra/) :
#   esperance_vie_dept.parquet : 1 ligne / département
#   esperance_vie.parquet      : 1 ligne / commune (valeur du département)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(arrow); library(here); library(readxl)
  library(curl); library(glue)
})

URL <- "https://www.insee.fr/fr/statistiques/fichier/2012749/TCRD_050.xlsx"
raw_path <- here::here("data", "raw", "insee_extra", "TCRD_050.xlsx")
out_dir  <- here::here("data", "processed", "insee_extra")
dir.create(dirname(raw_path), recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
if (!file.exists(raw_path)) {
  h <- curl::new_handle(useragent = "Mozilla/5.0")
  curl::curl_download(URL, raw_path, handle = h, quiet = TRUE)
}

# Feuille 1 = départements : col1 code, col2 libellé, col3 H, col4 F
raw <- readxl::read_excel(raw_path, sheet = 1, col_names = FALSE,
                          .name_repair = "minimal")
dept <- as.data.frame(raw)[, 1:4] |>
  stats::setNames(c("code_dept", "libelle_dept", "ev_h", "ev_f")) |>
  dplyr::filter(grepl("^(\\d{2}|2A|2B|\\d{3})$", code_dept)) |>
  dplyr::mutate(
    ev_h = as.numeric(ev_h), ev_f = as.numeric(ev_f),
    esperance_vie = (ev_h + ev_f) / 2,
    esperance_vie_niveau = "departement",
    annee_ev = 2025L
  )
stopifnot(nrow(dept) >= 100, !anyNA(dept$esperance_vie),
          all(dept$esperance_vie > 70 & dept$esperance_vie < 95))
arrow::write_parquet(dept, file.path(out_dir, "esperance_vie_dept.parquet"))

# Attribution aux communes via le département (grille densité INSEE, 1 ligne / commune)
com <- arrow::read_parquet(here::here("data", "processed", "insee_densite",
                                      "grille_densite.parquet")) |>
  dplyr::select(code_commune, code_dept)
out <- com |>
  dplyr::inner_join(dplyr::select(dept, code_dept, esperance_vie,
                                  esperance_vie_h = ev_h, esperance_vie_f = ev_f,
                                  esperance_vie_niveau, annee_ev),
                    by = "code_dept")
arrow::write_parquet(out, file.path(out_dir, "esperance_vie.parquet"))

message(glue::glue("✔ {nrow(dept)} départements ; {nrow(out)}/{nrow(com)} communes ",
                   "(valeur du département)"))
print(summary(dept$esperance_vie))
