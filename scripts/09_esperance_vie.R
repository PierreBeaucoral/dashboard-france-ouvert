# ============================================================
# scripts/09_esperance_vie.R
#
# Bloc 5 (suite) — Espérance de vie à la naissance par commune.
#
# Source : data.gouv.fr (snapshot 2024 INSEE)
#   Dataset : 2024-esperance-de-vie-par-regions-departements-et-villes
#   Ressource : CSV 74 KB
#
# Format source : 1 colonne "Région-Département-Ville" + 1 colonne valeur.
# 1 680 lignes (les principales communes + agrégats région/département).
#
# Caveat : espérance de vie au niveau commune est BRUITÉE pour les
# petites communes (1-2 décès/an dominent la statistique). Médiane
# nationale ~83 ans = cohérent. Outliers (11, 105) à interpréter
# comme bruit de petit échantillon. Les analyses sérieuses devraient
# pondérer par taille ou utiliser le niveau dépt.
#
# Sortie : data/processed/insee_extra/esperance_vie.parquet
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(here)
  library(readr)
  library(stringr)
  library(glue)
})

raw_path <- here::here("data", "raw", "insee_extra", "esperance_vie.csv")
out_dir  <- here::here("data", "processed", "insee_extra")
out_path <- file.path(out_dir, "esperance_vie.parquet")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ev <- readr::read_csv(raw_path, show_col_types = FALSE)
names(ev) <- c("hierarchie", "esperance_vie")

# Parser "Région - Département - Ville" en 3 colonnes
ev_parsed <- ev |>
  tidyr::separate(hierarchie, into = c("region", "dept", "ville"),
                  sep = " - ", fill = "right", remove = FALSE) |>
  dplyr::mutate(
    # Si "ville" est NA, c'est un agrégat département/région — on les
    # garde séparément pour usage éventuel
    niveau = dplyr::case_when(
      is.na(dept)  ~ "region",
      is.na(ville) ~ "departement",
      TRUE         ~ "commune"
    )
  )

# Garder commune-level pour la carte (jointure par nom approximatif —
# pas d'INSEE code dans la source). On laisse la possibilité d'utiliser
# le niveau dept pour l'analyse macro.
ev_commune <- ev_parsed |>
  dplyr::filter(niveau == "commune") |>
  dplyr::transmute(
    nom_commune_norm = stringr::str_to_lower(stringr::str_trim(ville)),
    region, dept,
    nom_commune = ville,
    esperance_vie = as.numeric(esperance_vie)
  )

arrow::write_parquet(ev_commune, out_path)

# Aussi un agrégat dept (utile pour analyse macro / fallback)
ev_dept <- ev_parsed |>
  dplyr::filter(niveau == "departement") |>
  dplyr::transmute(
    region, nom_dept = dept,
    esperance_vie_dept = as.numeric(esperance_vie)
  )
arrow::write_parquet(ev_dept, file.path(out_dir, "esperance_vie_dept.parquet"))

cat(glue::glue("

  ✔ Espérance de vie {nrow(ev_commune)} communes → {out_path}
  ✔ Espérance de vie {nrow(ev_dept)} départements → esperance_vie_dept.parquet

  Distribution commune-level (à interpréter avec prudence — bruit fort
  sur les petites communes) :
"))
print(summary(ev_commune$esperance_vie))
cat("\n  Distribution dept-level (robuste) :\n")
print(summary(ev_dept$esperance_vie_dept))
