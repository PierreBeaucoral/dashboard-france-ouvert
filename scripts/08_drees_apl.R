# ============================================================
# scripts/08_drees_apl.R
#
# Bloc 5 (partiel) — Accessibilité Potentielle Localisée (APL) aux
# médecins généralistes par commune.
#
# Source : DREES via data.gouv.fr
#   Dataset : l-accessibilite-potentielle-localisee-apl
#   Ressource : APL aux médecins généralistes.xlsx (4.7 MB)
#   Année retenue : APL 2023 (la plus récente)
#
# L'APL est le nombre de consultations de médecine générale accessibles
# par an et par habitant, calculé en tenant compte de l'offre des médecins
# des communes environnantes, pondéré par la distance et la disponibilité.
# C'est l'indicateur de référence pour mesurer la désertification médicale.
#
# Sortie : data/processed/drees/apl_medecins.parquet
#   1 ligne / commune : apl_total (tous médecins) + apl_jeunes (≤65 ans
#   = soutenabilité future)
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(arrow)
  library(here)
  library(glue)
  library(stringr)
})

raw_path <- here::here("data", "raw", "drees", "apl_medecins.xlsx")
out_dir  <- here::here("data", "processed", "drees")
out_path <- file.path(out_dir, "apl_medecins.parquet")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Lecture : 5 lignes d'en-tête à skipper avant la table
apl <- readxl::read_excel(raw_path, sheet = "APL 2023", skip = 8,
                          col_names = FALSE)

# Colonnes (d'après inspection schéma)
names(apl) <- c("code_commune", "nom_commune",
                "apl_total", "apl_total_alt",
                "apl_jeunes", "apl_jeunes_alt",
                "pop_apl", "pop_apl_alt")

apl_clean <- apl |>
  dplyr::filter(!is.na(code_commune), nchar(code_commune) == 5) |>
  dplyr::transmute(
    code_commune = stringr::str_pad(code_commune, 5, "left", "0"),
    nom_commune,
    apl_medecins        = as.numeric(apl_total),
    apl_medecins_jeunes = as.numeric(apl_jeunes),
    pop_referenced      = as.numeric(pop_apl)
  )

arrow::write_parquet(apl_clean, out_path)

cat(glue::glue("

  ✔ APL {nrow(apl_clean)} communes → {out_path}
    ({round(file.size(out_path)/1024)} KB)

  Distribution APL médecins (consultations/an/hab) :
"))
print(summary(apl_clean$apl_medecins))

cat(glue::glue("

  Sous le seuil de désert médical (< 2.5 cons/an/hab) :
"))
n_desert <- sum(apl_clean$apl_medecins < 2.5, na.rm = TRUE)
n_total  <- sum(!is.na(apl_clean$apl_medecins))
cat(glue::glue("    {n_desert} / {n_total} communes ({round(100*n_desert/n_total,1)}%)\n"))
