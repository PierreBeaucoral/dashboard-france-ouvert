# ============================================================
# scripts/09_esperance_vie.R
#
# Bloc F (Santé suite) — Espérance de vie à la naissance par commune.
#
# Source : data.gouv.fr "2024 : Espérance de vie par régions,
# départements et villes" — INSEE.
#
# Format source : 1 col "Région-Département-Ville" + 1 col valeur années.
# 1 680 communes (principales). Pas de code INSEE → join par (nom_commune,
# libelle_dept) normalisé contre le référentiel des populations INSEE.
#
# Caveat : médiane nationale ~83 ans cohérente, mais variance brute par
# commune (1-2 décès dominent une petite commune). À interpréter avec
# prudence sur les très petites populations.
#
# Sortie : data/processed/insee_extra/esperance_vie.parquet
#   (code_commune, esperance_vie, nom_commune, libelle_dept)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(arrow)
  library(here)
  library(readr)
  library(stringr)
  library(glue)
})

raw_path  <- here::here("data", "raw", "insee_extra", "esperance_vie.csv")
out_dir   <- here::here("data", "processed", "insee_extra")
out_path  <- file.path(out_dir, "esperance_vie.parquet")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ev <- readr::read_csv(raw_path, show_col_types = FALSE)
names(ev) <- c("hierarchie", "esperance_vie")

# Parser "Région - Département - Ville"
ev_parsed <- ev |>
  tidyr::separate(hierarchie, into = c("region", "dept", "ville"),
                  sep = " - ", fill = "right", remove = FALSE) |>
  dplyr::mutate(
    niveau = dplyr::case_when(
      is.na(dept)  ~ "region",
      is.na(ville) ~ "departement",
      TRUE         ~ "commune"
    )
  )

# Garde commune-level pour le join
ev_commune <- ev_parsed |>
  dplyr::filter(niveau == "commune") |>
  dplyr::select(region, dept_src = dept, nom_src = ville, esperance_vie)

# ---- Normalisation pour join (case + accents + espaces) ----
normalize <- function(s) {
  s |>
    stringr::str_to_lower() |>
    stringr::str_trim() |>
    iconv(to = "ASCII//TRANSLIT", from = "UTF-8") |>
    stringr::str_remove_all("[\\'\\\"^]") |>
    stringr::str_replace_all("\\s+", " ")
}

# Référentiel : combinaison nom commune + nom dept depuis les élections
# (= seule source qui a les deux noms harmonisés à l'INSEE et code_insee)
elec_path <- here::here("data", "processed", "elections",
                        "legislatives_2024_t2.parquet")
ref <- arrow::read_parquet(elec_path) |>
  dplyr::transmute(
    code_commune = code_insee,
    key = paste(normalize(nom_commune), normalize(libelle_dept), sep = "|")
  ) |>
  dplyr::distinct(key, .keep_all = TRUE)

ev_commune <- ev_commune |>
  dplyr::mutate(key = paste(normalize(nom_src), normalize(dept_src), sep = "|"))

joined <- ev_commune |>
  dplyr::left_join(ref, by = "key")

match_rate <- sum(!is.na(joined$code_commune)) / nrow(joined)
message(glue::glue("Match : {sum(!is.na(joined$code_commune))} / {nrow(joined)} ",
                   "({round(match_rate*100,1)}%)"))

# Sortie : 1 ligne / commune matchée
out <- joined |>
  dplyr::filter(!is.na(code_commune)) |>
  dplyr::transmute(
    code_commune,
    esperance_vie = as.numeric(esperance_vie),
    nom_src,
    dept_src,
    region
  ) |>
  dplyr::distinct(code_commune, .keep_all = TRUE)

arrow::write_parquet(out, out_path)

cat(glue::glue("

  ✔ {nrow(out)} communes matchées sur {nrow(ev_commune)} ({round(100*nrow(out)/nrow(ev_commune),1)}%) → {out_path}

  Distribution espérance de vie (années) :
"))
print(summary(out$esperance_vie))
