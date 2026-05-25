# ============================================================
# scripts/11_baac.R
#
# Bloc C/I — BAAC (Bulletin d'Analyse des Accidents Corporels)
#   Accidents corporels de la circulation routière 2024.
#
# Source : data.gouv.fr, dataset 53698f4ca3a729239d2036df
#   (Ministère de l'Intérieur · ONISR Observatoire National
#    Interministériel de la Sécurité Routière)
#
# On retient le fichier "caractéristiques" (1 ligne / accident, avec
# code commune). On compte les accidents par commune sur 2024.
# Normalisation par 1000 hab → taux d'accidentologie comparable.
#
# Sortie : data/processed/baac/accidents_commune.parquet
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(arrow)
  library(here)
  library(stringr)
  library(glue)
})

raw_path <- here::here("data", "raw", "baac", "caract_2024.csv")
out_dir  <- here::here("data", "processed", "baac")
out_path <- file.path(out_dir, "accidents_commune.parquet")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# CSV séparateur ";", quotation "
caract <- readr::read_delim(raw_path, delim = ";",
                            show_col_types = FALSE,
                            locale = readr::locale(encoding = "UTF-8"))
message(glue::glue("BAAC caract 2024 : {nrow(caract)} accidents, ",
                   "cols : {paste(names(caract), collapse=', ')}"))

# BAAC : la colonne `com` contient déjà le code INSEE 5 chars complet
# (inutile de concaténer avec dep). Quelques rares NA possibles.
caract <- caract |>
  dplyr::filter(!is.na(com), nchar(as.character(com)) == 5) |>
  dplyr::mutate(code_commune = as.character(com))

# Compte par commune
agg <- caract |>
  dplyr::group_by(code_commune) |>
  dplyr::summarise(
    n_accidents_2024 = dplyr::n(),
    n_acc_intersection = sum(int != 1, na.rm = TRUE),
    n_acc_nuit = sum(lum %in% c(3, 4, 5), na.rm = TRUE),  # nuit
    .groups = "drop"
  )

arrow::write_parquet(agg, out_path)

cat(glue::glue("

  ✔ BAAC 2024 : {nrow(agg)} communes avec ≥1 accident
    → {out_path}
    ({round(file.size(out_path)/1024)} KB)
    Total accidents : {sum(agg$n_accidents_2024)}
    Médiane / commune : {median(agg$n_accidents_2024)}
    Max (Paris probablement) : {max(agg$n_accidents_2024)}
"))
