# ============================================================
# scripts/08_drees_apl.R
#
# Bloc 5 (Santé) — Accessibilité Potentielle Localisée (APL) à 5
# professions de santé : médecins généralistes, infirmières,
# sages-femmes, chirurgiens-dentistes, kinésithérapeutes.
#
# Source : DREES via data.gouv.fr
#   Dataset : l-accessibilite-potentielle-localisee-apl
#   Année retenue : APL 2023 (la plus récente)
#
# Format XLSX commun aux 5 fichiers : 8 lignes d'en-tête, puis
# col 1 = Code commune INSEE, col 2 = Nom commune, col 3 = APL principal,
# col 4 = APL variante, col 5 = APL médecins ≤65 ans (uniquement médecins),
# col 7 = Population.
#
# Sortie : data/processed/drees/apl_medecins.parquet
#   1 ligne / commune × 5 colonnes apl_xxx
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(arrow)
  library(here)
  library(glue)
  library(stringr)
  library(purrr)
})

raw_dir <- here::here("data", "raw", "drees")
out_dir <- here::here("data", "processed", "drees")
out_path <- file.path(out_dir, "apl_medecins.parquet")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Mapping : fichier → nom variable + colonne principale à extraire
pros <- list(
  list(file = "apl_medecins.xlsx",     sheet = "APL 2023", var = "apl_medecins",     col_main = 3, col_jeunes = 5),
  list(file = "apl_infirmieres.xlsx",  sheet = "APL 2023", var = "apl_infirmieres",  col_main = 3, col_jeunes = NA),
  list(file = "apl_sagefemmes.xlsx",   sheet = "APL 2023", var = "apl_sagefemmes",   col_main = 3, col_jeunes = NA),
  list(file = "apl_dentistes.xlsx",    sheet = "APL 2023", var = "apl_dentistes",    col_main = 3, col_jeunes = NA),
  list(file = "apl_kines.xlsx",        sheet = "APL 2023", var = "apl_kines",        col_main = 3, col_jeunes = NA)
)

# Helper : parse un XLSX, garde code + APL principal
parse_apl <- function(spec) {
  path <- file.path(raw_dir, spec$file)
  if (!file.exists(path)) {
    message(glue::glue("  ⚠ {spec$file} manquant"))
    return(NULL)
  }
  message(glue::glue("  → {spec$file}"))
  # Skip = 8 ne marche pas universellement ; on essaie 8 puis 7
  d <- tryCatch(
    suppressMessages(readxl::read_excel(path, sheet = spec$sheet, skip = 8,
                                        col_names = FALSE)),
    error = function(e) NULL
  )
  if (is.null(d) || ncol(d) < spec$col_main) {
    d <- tryCatch(
      suppressMessages(readxl::read_excel(path, sheet = spec$sheet, skip = 7,
                                          col_names = FALSE)),
      error = function(e) NULL
    )
  }
  if (is.null(d)) return(NULL)

  out <- d |>
    dplyr::filter(!is.na(.data[[paste0("...", 1)]]),
                  nchar(.data[[paste0("...", 1)]]) == 5) |>
    dplyr::transmute(
      code_commune = stringr::str_pad(.data[[paste0("...", 1)]], 5, "left", "0"),
      nom_commune  = .data[[paste0("...", 2)]],
      !!spec$var   := suppressWarnings(as.numeric(.data[[paste0("...", spec$col_main)]]))
    )
  if (!is.na(spec$col_jeunes) && spec$col_jeunes <= ncol(d)) {
    out[[paste0(spec$var, "_jeunes")]] <- suppressWarnings(
      as.numeric(d[[paste0("...", spec$col_jeunes)]][
        seq_len(nrow(d))[!is.na(d[[paste0("...", 1)]]) &
                          nchar(d[[paste0("...", 1)]]) == 5]
      ])
    )
  }
  return(out)
}

message("→ Lecture des 5 fichiers APL 2023...")
parsed_list <- purrr::map(pros, parse_apl)

# Réduit par left_join sur code_commune
combined <- purrr::reduce(
  purrr::compact(parsed_list),
  function(a, b) dplyr::left_join(a, b |> dplyr::select(-dplyr::any_of("nom_commune")),
                                  by = "code_commune")
)

arrow::write_parquet(combined, out_path)

cat(glue::glue("

  ✔ APL {nrow(combined)} communes → {out_path}
    ({round(file.size(out_path)/1024)} KB)
    Colonnes : {paste(names(combined), collapse=', ')}

  Désert médical (APL généraliste < 2.5) :
"))
if ("apl_medecins" %in% names(combined)) {
  n_dm <- sum(combined$apl_medecins < 2.5, na.rm = TRUE)
  n_t  <- sum(!is.na(combined$apl_medecins))
  cat(glue::glue("    {n_dm} / {n_t} ({round(100*n_dm/n_t,1)}%)\n"))
}

cat("\n  Distributions par profession :\n")
for (col in grep("^apl_(medecins|infirmieres|sagefemmes|dentistes|kines)$",
                 names(combined), value = TRUE)) {
  s <- summary(combined[[col]])
  cat(glue::glue("    {col} : med={s['Median']} mean={round(s['Mean'],2)}\n"))
}
