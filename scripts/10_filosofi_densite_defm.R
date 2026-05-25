# ============================================================
# scripts/10_filosofi_densite_defm.R
#
# Bloc 1 (compléments) + Bloc 3 — INSEE FiLoSoFi + grille densité +
# DARES DEFM (chômage par commune).
#
# Sources (toutes découvertes via MCP datagouv) :
#   - FiLoSoFi 2021 (revenus) : Geoptis mirror
#   - Grille densité 2024 INSEE : icem7 mirror
#   - DEFM communales DARES : data.dares.travail-emploi.gouv.fr
#
# Acronymes (à afficher dans le dashboard) :
#   - FiLoSoFi : Fichier Localisé Social et Fiscal (INSEE)
#   - INSEE   : Institut National de la Statistique et des Études Économiques
#   - DARES   : Direction de l'Animation de la Recherche, des Études et des
#               Statistiques (Ministère du Travail)
#   - DEFM    : Demandeurs d'Emploi en Fin de Mois
#   - Catégorie ABC : tous les demandeurs (sans / avec activité réduite)
#
# Sorties :
#   - data/processed/filosofi/revenus_communes.parquet
#   - data/processed/insee_densite/grille_densite.parquet
#   - data/processed/dares/defm_commune.parquet
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(arrow)
  library(here)
  library(stringr)
  library(glue)
  library(janitor)
})

# ============================================================
# 1. FiLoSoFi — revenus
# ============================================================

message("→ FiLoSoFi 2021 (revenus)...")
filo_path_in  <- here::here("data", "raw", "filosofi", "revenus_communes_2021.csv")
filo_path_out <- here::here("data", "processed", "filosofi", "revenus_communes.parquet")
dir.create(dirname(filo_path_out), recursive = TRUE, showWarnings = FALSE)

filo_raw <- readr::read_delim(filo_path_in,
                              delim = ";",
                              show_col_types = FALSE,
                              locale = readr::locale(decimal_mark = ".",
                                                     encoding = "UTF-8"))

# Identifier les colonnes utiles via match partiel (les noms ont des
# caractères spéciaux : "[DISP]", "ᵉʳ", etc.)
col_code <- grep("Code g", names(filo_raw), value = TRUE)[1]
col_nom  <- grep("Lib", names(filo_raw), value = TRUE)[1]
col_med_disp <- grep("DISP.*M.diane", names(filo_raw), value = TRUE)[1]
col_gini_disp <- grep("DISP.*Gini",  names(filo_raw), value = TRUE)[1]
col_s80_disp  <- grep("DISP.*S80",   names(filo_raw), value = TRUE)[1]
col_d1   <- grep("DISP.*1.*d.cile", names(filo_raw), value = TRUE)[1]
col_d9   <- grep("DISP.*9.*d.cile", names(filo_raw), value = TRUE)[1]
col_imp  <- grep("DEC.*imp", names(filo_raw), value = TRUE)[1]
col_pens <- grep("DISP.*Part des pensions", names(filo_raw), value = TRUE)[1]
col_act  <- grep("DISP.*Part des revenus d.activit", names(filo_raw), value = TRUE)[1]
col_soc  <- grep("DISP.*Part de l.ensemble des prestations", names(filo_raw), value = TRUE)[1]

filo <- filo_raw |>
  dplyr::transmute(
    code_commune    = stringr::str_pad(.data[[col_code]], 5, "left", "0"),
    nom_commune     = .data[[col_nom]],
    revenu_median   = suppressWarnings(as.numeric(.data[[col_med_disp]])),
    revenu_d1       = suppressWarnings(as.numeric(.data[[col_d1]])),
    revenu_d9       = suppressWarnings(as.numeric(.data[[col_d9]])),
    gini            = suppressWarnings(as.numeric(.data[[col_gini_disp]])),
    s80_s20         = suppressWarnings(as.numeric(.data[[col_s80_disp]])),
    pct_imposes     = suppressWarnings(as.numeric(.data[[col_imp]])),
    part_pensions   = suppressWarnings(as.numeric(.data[[col_pens]])),
    part_activite   = suppressWarnings(as.numeric(.data[[col_act]])),
    part_prestations = suppressWarnings(as.numeric(.data[[col_soc]]))
  )

arrow::write_parquet(filo, filo_path_out)
message(glue::glue("  ✔ {nrow(filo)} communes ({sum(!is.na(filo$revenu_median))} avec revenu médian)"))

# ============================================================
# 2. Grille densité INSEE 2024
# ============================================================

message("→ Grille densité INSEE 2024...")
dens_path_in  <- here::here("data", "raw", "insee_densite", "categories_espace_2024.parquet")
dens_path_out <- here::here("data", "processed", "insee_densite", "grille_densite.parquet")
dir.create(dirname(dens_path_out), recursive = TRUE, showWarnings = FALSE)

dens_raw <- arrow::read_parquet(dens_path_in)
dens <- dens_raw |>
  dplyr::transmute(
    code_commune  = stringr::str_pad(CODGEO, 5, "left", "0"),
    nom_commune   = LIBGEO,
    code_dept     = DEP,
    code_region   = REG,
    densite_insee = CAT_ESPACE,
    # Code numérique 1-7 pour ordre dans la légende
    densite_rang = dplyr::case_when(
      stringr::str_detect(CAT_ESPACE, "(?i)centre.*urbain") ~ 1L,
      stringr::str_detect(CAT_ESPACE, "(?i)urbain.*dense")  ~ 2L,
      stringr::str_detect(CAT_ESPACE, "(?i)urbain")         ~ 3L,
      stringr::str_detect(CAT_ESPACE, "(?i)p.riurbain.*dense") ~ 4L,
      stringr::str_detect(CAT_ESPACE, "(?i)p.riurbain")    ~ 5L,
      stringr::str_detect(CAT_ESPACE, "(?i)rural.*p.riurbain") ~ 5L,
      stringr::str_detect(CAT_ESPACE, "(?i)rural")          ~ 6L,
      TRUE                                                   ~ 7L
    )
  )

arrow::write_parquet(dens, dens_path_out)
message(glue::glue("  ✔ {nrow(dens)} communes catégorisées"))
print(dens |> dplyr::count(densite_insee))

# ============================================================
# 3. DEFM DARES (chômage)
# ============================================================

message("→ DEFM DARES (1.08M lignes en long format)...")
defm_path_in  <- here::here("data", "raw", "dares", "defm_communales.csv")
defm_path_out <- here::here("data", "processed", "dares", "defm_commune.parquet")
dir.create(dirname(defm_path_out), recursive = TRUE, showWarnings = FALSE)

# Lecture stream (séparateur ; supposé — vérifier)
defm_raw <- readr::read_delim(defm_path_in, delim = ";",
                              show_col_types = FALSE,
                              locale = readr::locale(encoding = "UTF-8"))
message(glue::glue("  {nrow(defm_raw)} lignes brutes, dernière date : ",
                   max(defm_raw$Date, na.rm = TRUE)))

# Garder le dernier trimestre, catégorie ABC, sexe Total, âge Total
latest_q <- max(defm_raw$Date, na.rm = TRUE)
defm <- defm_raw |>
  dplyr::filter(Date == latest_q,
                Catégorie == "ABC",
                Sexe == "Total",
                `Tranche d'âge` == "Total") |>
  dplyr::transmute(
    code_commune = stringr::str_pad(`Code commune`, 5, "left", "0"),
    nom_commune  = Commune,
    code_dept    = `Code département`,
    trimestre    = Date,
    defm_abc     = `Nombre de demandeurs d'emploi`
  )

arrow::write_parquet(defm, defm_path_out)
message(glue::glue("  ✔ {nrow(defm)} communes pour le trimestre {latest_q}"))

cat(glue::glue("

  RAPPORT
  =======
  FiLoSoFi 2021 :
    Revenu médian — Min : {min(filo$revenu_median, na.rm=TRUE)}€
    Médiane nationale (de la médiane commune) : {median(filo$revenu_median, na.rm=TRUE)}€
    Max : {max(filo$revenu_median, na.rm=TRUE)}€

  Densité INSEE :
    {nrow(dens)} communes catégorisées (7 classes possibles)

  DEFM {latest_q} :
    {nrow(defm)} communes
    Total demandeurs catégorie ABC : {format(sum(defm$defm_abc, na.rm=TRUE), big.mark=' ')}
"))
