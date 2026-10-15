# ============================================================
# scripts/10_filosofi_densite_defm.R
#
# Bloc 1 (compléments) + Bloc 3 — INSEE FiLoSoFi + grille densité +
# DARES DEFM (chômage par commune).
#
# Sources (toutes découvertes via MCP datagouv) :
#   - FiLoSoFi 2021 (revenus) : fichier officiel INSEE, TÉLÉCHARGÉ par ce script
#     (https://www.insee.fr/fr/statistiques/7756855, csv communal)
#   - Catégories d'espace 2024 (grille de densité INSEE croisée aux aires d'attraction) :
#     jeu publié par l'INSEE sur data.gouv.fr, TÉLÉCHARGÉ par ce script
#     (https://www.data.gouv.fr/datasets/nomenclature-communale-des-categories-despace-2024)
#   - DEFM communales DARES (« Inscrits à France Travail, données communales
#     trimestrielles, brutes ») : jeu publié par la DARES sur data.gouv.fr, TÉLÉCHARGÉ
#     par ce script (https://www.data.gouv.fr/datasets/66df098924d76afbdd70938a).
#     Le jeu est mis à jour chaque trimestre : un nouveau trimestre change defm_taux_pop.
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
# Source OFFICIELLE : INSEE, « Structure et distribution des revenus, inégalité des
# niveaux de vie en 2021 » (https://www.insee.fr/fr/statistiques/7756855), fichiers
# communaux au COG 2023 : FILO2021_DISP_COM.csv (revenu disponible) et
# FILO2021_DEC_COM.csv (revenu déclaré : part de ménages imposés). Séparateur ";".
# Valeurs « secret statistique » : cellule vide (NA). Voir meta_FILO2021_*_COM.csv.
FILO_URL <- "https://www.insee.fr/fr/statistiques/fichier/7756855/indic-struct-distrib-revenu-2021-COMMUNES_csv.zip"
filo_raw_dir  <- here::here("data", "raw", "filosofi")
filo_disp_in  <- file.path(filo_raw_dir, "FILO2021_DISP_COM.csv")
filo_dec_in   <- file.path(filo_raw_dir, "FILO2021_DEC_COM.csv")
filo_path_out <- here::here("data", "processed", "filosofi", "revenus_communes.parquet")
dir.create(filo_raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(filo_path_out), recursive = TRUE, showWarnings = FALSE)

if (!file.exists(filo_disp_in) || !file.exists(filo_dec_in)) {
  message("  Télécharge : ", FILO_URL)
  zp <- file.path(filo_raw_dir, "filosofi_2021_communes_csv.zip")
  options(timeout = 900)
  utils::download.file(FILO_URL, zp, mode = "wb", quiet = TRUE,
                       headers = c("User-Agent" = "Mozilla/5.0"))
  utils::unzip(zp, files = c("FILO2021_DISP_COM.csv", "FILO2021_DEC_COM.csv"), exdir = filo_raw_dir)
}

rd_filo <- function(f, cols) readr::read_delim(f, delim = ";", show_col_types = FALSE,
  col_types = readr::cols(.default = "c"), col_select = dplyr::all_of(c("CODGEO", cols)))
num <- function(x) suppressWarnings(as.numeric(sub(",", ".", x, fixed = TRUE)))   # décimale virgule ; "s" (secret) -> NA
disp <- rd_filo(filo_disp_in, c("Q221", "D121", "D921", "GI21", "S80S2021", "PACT21", "PPEN21", "PPSOC21"))
dec  <- rd_filo(filo_dec_in,  "PMIMP21")
pop_names <- arrow::read_parquet(here::here("data", "processed", "insee", "populations.parquet"),
                                 col_select = c("code_commune", "nom_commune")) |> dplyr::distinct(code_commune, .keep_all = TRUE)

filo <- disp |>
  dplyr::left_join(dec, by = "CODGEO") |>
  dplyr::transmute(
    code_commune     = stringr::str_pad(CODGEO, 5, "left", "0"),
    revenu_median    = num(Q221),
    revenu_d1        = num(D121),
    revenu_d9        = num(D921),
    gini             = num(GI21),
    s80_s20          = num(S80S2021),
    pct_imposes      = num(PMIMP21),
    part_pensions    = num(PPEN21),
    part_activite    = num(PACT21),
    part_prestations = num(PPSOC21)
  ) |>
  dplyr::left_join(pop_names, by = "code_commune") |>
  dplyr::select(code_commune, nom_commune, dplyr::everything())

arrow::write_parquet(filo, filo_path_out)
message(glue::glue("  ✔ {nrow(filo)} communes ({sum(!is.na(filo$revenu_median))} avec revenu médian)"))

# ============================================================
# 2. Grille densité INSEE 2024
# ============================================================

message("→ Grille densité INSEE 2024...")
dens_path_in  <- here::here("data", "raw", "insee_densite", "categories_espace_2024.parquet")
dens_path_out <- here::here("data", "processed", "insee_densite", "grille_densite.parquet")
dir.create(dirname(dens_path_out), recursive = TRUE, showWarnings = FALSE)

# Ressource stable data.gouv.fr (redirige vers le fichier courant du producteur).
DENS_URL <- "https://www.data.gouv.fr/api/1/datasets/r/8229e391-95c3-4e50-84bc-a30cfd4232b6"
if (!file.exists(dens_path_in)) {
  dir.create(dirname(dens_path_in), recursive = TRUE, showWarnings = FALSE)
  message("  Télécharge : ", DENS_URL)
  options(timeout = 900)
  utils::download.file(DENS_URL, dens_path_in, mode = "wb", quiet = TRUE)
}
if (file.exists(dens_path_in)) {
  dens_raw <- arrow::read_parquet(dens_path_in)
  dens <- dens_raw |>
    dplyr::transmute(
      code_commune  = stringr::str_pad(CODGEO, 5, "left", "0"),
      nom_commune   = LIBGEO,
      code_dept     = DEP,
      code_region   = REG,
      densite_insee = CAT_ESPACE,
      # Rang ordinal 1 (rural non périurbain) -> 6 (grands centres urbains).
      # Lookup explicite sur les 6 libellés INSEE (l'ancien cascade regex
      # classait "Rural périurbain" comme "urbain" et "Petites villes" = 7).
      # Ordre des classes intermédiaires = médiane croissante de la population
      # (Ceintures < Petites villes), vérifié dans scripts/99_validate_data.R.
      densite_rang = unname(c(
        "Rural non périurbain"           = 1L,
        "Rural périurbain"               = 2L,
        "Ceintures urbaines"             = 3L,
        "Petites villes"                 = 4L,
        "Centres urbains intermédiaires" = 5L,
        "Grands centres urbains"         = 6L
      )[CAT_ESPACE])
    )

  arrow::write_parquet(dens, dens_path_out)
  message(glue::glue("  ✔ {nrow(dens)} communes catégorisées"))
  stopifnot(!anyNA(dens$densite_rang))  # libellé INSEE inattendu => échec
  print(dens |> dplyr::count(densite_insee, densite_rang))
} else {
  message("  ⚠ raw densité absent (", dens_path_in, ") : parquet existant conservé")
  dens <- arrow::read_parquet(dens_path_out)
}

# ============================================================
# 3. DEFM DARES (chômage)
# ============================================================

message("→ DEFM DARES (≈2,09 M lignes en long format)...")
defm_path_in  <- here::here("data", "raw", "dares", "defm_communales.csv")
defm_path_out <- here::here("data", "processed", "dares", "defm_commune.parquet")
dir.create(dirname(defm_path_out), recursive = TRUE, showWarnings = FALSE)

DEFM_URL <- "https://www.data.gouv.fr/api/1/datasets/r/c3de380a-3829-4e04-aca4-19e685f9da1a"
if (!file.exists(defm_path_in)) {
  dir.create(dirname(defm_path_in), recursive = TRUE, showWarnings = FALSE)
  message("  Télécharge : ", DEFM_URL)
  options(timeout = 1800)
  utils::download.file(DEFM_URL, defm_path_in, mode = "wb", quiet = TRUE)
}
if (file.exists(defm_path_in)) {
  # Lecture stream (séparateur ; supposé — vérifier)
  defm_raw <- readr::read_delim(defm_path_in, delim = ";",
                                show_col_types = FALSE,
                                locale = readr::locale(encoding = "UTF-8"))
  message(glue::glue("  {nrow(defm_raw)} lignes brutes, dernière date : ",
                     max(defm_raw$Date, na.rm = TRUE)))

  # Garder le dernier trimestre, catégorie ABC, sexe Total, âge Total
  latest_q <- max(defm_raw$Date, na.rm = TRUE)
  defm_q <- defm_raw |>
    dplyr::filter(Date == latest_q,
                  Catégorie == "ABC",
                  Sexe == "Total",
                  `Tranche d'âge` == "Total")
  # Lignes agrégées ("Total France", "Total France métropolitaine", "Non
  # localisés…") et lignes ville PLM (= somme de leurs arrondissements) écartées :
  # 05_merge reconstruit la ville depuis les arrondissements.
  total_fr <- defm_q$`Nombre de demandeurs d'emploi`[defm_q$Commune == "Total France"]
  defm <- defm_q |>
    dplyr::filter(grepl("^[0-9][0-9AB][0-9]{3}$", `Code commune`),
                  !`Code commune` %in% c("75056", "69123", "13055")) |>
    dplyr::transmute(
      code_commune = stringr::str_pad(`Code commune`, 5, "left", "0"),
      nom_commune  = Commune,
      code_dept    = `Code département`,
      trimestre    = Date,
      defm_abc     = `Nombre de demandeurs d'emploi`
    )
  # Contrôle même périmètre : somme des communes vs ligne "Total France" du
  # même fichier (écart attendu < 0,5 %, arrondi des effectifs à 5).
  # 2024-T4 : 5 563 980 vs 5 565 440. Contrôle externe fait le 2026-10-15 contre
  # les notes DREETS 2024-T4 (Marne, Moselle, Hauts-de-France, BFC : ±0,5 %) :
  #   https://grand-est.dreets.gouv.fr/sites/grand-est.dreets.gouv.fr/IMG/pdf/defm_moselle_2024t4.pdf
  #   https://grand-est.dreets.gouv.fr/sites/grand-est.dreets.gouv.fr/IMG/pdf/defm_marne_2024t4.pdf
  #   https://hauts-de-france.dreets.gouv.fr/sites/hauts-de-france.dreets.gouv.fr/IMG/pdf/la_demande_d_emploi_dans_la_region_hauts-de-france_au_4e_trimestre_2024.pdf
  stopifnot(!anyDuplicated(defm$code_commune), length(total_fr) == 1,
            abs(sum(defm$defm_abc) / total_fr - 1) < 0.005)
  # Effectifs -> COG 2026 (fusions depuis 2024 ; règle de scripts/cog_helpers.R)
  source(here::here("scripts", "cog_helpers.R"))
  cmap <- cog_map("2024-01-01")
  defm <- defm |>
    dplyr::mutate(code_commune = to_cog2026(code_commune, cmap)) |>
    dplyr::group_by(code_commune) |>
    dplyr::summarise(nom_commune = dplyr::first(nom_commune),
                     code_dept   = dplyr::first(code_dept),
                     trimestre   = dplyr::first(trimestre),
                     defm_abc    = sum(defm_abc), .groups = "drop")

  arrow::write_parquet(defm, defm_path_out)
  message(glue::glue("  ✔ {nrow(defm)} communes pour le trimestre {latest_q}"))
} else {
  message("  ⚠ raw DEFM absent (", defm_path_in, ") : parquet existant conservé")
  defm <- arrow::read_parquet(defm_path_out)
  latest_q <- max(defm$trimestre, na.rm = TRUE)
}

cat(glue::glue("

  RAPPORT
  =======
  FiLoSoFi 2021 :
    Revenu médian — Min : {min(filo$revenu_median, na.rm=TRUE)}€
    Médiane nationale (de la médiane commune) : {median(filo$revenu_median, na.rm=TRUE)}€
    Max : {max(filo$revenu_median, na.rm=TRUE)}€

  Densité INSEE :
    {nrow(dens)} communes catégorisées (6 classes)

  DEFM {latest_q} :
    {nrow(defm)} communes
    Total demandeurs catégorie ABC : {format(sum(defm$defm_abc, na.rm=TRUE), big.mark=' ')}
"))
