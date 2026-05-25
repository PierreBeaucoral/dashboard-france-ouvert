# ============================================================
# scripts/02_elections.R
#
# Étape 2 — Législatives 2024, 2nd tour : résultats par commune
#
# Source : Ministère de l'Intérieur, via data.gouv.fr
#   Dataset : elections-legislatives-des-30-juin-et-7-juillet-2024-resultats-
#             provisoires-du-2nd-tour
#   Ressource : resultats-definitifs-par-commune.csv (~12 MB)
#
# Format source : CSV ; séparateur ";" ; décimales "," ; encodage UTF-8.
# Wide : 1 ligne / commune, avec N blocs de colonnes "candidat N"
# (Nuance, Nom, Prénom, Voix, % Voix, Elu...).
#
# Produit : data/processed/elections/legislatives_2024_t2.parquet
#   1 ligne / commune, avec :
#   - identifiants : code_insee, nom, code_dept, libelle_dept
#   - participation : inscrits, votants, exprimes, abstentions, pct_abstention
#   - résultat : nuance_vainqueur, famille_vainqueur, voix_vainqueur,
#                pct_vainqueur, marge_vainqueur
#   - % par famille politique : pct_NFP, pct_ENS, pct_RN, pct_LR, pct_Divers
#
# Lancement : Rscript scripts/02_elections.R
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(arrow)
  library(here)
  library(curl)
  library(glue)
})

# ---- Paramètres ----

URL_T2_COMMUNE <- paste0(
  "https://static.data.gouv.fr/resources/",
  "elections-legislatives-des-30-juin-et-7-juillet-2024-resultats-",
  "definitifs-du-2nd-tour/20240710-170606/",
  "resultats-definitifs-par-commune.csv"
)

# Mapping nuance INSEE/MI → famille politique du dashboard
#
# Réf : codes "nuances politiques" du Ministère de l'Intérieur, Législatives 2024.
# Nuances effectivement présentes dans les données T2 (par fréquence décroissante) :
#   RN, UG, ENS, LR, UXD, DVD, HOR, DVC, REG, DVG, UDI, EXD, ECO, SOC, DIV, DSV, FI
#
# Choix de regroupement en 5 familles (NFP, ENS, LR, RN, Divers) :
#  - UG = "Union de la Gauche", étiquette officielle NFP au T2     → NFP
#  - SOC, FI : candidats dissidents (rares, qq dizaines au max)    → NFP
#  - ENS : Ensemble (Renaissance + MoDem)                          → ENS
#  - HOR = Horizons (É. Philippe), allié ENS                       → ENS
#  - LR : Les Républicains                                         → LR
#  - UDI : centre-droit, allié LR au niveau national               → LR
#  - UXD = Union de l'Extrême Droite (Ciotti+RN au T1, RN au T2)   → RN
#  - EXD = Extrême Droite hors RN (ex-Reconquête)                  → RN
#  - DVD, DVG, DVC, DSV, REG, ECO, DIV : divers locaux/régionaux   → Divers
nuance_to_famille <- c(
  # NFP / gauche unie
  "UG"  = "NFP",
  "FI"  = "NFP",
  "SOC" = "NFP",
  "COM" = "NFP",
  "VEC" = "NFP",
  # Ensemble (majorité présidentielle + Horizons)
  "ENS" = "ENS",
  "HOR" = "ENS",
  # Les Républicains + UDI
  "LR"  = "LR",
  "UDI" = "LR",
  "UDR" = "LR",
  # RN + alliés + extrême-droite hors RN
  "RN"  = "RN",
  "UXD" = "RN",
  "EXD" = "RN",
  # Divers locaux / régionaux / dissidents
  "DVG" = "Divers",
  "DVD" = "Divers",
  "DVC" = "Divers",
  "DSV" = "Divers",
  "REG" = "Divers",
  "ECO" = "Divers",
  "EXG" = "Divers",
  "DIV" = "Divers"
)

# ---- Chemins ----

raw_path <- here::here("data", "raw", "elections",
                      "resultats-2024-t2-par-commune.csv")
out_dir  <- here::here("data", "processed", "elections")
out_path <- file.path(out_dir, "legislatives_2024_t2.parquet")

dir.create(dirname(raw_path), recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir,           recursive = TRUE, showWarnings = FALSE)

# ---- Téléchargement ----

if (!file.exists(raw_path) || file.size(raw_path) < 5e6) {
  message(glue::glue("Téléchargement : {URL_T2_COMMUNE}"))
  curl::curl_download(URL_T2_COMMUNE, raw_path, quiet = FALSE)
} else {
  message(glue::glue("Cache trouvé : {basename(raw_path)} ({round(file.size(raw_path)/1e6, 1)} MB)"))
}

# ---- Lecture ----

# Encodage UTF-8, décimale ",", séparateur ";", tous en character d'abord
# pour gérer proprement les % et NA, puis on convertit.
df_wide <- readr::read_delim(
  raw_path,
  delim   = ";",
  locale  = readr::locale(
    decimal_mark   = ",",
    grouping_mark  = " ",
    encoding       = "UTF-8"
  ),
  col_types = readr::cols(.default = readr::col_character()),
  na = c("", "NA"),
  trim_ws = TRUE,
  progress = FALSE
)

message(glue::glue("Lu : {nrow(df_wide)} communes, {ncol(df_wide)} colonnes"))

# ---- Identification des blocs candidat ----

all_cols <- names(df_wide)

# Colonnes "fixes" = celles sans suffixe numérique en fin
candidate_cols <- all_cols[stringr::str_detect(all_cols, "\\s\\d+$")]
fixed_cols     <- setdiff(all_cols, candidate_cols)

# Nombres de candidats détectés
max_cand <- max(as.integer(stringr::str_extract(candidate_cols, "\\d+$")),
                na.rm = TRUE)
message(glue::glue("Nombre max de candidats détecté : {max_cand}"))

# ---- Pivot long : 1 ligne / (commune × candidat) ----

# names_pattern capture (variable, numéro) en fin de string
df_long <- df_wide |>
  tidyr::pivot_longer(
    cols          = tidyselect::all_of(candidate_cols),
    names_to      = c(".value", "candidate_num"),
    names_pattern = "^(.*) (\\d+)$"
  ) |>
  # On garde uniquement les lignes où il y a effectivement un candidat
  dplyr::filter(!is.na(`Nuance candidat`) & `Nuance candidat` != "")

# Nettoyer / typer les colonnes utiles
clean_pct <- function(x) {
  # "30,66%" ou "30,66" → 30.66 ; NA si vide
  x <- stringr::str_remove(x, "%")
  x <- stringr::str_replace(x, ",", ".")
  x <- stringr::str_trim(x)
  suppressWarnings(as.numeric(x))
}
clean_int <- function(x) {
  x <- stringr::str_remove_all(x, "\\s| ")  # espaces fines de milliers
  suppressWarnings(as.integer(x))
}

df_long <- df_long |>
  dplyr::transmute(
    code_insee      = `Code commune`,
    nom_commune     = `Libellé commune`,
    code_dept       = `Code département`,
    libelle_dept    = `Libellé département`,
    inscrits        = clean_int(Inscrits),
    votants         = clean_int(Votants),
    exprimes        = clean_int(`Exprimés`),
    abstentions     = clean_int(Abstentions),
    pct_abstention  = clean_pct(`% Abstentions`),
    blancs          = clean_int(Blancs),
    nuls            = clean_int(Nuls),
    candidate_num   = as.integer(candidate_num),
    nuance          = `Nuance candidat`,
    voix            = clean_int(Voix),
    pct_voix_exprimes = clean_pct(`% Voix/exprimés`),
    elu             = !is.na(Elu) & stringr::str_detect(tolower(Elu), "élu|elu")
  )

# Famille politique
df_long <- df_long |>
  dplyr::mutate(
    famille = dplyr::coalesce(unname(nuance_to_famille[nuance]), "Divers")
  )

# ---- Agrégation par commune ----

# 1) Pour chaque commune, identifier le vainqueur (max voix) et le second
ranked <- df_long |>
  dplyr::group_by(code_insee) |>
  dplyr::arrange(dplyr::desc(voix), .by_group = TRUE) |>
  dplyr::mutate(rang = dplyr::row_number()) |>
  dplyr::ungroup()

vainqueur <- ranked |>
  dplyr::filter(rang == 1L) |>
  dplyr::transmute(
    code_insee,
    nuance_vainqueur  = nuance,
    famille_vainqueur = famille,
    voix_vainqueur    = voix,
    pct_vainqueur     = pct_voix_exprimes
  )

second <- ranked |>
  dplyr::filter(rang == 2L) |>
  dplyr::transmute(
    code_insee,
    voix_second   = voix,
    pct_second    = pct_voix_exprimes
  )

# 2) % par famille politique (sommer les % des candidats de chaque famille
#    présents dans la commune ; NA si la famille n'est pas représentée)
pct_par_famille <- df_long |>
  dplyr::group_by(code_insee, famille) |>
  dplyr::summarise(pct = sum(pct_voix_exprimes, na.rm = TRUE),
                   .groups = "drop") |>
  tidyr::pivot_wider(
    names_from   = famille,
    values_from  = pct,
    names_prefix = "pct_"
  )

# 3) Informations fixes par commune
fixes <- df_long |>
  dplyr::distinct(code_insee, nom_commune, code_dept, libelle_dept,
                  inscrits, votants, exprimes, abstentions, pct_abstention)

# 4) Assembler
out <- fixes |>
  dplyr::left_join(vainqueur,         by = "code_insee") |>
  dplyr::left_join(second,            by = "code_insee") |>
  dplyr::left_join(pct_par_famille,   by = "code_insee") |>
  dplyr::mutate(
    marge_vainqueur = pct_vainqueur - dplyr::coalesce(pct_second, 0)
  )

# ---- Écriture ----

arrow::write_parquet(out, out_path)

# ---- Rapport ----

n_communes <- nrow(out)
n_familles <- out |> dplyr::count(famille_vainqueur)

cat(glue::glue("

  ✔ {n_communes} communes traitées → {out_path}
    ({round(file.size(out_path)/1024)} KB)

  Vainqueurs par famille politique :
"))
print(n_familles)
cat("\n")

# Aperçu schéma
cat("Aperçu :\n")
print(dplyr::glimpse(out))
