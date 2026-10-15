# ============================================================
# R/site_numbers.R
# SOURCE UNIQUE des chiffres de tête (communes, indicateurs, paires).
# Calculés à chaque rendu depuis data/processed/ : aucun nombre n'est écrit
# à la main dans les pages. Usage : source(here::here("R", "site_numbers.R"))
# puis `r site$communes` dans le texte.
# Définitions (à garder stables) :
#   communes    = lignes de la table croisée commune_merged (une par commune)
#   indicateurs = lignes de indicator_meta.parquet (= variables de la matrice Spearman)
#   paires      = C(indicateurs, 2)
#   thematiques = pages cartographiques listées dans site$themes
# ============================================================

suppressPackageStartupMessages({ library(arrow); library(here) })

site_fmt <- function(x) format(x, big.mark = " ", scientific = FALSE, trim = TRUE)

.merged_path <- here::here("data", "processed", "explorer", "commune_merged.parquet")
.corr_path   <- here::here("data", "processed", "explorer", "correlations.parquet")
.meta_path   <- here::here("data", "processed", "explorer", "indicator_meta.parquet")

# Registre des jeux de données (SOURCE UNIQUE du « nombre de sources »).
# Un jeu = une publication d'un producteur avec son millésime ; Filosofi 2021 et
# 2023 comptent à part (méthodes différentes). Ajouter une ligne = +1 partout.
SOURCES <- c(
  "Législatives 2024", "Municipales 2026", "Élections agrégées (participation 2002-2026)",
  "DVF 2021-2025", "Atmo air", "SSMSI délinquance", "BAAC accidents", "DGFiP comptes communaux",
  "DREES APL", "INSEE espérance de vie", "INSEE FiLoSoFi 2021", "INSEE FiLoSoFi 2023",
  "INSEE populations", "INSEE grille de densité", "DARES DEFM", "RARE / CITEPA carbone",
  "INSEE MOBPRO 2022", "Agence Bio", "INSEE RP 2022 logement et emploi", "INSEE BPE 2025",
  "CNAF allocataires", "ARCEP fibre", "GASPAR CatNat et PPRN", "Cerema ENAF", "ADEME DPE",
  "Agence ORE électricité et gaz"
)

site <- local({
  n_com <- if (file.exists(.merged_path)) {
    nrow(arrow::read_parquet(.merged_path, col_select = "code_commune"))
  } else NA_integer_
  # Indicateurs = lignes du registre explorer/indicator_meta.parquet, vérifié contre la
  # matrice de corrélations (Explorer) : les deux doivent coïncider, sinon NA visible.
  n_ind <- if (file.exists(.meta_path) && file.exists(.corr_path)) {
    mv <- arrow::read_parquet(.meta_path)$var
    cr <- arrow::read_parquet(.corr_path)
    cr <- cr[cr$method == "spearman" & cr$strate == "Toutes", ]
    if (setequal(mv, unique(c(cr$v1, cr$v2)))) length(mv) else NA_integer_
  } else NA_integer_
  themes <- c("elections", "municipales", "immobilier", "air", "securite", "finances",
              "sante", "revenus", "demographie", "energie", "mobilite", "agriculture",
              "logement", "risques", "services")
  n_scr <- length(list.files(here::here("scripts"), pattern = "^[0-9]{2}_.*\\.R$"))
  # Millésime APL lu dans les données (08_), jamais figé dans les libellés
  apl_path <- here::here("data", "processed", "drees", "apl_medecins.parquet")
  apl_mil <- if (file.exists(apl_path))
    max(arrow::read_parquet(apl_path, col_select = "apl_millesime")$apl_millesime, na.rm = TRUE) else NA_integer_
  list(
    apl         = apl_mil,
    communes    = site_fmt(n_com),
    indicateurs = n_ind,
    paires      = site_fmt(if (is.na(n_ind)) NA else choose(n_ind, 2)),
    thematiques = length(themes),
    themes      = themes,
    sources     = length(SOURCES),
    scripts     = n_scr
  )
})
