# ============================================================
# scripts/28_filosofi_2023.R
#
# INSEE Filosofi 2023 ("Filosofi 2", nouvelle méthodologie) — niveau de vie
# médian et taux de pauvreté par commune.
#
# DÉCISION (Pierre, 2026-10-14) : on GARDE Filosofi 2021 (script 10, source de
# la composition du revenu) et on AJOUTE 2023 comme NIVEAU courant. Rupture de
# série : INSEE déclare toute comparaison Filosofi (2012-2021) / Filosofi 2
# (2023) "à proscrire". AUCUNE variation 2021->2023 n'est calculée ni affichée.
# Le seul croisement 2021/2023 ci-dessous est un contrôle de validité (corrélation
# de rang), jamais exporté.
#
# Source : https://www.insee.fr/fr/statistiques/8984752  (Insee Résultats,
#   paru le 06/08/2026 ; fichier FILOSOFI_CC_csv.zip, géographie au 01/01/2026)
# Licence : Licence Ouverte Etalab 2.0 (conditions de réutilisation des
#   informations publiques de l'Insee).
# Champ : France métropolitaine + La Réunion. Aucune statistique pour
#   971/972/973/976. Sous le niveau département, SEULS deux indicateurs sont
#   diffusés : MED_SL (niveau de vie médian, EUR/UC) et PR_MD60 (taux de pauvreté
#   à 60 %). D1, D9, D9/D1, Gini, parts de revenus et nombre de ménages ne sont
#   publiés qu'à partir du département => absents ici (pas de colonnes vides).
# Secret statistique : OBS_VALUE vide (CONF_STATUS = "C" ou non diffusé) -> NA.
#
# Sortie : data/processed/filosofi_2023/filosofi_2023_communes.parquet
# ============================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(arrow)
  library(here); library(glue); library(sf)
})

url_zip  <- "https://www.insee.fr/fr/statistiques/fichier/8984752/FILOSOFI_CC_csv.zip"
raw_dir  <- here::here("data", "raw", "filosofi_2023")
csv_in   <- file.path(raw_dir, "DS_FILOSOFI_CC_2023_data.csv")
out_path <- here::here("data", "processed", "filosofi_2023", "filosofi_2023_communes.parquet")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

if (!file.exists(csv_in)) {
  zp <- file.path(raw_dir, "cc.zip")
  options(timeout = 600)
  download.file(url_zip, zp, mode = "wb", quiet = TRUE)
  unzip(zp, exdir = raw_dir)
}

raw <- readr::read_delim(csv_in, delim = ";", show_col_types = FALSE,
                         col_types = readr::cols(.default = "c", OBS_VALUE = "d"))

# COM = communes (Paris/Lyon/Marseille en commune entière 75056/69123/13055 ;
# les arrondissements sont un autre GEO_OBJECT = "ARM", non utilisé ici).
com <- raw |>
  filter(GEO_OBJECT == "COM", FILOSOFI_MEASURE %in% c("MED_SL", "PR_MD60")) |>
  select(code_commune = GEO, mesure = FILOSOFI_MEASURE, valeur = OBS_VALUE) |>
  pivot_wider(names_from = mesure, values_from = valeur) |>
  transmute(
    code_commune,
    rev_median_2023     = MED_SL,    # EUR / UC / an, niveau de vie médian
    taux_pauvrete_2023  = PR_MD60,   # %, seuil 60 % de la médiane
    # Périmètre élargi : INSEE agrège ces communes absorbées dans la commune
    # hôte (Celles, Chalinargues, Chavagnac, Sainte-Anastasie -> 15141 ;
    # L'Oie, Sainte-Florence -> 85084 ; Suzan -> 09042).
    perimetre_elargi_2023 = code_commune %in% c("15141", "85084", "09042")
  )

# ---- Harmonisation COG 2026 ----
# Le fichier est DÉJÀ au COG 2026 : aucune fusion à rejouer, aucune médiane
# moyennée. On vérifie simplement que chaque code existe au COG 2026 et on
# compte les communes COG 2026 sans ligne (absorbées par l'enclave/fusion INSEE).
cog_path <- file.path(raw_dir, "v_commune_2026.csv")   # même source que le script 06
if (!file.exists(cog_path))
  download.file("https://www.insee.fr/fr/statistiques/fichier/8740222/v_commune_2026.csv",
                cog_path, mode = "wb", quiet = TRUE)
cog <- readr::read_csv(cog_path, show_col_types = FALSE, col_types = readr::cols(.default = "c")) |>
  filter(TYPECOM == "COM")                              # communes (ni COMD ni ARM)
hors_cog <- setdiff(com$code_commune, cog$COM)
absentes <- setdiff(cog$COM, com$code_commune)
message(glue("COG 2026 : {nrow(cog)} communes ; fichier : {nrow(com)} ; codes hors COG : {length(hors_cog)} ; ",
             "communes COG sans ligne : {length(absentes)}"))
# Les 4+2+1 communes absorbées (15031...) figurent dans le fichier avec valeur vide -> NA.
message("Communes à fusion post-fichier mises à NA : 0 (fichier déjà COG 2026) ; ",
        "dont 7 absorbées par INSEE (valeur vide) ; périmètre élargi signalé sur ",
        sum(com$perimetre_elargi_2023), " communes hôtes")

arrow::write_parquet(com, out_path)

# ---- Couverture ----
dep <- substr(com$code_commune, 1, 3)
zone <- case_when(dep %in% c("971","972","973","976") ~ "DROM non diffusé",
                  dep == "974" ~ "La Réunion", TRUE ~ "Métropole")
message("\nCouverture (publié = non-NA) :")
print(com |> mutate(zone = zone) |> group_by(zone) |>
        summarise(n = n(), med_publie = sum(!is.na(rev_median_2023)),
                  med_secret = sum(is.na(rev_median_2023)),
                  pauv_publie = sum(!is.na(taux_pauvrete_2023)),
                  pauv_secret = sum(is.na(taux_pauvrete_2023)), .groups = "drop"))
print(summary(com$rev_median_2023)); print(summary(com$taux_pauvrete_2023))

# ---- Contrôle de validité 2021 vs 2023 (NON exporté, NON affiché) ----
f21 <- arrow::read_parquet(here::here("data", "processed", "filosofi", "revenus_communes.parquet")) |>
  select(code_commune, revenu_median)
cc <- inner_join(com, f21, by = "code_commune") |> filter(!is.na(rev_median_2023), !is.na(revenu_median))
rho <- cor(cc$rev_median_2023, cc$revenu_median, method = "spearman")
message(glue("\nContrôle de validité (pas pour affichage) : Spearman médiane 2021 vs 2023 = ",
             "{round(rho, 4)} sur {nrow(cc)} communes communes"))

# ---- Checks ----
stopifnot(
  !anyDuplicated(com$code_commune),
  nrow(com) == 34768,                                   # COM publiés dans FILOSOFI_CC 2023
  all(com$rev_median_2023 >= 10000 & com$rev_median_2023 <= 70000, na.rm = TRUE),
  all(com$taux_pauvrete_2023 >= 0 & com$taux_pauvrete_2023 <= 100, na.rm = TRUE),
  !any(com$rev_median_2023 == 0, na.rm = TRUE),         # secret = NA, jamais 0
  length(hors_cog) == 0,
  rho > 0.9
)
message("OK : tous les contrôles passent.")
