# ============================================================
# scripts/06_insee_populations.R
#
# Bloc 1 (partiel) — Données INSEE socio-démographiques.
#
# Cette session livre :
#   1. COG 2026 — référentiel des codes communaux 2026, pour corriger
#      les ~950 trous de vintage (communes du geo absents des données
#      MI/DVF/ATMO parce que fusionnées entre 2023 et 2026).
#   2. Populations légales 2017-2021 — parquet d'icem7 (mirror INSEE).
#      Permet la normalisation par 1 000 hab. et une stratification par
#      taille réelle (pas le proxy d'inscrits).
#
# À faire en session suivante (MCP datagouv actif pour discovery) :
#   - FiLoSoFi récent (revenu médian + taux pauvreté par commune)
#   - Grille communale de densité INSEE (4 ou 7 classes)
#   - SSMSI délinquance par commune
#   - BAAC accidents
#   - APL médecins DREES
#   - Enedis consommation électrique
#   - GRDF gaz
#   - ADEME bilan carbone
#   - Etc.
#
# Sortie : data/processed/insee/populations.parquet
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(here)
  library(curl)
  library(readr)
  library(glue)
})

# ---- Sources ----

URL_COG_COMMUNES <- "https://www.insee.fr/fr/statistiques/fichier/8740222/v_commune_2026.csv"
URL_COG_MVTS     <- "https://www.insee.fr/fr/statistiques/fichier/8740222/v_mvt_commune_2026.csv"
URL_POP          <- "https://static.data.gouv.fr/resources/populations-legales-communales-2017-2021/20240122-151058/poplegales2017-2021.parquet"
# Populations légales 2023 (INSEE, géographie au 1er janvier 2025 : contient les communes
# rétablies de 2024-2025). Page : https://www.insee.fr/fr/statistiques/8680726
URL_POP23        <- "https://www.insee.fr/fr/statistiques/fichier/8680726/ensemble.zip"
# Mayotte : INSEE Focus n° 105, « 256 500 habitants à Mayotte en 2017 » (recensement 2017,
# population municipale par commune, feuille « Figure 2 », total 256 518).
# Page : https://www.insee.fr/fr/statistiques/3286558
URL_MAYOTTE      <- "https://www.insee.fr/fr/statistiques/fichier/3286558/if105.xls"

# ---- Chemins ----

raw_dir <- here::here("data", "raw", "insee")
out_dir <- here::here("data", "processed", "insee")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cog_path  <- file.path(raw_dir, "v_commune_2026.csv")
mvt_path  <- file.path(raw_dir, "v_mvt_commune_2026.csv")
pop_path  <- file.path(raw_dir, "populations_2017_2021.parquet")
pop23_zip <- file.path(raw_dir, "popleg2023_ensemble.zip")
mayotte_path <- file.path(raw_dir, "if105_mayotte_2017.xls")

# ---- Téléchargement ----

dl_if_missing <- function(url, dest, min_bytes = 1000L) {
  if (file.exists(dest) && file.size(dest) >= min_bytes) {
    message(glue::glue("  Cache : {basename(dest)}"))
    return(invisible(NULL))
  }
  message(glue::glue("  Télécharge : {url}"))
  curl::curl_download(url, dest, quiet = TRUE, handle = curl::new_handle(useragent = "Mozilla/5.0"))
}

message("→ Téléchargement INSEE")
dl_if_missing(URL_COG_COMMUNES, cog_path, min_bytes = 1e5)
dl_if_missing(URL_COG_MVTS,     mvt_path, min_bytes = 1e5)
dl_if_missing(URL_POP,          pop_path, min_bytes = 1e5)
dl_if_missing(URL_POP23,        pop23_zip, min_bytes = 5e5)
dl_if_missing(URL_MAYOTTE,      mayotte_path, min_bytes = 1e5)

# ---- Lecture COG ----

cog <- readr::read_csv(cog_path, show_col_types = FALSE)
message(glue::glue("COG : {nrow(cog)} entités"))

# Le COG contient COMMUNES (TYPECOM == 'COM'), arrondissements municipaux
# (ARM, codes 75101-75120, 69381-69389, 13201-13216), et communes déléguées
# (COMD). Pour la jointure avec elec/dvf, on garde COMs + ARMs.
cog_main <- cog |>
  dplyr::filter(TYPECOM %in% c("COM", "ARM")) |>
  dplyr::transmute(
    code_commune = COM,
    nom_commune  = LIBELLE,
    type_com     = TYPECOM,
    code_dept    = DEP,
    code_region  = REG
  )
message(glue::glue("  Communes + arrondissements actifs : {nrow(cog_main)}"))

# ---- Lecture mouvements (pour vintage correction) ----

mvts <- readr::read_csv(mvt_path, show_col_types = FALSE)
# Colonnes : MOD (type de mouvement), DATE_EFF, TYPECOM_AV, COM_AV (ancien),
# TYPECOM_AP, COM_AP (nouveau). MOD 32/33/34 = fusions.
fusions <- mvts |>
  dplyr::filter(MOD %in% c("31","32","33","34","41","50","70")) |>
  dplyr::transmute(
    code_avant   = COM_AV,
    code_apres   = COM_AP,
    date_effet   = DATE_EFF,
    type_mvt     = MOD
  ) |>
  dplyr::distinct()
message(glue::glue("  Mouvements de communes (fusions/scissions) : {nrow(fusions)}"))

# ---- Lecture populations ----

pop <- arrow::read_parquet(pop_path)
names(pop) <- tolower(names(pop))
message(glue::glue("Populations : {nrow(pop)} lignes (long format), cols : ",
                   paste(names(pop), collapse=", ")))

# Format LONG : 1 ligne par (commune × annee_rp). Colonnes :
#   codgeo, libgeo, coddep, codreg, pmun, ptot, pcap, annee_rp
# On pivote en wide : pop_2017, pop_2018, ..., pop_latest = pmun max(annee_rp).

# Clé = codgeo SEUL. Avec libgeo dans la clé, une commune renommée entre deux
# millésimes était éclatée en deux lignes (492 codes dupliqués). On garde le
# libellé de l'année la plus récente.
# HARMONISATION COG 2026 (audit C2) : les populations légales RP 2017-2021 sont à la
# géographie d'avant les fusions de 2022-2026. Les effectifs des autres sources étant
# sommés vers le code 2026 (cog_helpers.R), le dénominateur doit l'être aussi, sinon
# une commune fusionnée a un numérateur de N communes pour une population d'une seule.
# Effectif sommé, jamais moyenné. Un code encore vivant en 2026 n'est pas remappé.
source(here::here("scripts", "cog_helpers.R"))
pop_map <- cog_map("2016-01-01", fus = fusions, valid = cog_main$code_commune)
pop_ok <- pop |>
  dplyr::filter(!is.na(pmun)) |>
  dplyr::mutate(codgeo_orig = codgeo, codgeo = to_cog2026(codgeo, pop_map))
n_absorbed <- dplyr::n_distinct(pop_ok$codgeo_orig[pop_ok$codgeo_orig != pop_ok$codgeo])
message(glue::glue("  Codes absorbés sommés vers leur commune 2026 : {n_absorbed}"))
pop_ok <- pop_ok |>
  dplyr::group_by(codgeo, annee_rp) |>
  dplyr::summarise(pmun = sum(pmun), libgeo = libgeo[1], .groups = "drop")
pop_lib <- pop_ok |>
  dplyr::arrange(codgeo, dplyr::desc(annee_rp)) |>
  dplyr::distinct(codgeo, .keep_all = TRUE) |>
  dplyr::select(codgeo, libgeo)
populations_wide <- pop_ok |>
  dplyr::select(codgeo, annee_rp, pmun) |>
  tidyr::pivot_wider(
    id_cols      = codgeo,
    names_from   = annee_rp,
    values_from  = pmun,
    names_prefix = "pop_"
  ) |>
  dplyr::left_join(pop_lib, by = "codgeo")

# Identifier la dernière année disponible
year_cols <- grep("^pop_\\d{4}$", names(populations_wide), value = TRUE)
latest_year <- max(as.integer(sub("pop_", "", year_cols)))
latest_col  <- paste0("pop_", latest_year)
message(glue::glue("  Année RP la plus récente : {latest_year}"))

populations <- populations_wide |>
  dplyr::transmute(
    code_commune = codgeo,
    nom_pop      = libgeo,
    pop_2017     = if ("pop_2017" %in% year_cols) pop_2017 else NA_integer_,
    pop_2018     = if ("pop_2018" %in% year_cols) pop_2018 else NA_integer_,
    pop_2019     = if ("pop_2019" %in% year_cols) pop_2019 else NA_integer_,
    pop_2020     = if ("pop_2020" %in% year_cols) pop_2020 else NA_integer_,
    pop_2021     = if ("pop_2021" %in% year_cols) pop_2021 else NA_integer_,
    pop_latest   = .data[[latest_col]]
  )

# ---- Strates de taille de commune (vraie) ----
# 4 quartiles approximatifs de population (INSEE-like) :
populations <- populations |>
  dplyr::mutate(
    strate_pop = dplyr::case_when(
      pop_latest < 500              ~ "Très petite (< 500 hab.)",
      pop_latest < 2000             ~ "Rurale (500-2 000)",
      pop_latest < 10000            ~ "Bourg / périurbain (2 000-10 000)",
      pop_latest < 50000            ~ "Ville moyenne (10 000-50 000)",
      pop_latest >= 50000           ~ "Grande ville (≥ 50 000)",
      TRUE                          ~ NA_character_
    ),
    strate_pop = factor(strate_pop,
      levels = c("Très petite (< 500 hab.)", "Rurale (500-2 000)",
                 "Bourg / périurbain (2 000-10 000)",
                 "Ville moyenne (10 000-50 000)",
                 "Grande ville (≥ 50 000)"))
  )

# ---- Jointure COG + populations ----

out <- cog_main |>
  dplyr::left_join(populations, by = "code_commune") |>
  dplyr::mutate(
    nom_commune = dplyr::coalesce(nom_commune, nom_pop)
  ) |>
  dplyr::select(-nom_pop)

# Mayotte : absente des populations légales RP 2017-2021 ci-dessus. Source : INSEE Focus 105
# (recensement de Mayotte 2017, population municipale par commune ; total 256 518), téléchargée
# par ce script (URL_MAYOTTE) : aucune dépendance à un autre script. Appariement par LIBELLÉ
# (le fichier n'a pas de code), vérifié exhaustif (17 communes) et sommé = « Ensemble » du fichier.
# Millésime tracé dans `pop_millesime`. NB (audit N10) : ce dénominateur (2017) est plus ancien que
# les numérateurs 2024-2025 des autres sources ; les taux de Mayotte sont donc surestimés
# (population estimée 2024 ~ +25 %) : divulgué sur les vues (cf. docs/decisions.qmd).
out$pop_millesime <- ifelse(is.na(out$pop_latest), NA_integer_, as.integer(latest_year))
norm_lib <- function(x) gsub("[^a-z]", "", tolower(iconv(x, to = "ASCII//TRANSLIT")))
my <- suppressMessages(readxl::read_excel(mayotte_path, sheet = "Figure 2", col_names = FALSE, .name_repair = "minimal"))
my <- data.frame(lib = as.character(my[[1]]), pop17 = suppressWarnings(as.numeric(my[[4]])))
my_tot <- my$pop17[!is.na(my$lib) & my$lib == "Ensemble"]
my <- my[!is.na(my$pop17) & !is.na(my$lib) & my$lib != "Ensemble", ]
my$code_commune <- cog_main$code_commune[match(norm_lib(my$lib),
                     norm_lib(ifelse(cog_main$code_dept == "976", cog_main$nom_commune, NA)))]
r <- match(my$code_commune, out$code_commune)
stopifnot(nrow(my) == 17, !anyNA(r), sum(my$pop17) == my_tot,
          all(is.na(out$pop_latest[r])))  # Mayotte n'avait aucune pop
out$pop_2017[r] <- my$pop17
out$pop_latest[r] <- my$pop17
out$pop_millesime[r] <- 2017L
message(glue::glue("  Mayotte : {nrow(my)} communes, pop 2017 = {sum(my$pop17)}"))

# Communes RÉTABLIES (MOD 21, 2024-2025 : Essarts-en-Bocage / L'Oie / Sainte-Florence, Beaumont-
# les-Nonains / Les Hauts-Talican, Neussargues-Moissac + 4 villages du Cantal) (audit N4).
# Les populations légales 2017-2021 ne connaissent que la commune fusionnée : le parent garde
# une population qui inclut encore ses ex-communes déléguées, et les rétablies n'ont pas de
# population. Les numérateurs (DEFM, BAAC, CAF...) restent sur la commune fusionnée : les taux
# par habitant de ces 10 communes sont masqués dans 05_merge (audit round 3, B).
# On prend pour ces communes (parents compris) la population municipale des populations légales
# 2023 (INSEE, géographie 2025) dans `pop_latest`, avec pop_millesime = 2023. `pop_2021`
# (RP 2021 harmonisé, utilisé pour le total national 67 408 052) n'est pas modifié.
reint <- mvts |>
  dplyr::filter(MOD == "21", DATE_EFF >= as.Date("2024-01-01")) |>
  dplyr::reframe(code = unique(c(COM_AV, COM_AP))) |>
  dplyr::pull(code)
reint <- intersect(reint, out$code_commune)
pl23 <- utils::unzip(pop23_zip, files = "donnees_communes.csv", exdir = file.path(raw_dir, "popleg2023"), overwrite = TRUE)
pl23 <- readr::read_delim(pl23, delim = ";", col_types = readr::cols(.default = "c"), show_col_types = FALSE) |>
  dplyr::transmute(code_commune = COM, pmun23 = as.integer(PMUN))
r <- match(reint, pl23$code_commune)
stopifnot(length(reint) == 10, !anyNA(r))  # 3 + 2 + 5 communes (parents + rétablies)
r_out <- match(reint, out$code_commune)
out$pop_latest[r_out] <- pl23$pmun23[r]
out$pop_millesime[r_out] <- 2023L
message(glue::glue("  Rétablies 2024-2025 : {length(reint)} communes, pop_latest = popleg 2023 (pop_millesime 2023)"))

# PLM : les populations légales sont publiées par arrondissement ; la commune
# 75056/69123/13055 (clé des élections, DGFiP, SSMSI...) y avait pop = NA.
# On la reconstitue comme la somme de ses arrondissements.
plm_city <- c(`751` = "75056", `693` = "69123", `132` = "13055")
pop_cols <- grep("^pop_(\\d{4}|latest)$", names(out), value = TRUE)  # pas pop_millesime (audit N9)
arr <- out |> dplyr::filter(type_com == "ARM") |>
  dplyr::mutate(city = unname(plm_city[substr(code_commune, 1, 3)])) |>
  dplyr::group_by(city) |>
  dplyr::summarise(dplyr::across(dplyr::all_of(pop_cols), ~ sum(.x, na.rm = TRUE)),
                   .groups = "drop")
for (i in seq_len(nrow(arr))) {
  r <- which(out$code_commune == arr$city[i])
  out[r, pop_cols] <- arr[i, pop_cols]
  out$pop_millesime[r] <- as.integer(latest_year)  # somme des arrondissements, même millésime
}
out <- out |> dplyr::mutate(strate_pop = factor(dplyr::case_when(
  pop_latest < 500 ~ "Très petite (< 500 hab.)",
  pop_latest < 2000 ~ "Rurale (500-2 000)",
  pop_latest < 10000 ~ "Bourg / périurbain (2 000-10 000)",
  pop_latest < 50000 ~ "Ville moyenne (10 000-50 000)",
  pop_latest >= 50000 ~ "Grande ville (≥ 50 000)"),
  levels = levels(strate_pop)))

stopifnot(!anyDuplicated(out$code_commune))  # 1 ligne par code commune

# ---- Écriture ----

arrow::write_parquet(out, file.path(out_dir, "populations.parquet"))
arrow::write_parquet(fusions, file.path(out_dir, "commune_fusions.parquet"))

# ---- Rapport ----

cat(glue::glue("

  ✔ {nrow(out)} communes (COG 2026 + pop 2017-2021 sommée sur le COG 2026)
    → {file.path(out_dir, 'populations.parquet')}
  ✔ {nrow(fusions)} mouvements de communes
    → {file.path(out_dir, 'commune_fusions.parquet')}

  Strates de population :
"))
print(out |> dplyr::count(strate_pop))

cat("\n  Distribution pop (latest) :\n")
print(summary(out$pop_latest))
