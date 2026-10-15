# ============================================================
# scripts/02_elections.R
#
# Étape 2 — Législatives 2024 : résultats par commune, reconstruits au BUREAU DE VOTE.
#
# Source : Ministère de l'Intérieur, via data.gouv.fr (licence Ouverte 2.0).
#   Datasets :
#     T1 : elections-legislatives-des-30-juin-et-7-juillet-2024-resultats-
#          definitifs-du-1er-tour
#     T2 : elections-legislatives-des-30-juin-et-7-juillet-2024-resultats-
#          definitifs-du-2nd-tour
#   Ressources utilisées (URL stables, cf. URL_* ci-dessous) :
#     - resultats-definitifs-par-bureau-de-vote.csv (T1 ~37 Mo, T2 ~17 Mo)
#     - resultats-definitifs-par-circonscription(s)[-legislatives].csv (T1, T2) : drapeau « élu »
#     - resultats-definitifs-france-entiere (T1 xlsx, T2 csv) : totaux officiels de contrôle
#
# Format source : CSV ; séparateur ";" ; décimales "," ; UTF-8. Wide : 1 ligne / bureau (ou
# circonscription) × N blocs de colonnes « candidat N ».
#
# Règle (audit r3 A) — le fichier T2 par commune ne contient QUE les circonscriptions allées au
# 2nd tour : une commune coupée entre circonscriptions voyait son « résultat » ne porter que sur
# une partie de l'électorat (Paris 50 %, Marseille 75 %, Courbevoie 15 %). On reconstruit donc :
#   1. Circonscription de chaque bureau : le fichier par bureau ne porte PAS le code
#      circonscription ; on le déduit des candidats (nom + prénom + département), uniques par
#      circonscription dans les fichiers par circonscription ; contrôle : tous les candidats d'un
#      bureau relèvent de la même circonscription, sinon arrêt.
#   2. Tour décisif d'une circonscription = celui qui a ÉLU son député (T1 si élu au 1er tour,
#      sinon T2).
#   3. Chaque bureau compte avec le tour décisif de SA circonscription ; voix cumulées par famille
#      sur les bureaux de la commune (remappée COG 2026 AVANT somme), parts sur les exprimés
#      cumulés, abstention sur les inscrits cumulés, vainqueur = famille aux voix cumulées les
#      plus élevées. Pour une commune qui mélange des circonscriptions de T1 et de T2, les parts
#      MÉLANGENT donc les deux tours : `tour` = "mixte".
#   4. `couverture_inscrits` = inscrits du résultat affiché / inscrits au 1er tour (≈ 1 partout ;
#      colonne et garde-fou conservés).
#   PLM : le fichier distingue les arrondissements (code BV = arrondissement + n° de bureau) ;
#   on écrit en plus legislatives_2024_arrondissements.parquet (45 lignes) ; la ligne ville du
#   parquet principal est la SOMME des arrondissements (contrôlé), à un bureau lyonnais près
#   (code BV « 0001 », 386 inscrits, sans arrondissement identifiable : dans la ville seulement).
#
# Produit : data/processed/elections/legislatives_2024_t2.parquet (1 ligne / commune COG 2026)
#           data/processed/elections/legislatives_2024_arrondissements.parquet (Paris, Lyon, Marseille)
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
  library(readxl)
})

# ---- Sources ----
# Bureaux de vote, circonscriptions, France entière : URL stables static.data.gouv.fr (horodatage
# = version publiée le 10/07/2024 ; T1 France entière en xlsx seulement).
B1 <- paste0("https://static.data.gouv.fr/resources/",
             "elections-legislatives-des-30-juin-et-7-juillet-2024-resultats-definitifs-du-1er-tour/")
B2 <- paste0("https://static.data.gouv.fr/resources/",
             "elections-legislatives-des-30-juin-et-7-juillet-2024-resultats-definitifs-du-2nd-tour/")
SRC <- c(
  "bv-2024-t1.csv"       = paste0(B1, "20240710-171445/resultats-definitifs-par-bureau-de-vote.csv"),
  "bv-2024-t2.csv"       = paste0(B2, "20240710-170658/resultats-definitifs-par-bureau-de-vote.csv"),
  "circo-2024-t1.csv"    = paste0(B1, "20240710-171413/resultats-definitifs-par-circonscriptions-legislatives.csv"),
  "circo-2024-t2.csv"    = paste0(B2, "20240710-170728/resultats-definitifs-par-circonscription.csv"),
  "france-2024-t1.xlsx"  = paste0(B1, "20240710-171459/resultats-definitifs-france-entiere.xlsx"),
  "france-2024-t2.csv"   = paste0(B2, "20240710-170713/resultats-definitifs-france-entiere.csv")
)
MIN_SIZE <- c("bv-2024-t1.csv" = 3e7, "bv-2024-t2.csv" = 1.5e7, "circo-2024-t1.csv" = 3e5,
              "circo-2024-t2.csv" = 1e5, "france-2024-t1.xlsx" = 5e3, "france-2024-t2.csv" = 1e3)

# ---- Mapping nuance MI → famille politique ----
# Familles = NFP / ENS / LR / RN / Divers. Choix du propriétaire (2026-10-15) : REC (Reconquête) et
# RDG (Parti radical de gauche) restent dans « Divers » : ni nuances du bloc RN (REC a présenté des
# candidats contre le RN dans 323 de ses 328 circonscriptions), ni du NFP (en 78-10, 82-02 et
# ZZ06, le candidat RDG s'est présenté contre le candidat officiel UG). Ils sont toutefois LISTÉS
# explicitement : une nuance absente de la table fait ÉCHOUER le script (plus de repli silencieux).
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
  "DIV" = "Divers",
  "REC" = "Divers",   # Reconquête : voir l'en-tête de la table
  "RDG" = "Divers"    # Parti radical de gauche : idem
)
FAMS <- c("NFP", "ENS", "LR", "RN", "Divers")

# ---- Helpers de nettoyage ----

clean_pct <- function(x) {
  x <- stringr::str_remove(x, "%")
  x <- stringr::str_replace(x, ",", ".")
  x <- stringr::str_trim(x)
  suppressWarnings(as.numeric(x))
}

clean_int <- function(x) {
  x <- stringr::str_remove_all(x, "\\s| ")  # espaces fines (séparateur milliers)
  suppressWarnings(as.integer(x))
}

# ---- Parser commun (bureaux, circonscriptions) ----

#' Fichier « wide » du ministère -> long, 1 ligne par (bureau ou circonscription) x candidat.
#' Colonnes absentes du niveau lu (commune/BV pour les circonscriptions, circonscription pour
#' les bureaux) = NA. Codes normalisés : département 2 car., commune 5 car., BV et
#' circonscription 4 car. (le fichier T1 perd les zéros initiaux : « 1001 », « 101 », « 929 »).
parse_tour <- function(csv_path, tour_label) {
  message(glue::glue("→ Parsing {basename(csv_path)} (tour = {tour_label})"))
  df_wide <- readr::read_delim(
    csv_path, delim = ";",
    locale = readr::locale(decimal_mark = ",", grouping_mark = " ", encoding = "UTF-8"),
    col_types = readr::cols(.default = readr::col_character()),
    na = c("", "NA"), trim_ws = TRUE, progress = FALSE)
  for (cn in c("Code commune", "Libellé commune", "Code BV", "Code circonscription législative"))
    if (!cn %in% names(df_wide)) df_wide[[cn]] <- NA_character_
  candidate_cols <- names(df_wide)[stringr::str_detect(names(df_wide), "\\s\\d+$")]
  df_wide$.row <- seq_len(nrow(df_wide))
  message(glue::glue("  {nrow(df_wide)} lignes, {ncol(df_wide)} colonnes"))
  pad <- function(x, w) ifelse(is.na(x), NA_character_, stringr::str_pad(x, w, "left", "0"))
  long <- df_wide |>
    tidyr::pivot_longer(cols = tidyselect::all_of(candidate_cols),
                        names_to = c(".value", "candidate_num"),
                        names_pattern = "^(.*) (\\d+)$") |>
    dplyr::filter(!is.na(`Nuance candidat`) & `Nuance candidat` != "") |>
    dplyr::transmute(
      code_insee   = pad(`Code commune`, 5),
      # identifiant de ligne source = identifiant de bureau : les codes BV ne sont PAS uniques
      # (Montbéliard 25388 : 4 bureaux tous codés « 0000 » au T1)
      bv_id        = paste0(tour_label, "-", .row),
      nom_commune  = `Libellé commune`,
      code_dept    = pad(`Code département`, 2),
      libelle_dept = `Libellé département`,
      code_bv      = pad(`Code BV`, 4),
      code_circo   = pad(`Code circonscription législative`, 4),
      inscrits     = clean_int(Inscrits),
      votants      = clean_int(Votants),
      exprimes     = clean_int(`Exprimés`),
      abstentions  = clean_int(Abstentions),
      nuance       = `Nuance candidat`,
      nom          = toupper(`Nom candidat`),
      prenom       = toupper(`Prénom candidat`),
      voix         = clean_int(Voix),
      elu          = !is.na(Elu) & stringr::str_detect(tolower(Elu), "lu"),
      tour         = tour_label
    ) |>
    dplyr::mutate(ckey = paste(code_dept, nom, prenom, sep = "|"))
  inconnues <- setdiff(unique(long$nuance), names(nuance_to_famille))
  if (length(inconnues)) stop("Nuances absentes de nuance_to_famille : ", paste(inconnues, collapse = ", "))
  dplyr::mutate(long, famille = unname(nuance_to_famille[nuance]))
}

# ---- Bureau -> circonscription (par les candidats) ----
#' Le fichier par bureau ne porte pas le code circonscription. Un candidat (dép. + nom + prénom)
#' appartient à une seule circonscription du fichier par circonscription du même tour ; un bureau
#' hérite de la circonscription de ses candidats. Arrêt si un candidat est ambigu, introuvable, ou
#' si les candidats d'un même bureau relèvent de plusieurs circonscriptions.
attach_circo <- function(bv, circo) {
  cm <- dplyr::distinct(circo, ckey, code_circo)
  stopifnot(!anyDuplicated(cm$ckey))
  x <- dplyr::left_join(dplyr::select(bv, -code_circo), cm, by = "ckey")
  stopifnot(!anyNA(x$code_circo))
  n <- x |> dplyr::group_by(bv_id) |> dplyr::summarise(n = dplyr::n_distinct(code_circo), .groups = "drop")
  stopifnot(all(n$n == 1L))
  x
}

# ---- Bureau-level -> commune-level ----
#' Agrège des lignes bureau x candidat (déjà au bon tour décisif et au bon code cible)
#' vers 1 ligne / `code_insee`. Règle unique : famille gagnante = voix CUMULÉES les plus élevées,
#' parts sur exprimés cumulés, marge = écart avec la 2e famille. Égalité : ordre FAMS
#' (déterministe). `tour` = "T1" / "T2" / "mixte" (bureaux de circonscriptions des deux tours).
commune_table <- function(long) {
  bu <- long |>
    dplyr::distinct(bv_id, code_insee, code_bv, code_circo, nom_commune, code_dept, libelle_dept,
                    inscrits, votants, exprimes, abstentions, tour)
  stopifnot(!anyDuplicated(bu$bv_id))
  fixes <- bu |>
    dplyr::group_by(code_insee) |>
    dplyr::summarise(
      nom_commune  = nom_commune[which.max(inscrits)],
      code_dept    = code_dept[which.max(inscrits)],
      libelle_dept = libelle_dept[which.max(inscrits)],
      tour = { t <- unique(tour[inscrits > 0]); if (length(t) == 1L) t else if (length(t) == 0L) tour[1] else "mixte" },
      dplyr::across(c(inscrits, votants, exprimes, abstentions), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop") |>
    dplyr::mutate(pct_abstention = ifelse(inscrits > 0, round(100 * abstentions / inscrits, 2), NA_real_))

  fam_voix <- long |>
    dplyr::group_by(code_insee, famille) |>
    dplyr::summarise(voix = sum(voix, na.rm = TRUE), .groups = "drop") |>
    dplyr::left_join(dplyr::select(fixes, code_insee, exprimes), by = "code_insee") |>
    dplyr::mutate(pct = ifelse(exprimes > 0, round(100 * voix / exprimes, 2), NA_real_)) |>
    dplyr::group_by(code_insee) |>
    dplyr::arrange(dplyr::desc(voix), match(famille, FAMS), .by_group = TRUE) |>
    dplyr::mutate(rang = dplyr::row_number()) |>
    dplyr::ungroup()

  # nuance la plus votée (voix cumulées) DANS la famille gagnante (libellé d'affichage)
  top_nuance <- long |>
    dplyr::group_by(code_insee, famille, nuance) |>
    dplyr::summarise(voix = sum(voix, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(code_insee, famille) |>
    dplyr::slice_max(voix, n = 1L, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(code_insee, famille, nuance)

  vainqueur <- fam_voix |>
    dplyr::filter(rang == 1L) |>
    dplyr::left_join(top_nuance, by = c("code_insee", "famille")) |>
    dplyr::transmute(code_insee, nuance_vainqueur = nuance, famille_vainqueur = famille,
                     voix_vainqueur = as.integer(voix), pct_vainqueur = pct)
  second <- fam_voix |>
    dplyr::filter(rang == 2L) |>
    dplyr::transmute(code_insee, voix_second = as.integer(voix), pct_second = pct)
  pct_par_famille <- fam_voix |>
    dplyr::select(code_insee, famille, pct) |>
    tidyr::pivot_wider(names_from = famille, values_from = pct, names_prefix = "pct_")
  for (f in FAMS) if (!paste0("pct_", f) %in% names(pct_par_famille)) pct_par_famille[[paste0("pct_", f)]] <- NA_real_

  fixes |>
    dplyr::left_join(vainqueur, by = "code_insee") |>
    dplyr::left_join(second, by = "code_insee") |>
    dplyr::left_join(pct_par_famille, by = "code_insee") |>
    dplyr::mutate(marge_vainqueur = pct_vainqueur - dplyr::coalesce(pct_second, 0)) |>
    # Aucun suffrage exprimé : pas de vainqueur (NA), pas une famille par défaut.
    dplyr::mutate(dplyr::across(
      c(nuance_vainqueur, famille_vainqueur, voix_vainqueur, pct_vainqueur, marge_vainqueur),
      ~ dplyr::if_else(is.na(exprimes) | exprimes <= 0L, .x[NA_integer_][1], .x))) |>
    dplyr::select(code_insee, nom_commune, code_dept, libelle_dept, inscrits, votants, exprimes,
                  abstentions, pct_abstention, tour, nuance_vainqueur, famille_vainqueur,
                  voix_vainqueur, pct_vainqueur, voix_second, pct_second,
                  pct_RN, pct_ENS, pct_NFP, pct_LR, pct_Divers, marge_vainqueur)
}

# ---- Harmonisation COG 2026 (audit M1/C2) ----
# Le fichier du ministère est au COG 2024 et contient des codes qui ne sont pas des communes
# (circonscriptions des Français de l'étranger ZZ…, COM 975/98x, ZX). Les codes sont remappés
# vers le COG 2026 AU NIVEAU BUREAU, avant toute somme (voix et effectifs sommés exactement :
# plus de recombinaison de pourcentages). Prérequis : 06_insee_populations.R.
source(here::here("scripts", "cog_helpers.R"))
cog_valid <- arrow::read_parquet(here::here("data", "processed", "insee", "populations.parquet")) |>
  dplyr::filter(type_com == "COM") |> dplyr::pull(code_commune)
# Depuis 2015 (et non 2024) : certaines sections communales portent d'anciens codes de
# changement de département/commune (MOD 41, 2015-2018 : Le Fresne-sur-Loire 44060, Pont-Farcy 14513,
# Freigné 49144...) puis fusionnées en 2019-2024 ; elles étaient écartées à tort (audit N5).
cog_m24 <- cog_map("2015-01-01")
# Cas unique hors table de mouvements (audit N5) : le fichier T1 porte une section « Cormicy »
# (dép. 02 Aisne, 31 inscrits) sous le code 02899, absent de tout COG ; Gernicourt (02344,
# ex-Aisne) est commune déléguée de Cormicy (51171, Marne) depuis 2017.
manuel <- c(`02899` = "51171")

#' Remappe `code_insee` (COG 2026) ; garde-fou : seuls les non-communes (ZZ/ZX étranger, COM
#' 975-989) sont écartés, tout autre code inconnu = commune réelle perdue -> erreur.
cog_remap <- function(df, keep_codes = cog_valid) {
  df <- dplyr::mutate(df, code_insee = to_cog2026(dplyr::coalesce(unname(manuel[code_insee]), code_insee), cog_m24))
  inconnus <- setdiff(unique(df$code_insee), keep_codes)
  reels <- inconnus[!grepl("^(Z|97[5-9]|98|99)", inconnus)]
  if (length(reels) > 0L) stop("Codes communes hors COG 2026 non remappés : ", paste(reels, collapse = ", "))
  dplyr::filter(df, code_insee %in% keep_codes)
}

# ---- Arrondissements PLM ----
# Le fichier est au niveau VILLE (75056 / 69123 / 13055) mais le code BV = arrondissement (2 chiffres)
# + n° de bureau (2 chiffres) : « 0101 » = Paris 1er, bureau 01 ; « 1613 » = Marseille 16e, bureau 13.
plm_arr_code <- function(code_commune, code_bv) {
  a <- as.integer(substr(code_bv, 1, 2))
  dplyr::case_when(code_commune == "75056" ~ sprintf("751%02d", a),
                   code_commune == "69123" ~ sprintf("693%02d", 80L + a),
                   code_commune == "13055" ~ sprintf("132%02d", a))
}
plm_arr_valid <- c(sprintf("751%02d", 1:20), sprintf("693%02d", 81:89), sprintf("132%02d", 1:16))
plm_prefix <- c(`75056` = "751", `69123` = "693", `13055` = "132")

# ---- Chemins ----

raw_dir  <- here::here("data", "raw", "elections")
out_dir  <- here::here("data", "processed", "elections")
out_path <- file.path(out_dir, "legislatives_2024_t2.parquet")
arr_path <- file.path(out_dir, "legislatives_2024_arrondissements.parquet")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Téléchargement (idempotent) ----

for (f in names(SRC)) {
  p <- file.path(raw_dir, f)
  if (!file.exists(p) || file.size(p) < MIN_SIZE[[f]]) {
    message(glue::glue("Téléchargement {f} ({SRC[[f]]})"))
    curl::curl_download(SRC[[f]], p, quiet = FALSE)
  }
}

# ---- Lecture + circonscription de chaque bureau ----

circo_t1 <- parse_tour(file.path(raw_dir, "circo-2024-t1.csv"), "T1")
circo_t2 <- parse_tour(file.path(raw_dir, "circo-2024-t2.csv"), "T2")
bv_t1 <- parse_tour(file.path(raw_dir, "bv-2024-t1.csv"), "T1") |> attach_circo(circo_t1)
bv_t2 <- parse_tour(file.path(raw_dir, "bv-2024-t2.csv"), "T2") |> attach_circo(circo_t2)
stopifnot(dplyr::n_distinct(bv_t1$bv_id) == 70102L, dplyr::n_distinct(bv_t2$bv_id) == 61615L)  # lignes sources

# ---- Tour décisif par circonscription (celui qui a élu le député) ----

circo_elu_t1 <- unique(circo_t1$code_circo[circo_t1$elu])
circo_elu_t2 <- unique(circo_t2$code_circo[circo_t2$elu])
message(glue::glue("Circonscriptions : {dplyr::n_distinct(circo_t1$code_circo)} au T1 ({length(circo_elu_t1)} ",
                   "élues au T1) ; {dplyr::n_distinct(circo_t2$code_circo)} au T2 ({length(circo_elu_t2)} élues)"))
# 577 circonscriptions (dont 11 + ZX de l'étranger/COM), 76 députés élus au 1er tour (résultats
# officiels MI / Assemblée nationale), 501 au 2nd tour ; aucune élue deux fois, aucune oubliée.
stopifnot(length(circo_elu_t1) == 76L, length(circo_elu_t2) == 501L,
          !length(intersect(circo_elu_t1, circo_elu_t2)),
          setequal(union(circo_elu_t1, circo_elu_t2), unique(circo_t1$code_circo)),
          setequal(unique(circo_t2$code_circo), circo_elu_t2))

# ---- Contrôle 1 : bureaux -> circonscriptions, au niveau CANDIDAT ----
# Somme des voix de chaque candidat sur ses bureaux == voix du candidat dans le fichier officiel par
# circonscription (même périmètre, même tour). Exact attendu ; sinon le rattachement bureau ->
# circonscription ou le fichier est faux et on s'arrête (aucune tolérance).
check_candidats <- function(bv, circo, lab) {
  a <- bv |> dplyr::group_by(ckey, code_circo) |> dplyr::summarise(v_bv = sum(voix), .groups = "drop")
  b <- circo |> dplyr::select(ckey, code_circo, v_circo = voix)
  m <- dplyr::full_join(a, b, by = c("ckey", "code_circo"))
  bad <- m[is.na(m$v_bv) | is.na(m$v_circo) | m$v_bv != m$v_circo, ]
  message(glue::glue("  Contrôle candidats {lab} : {nrow(m)} candidats, {nrow(bad)} écarts"))
  if (nrow(bad)) { print(utils::head(bad, 20)); stop("Voix par candidat bureaux != circonscriptions (", lab, ")") }
  invisible(m)
}
check_candidats(bv_t1, circo_t1, "T1")
check_candidats(bv_t2, circo_t2, "T2")

# ---- Contrôle 2 : totaux nationaux vs « France entière » officiel ----
# Exprimés, inscrits et voix par nuance, tous bureaux (étranger et COM compris).
read_fe <- function(path) {
  fe <- if (grepl("xlsx$", path)) readxl::read_excel(path, col_types = "text")
        else readr::read_delim(path, ";", col_types = readr::cols(.default = "c"), trim_ws = TRUE, progress = FALSE)
  fe <- fe[1, ]
  num <- function(x) as.numeric(gsub("[^0-9]", "", x))
  nn <- sub("^Nuance candidat ", "", grep("^Nuance candidat ", names(fe), value = TRUE))
  list(inscrits = num(fe$Inscrits), exprimes = num(fe[["Exprimés"]]),
       voix = tibble::tibble(nuance = unlist(fe[paste("Nuance candidat", nn)]),
                             voix = num(unlist(fe[paste("Voix", nn)]))) |>
         dplyr::filter(!is.na(nuance)) |> dplyr::group_by(nuance) |> dplyr::summarise(voix = sum(voix)))
}
check_national <- function(bv, fe_path, lab) {
  fe <- read_fe(fe_path)
  b <- bv |> dplyr::distinct(bv_id, inscrits, exprimes)
  v <- bv |> dplyr::group_by(nuance) |> dplyr::summarise(voix_bv = sum(voix))
  cmp <- dplyr::full_join(v, fe$voix, by = "nuance") |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ dplyr::coalesce(.x, 0)))
  message(glue::glue("  National {lab} : exprimés bureaux {format(sum(b$exprimes), big.mark = ' ')} vs officiel ",
                     "{format(fe$exprimes, big.mark = ' ')} ; inscrits {format(sum(b$inscrits), big.mark = ' ')} vs ",
                     "{format(fe$inscrits, big.mark = ' ')} ; nuances en écart : {sum(cmp$voix_bv != cmp$voix)}"))
  stopifnot(sum(b$exprimes) == fe$exprimes, sum(b$inscrits) == fe$inscrits, all(cmp$voix_bv == cmp$voix))
}
# Sources : France entière T1 (xlsx) et T2 (csv), data.gouv.fr, mêmes datasets que ci-dessus.
check_national(bv_t1, file.path(raw_dir, "france-2024-t1.xlsx"), "T1")
check_national(bv_t2, file.path(raw_dir, "france-2024-t2.csv"), "T2")

# ---- Sélection du tour décisif bureau par bureau ----

bv_dec <- dplyr::bind_rows(
  dplyr::filter(bv_t1, code_circo %in% circo_elu_t1),
  dplyr::filter(bv_t2, code_circo %in% circo_elu_t2))
# exprimés du tour décisif : bureaux == circonscriptions (fichier officiel, chacune à son tour décisif)
exp_dec_bv <- sum(dplyr::distinct(bv_dec, bv_id, exprimes)$exprimes)
circo_dec  <- dplyr::bind_rows(dplyr::filter(circo_t1, code_circo %in% circo_elu_t1),
                               dplyr::filter(circo_t2, code_circo %in% circo_elu_t2))
exp_dec_ci <- sum(dplyr::distinct(circo_dec, code_circo, exprimes, tour)$exprimes)
message(glue::glue("Exprimés du tour décisif : bureaux {format(exp_dec_bv, big.mark = ' ')} ; ",
                   "circonscriptions {format(exp_dec_ci, big.mark = ' ')}"))
stopifnot(exp_dec_bv == exp_dec_ci)

# ---- Arrondissements PLM (avant remappage COG : la ville reste la somme) ----

plm_cities <- names(plm_prefix)
arr_dec <- bv_dec |>
  dplyr::filter(code_insee %in% plm_cities) |>
  dplyr::mutate(code_insee = plm_arr_code(code_insee, code_bv))
# Un seul bureau n'a pas d'arrondissement identifiable : Lyon, code BV « 0001 » (386 inscrits, sans
# préfixe d'arrondissement ; les autres bureaux lyonnais sont « 0101 »...). Il reste dans la ville
# (somme sur tous les bureaux) mais est écarté du fichier par arrondissement.
arr_hors <- arr_dec |> dplyr::filter(!code_insee %in% plm_arr_valid)
stopifnot(identical(unique(arr_hors$code_bv), "0001"), dplyr::n_distinct(arr_hors$tour) == 1L)
arr_dec <- dplyr::filter(arr_dec, code_insee %in% plm_arr_valid)
stopifnot(setequal(unique(arr_dec$code_insee), plm_arr_valid))
inscrits_hors <- sum(dplyr::distinct(arr_hors, code_bv, inscrits)$inscrits)
agg_arr <- commune_table(arr_dec)
# T1 par arrondissement (mêmes bureaux, 1er tour, même décodage) : parts T1 et couverture
arr_t1 <- bv_t1 |>
  dplyr::filter(code_insee %in% plm_cities) |>
  dplyr::mutate(code_insee = plm_arr_code(code_insee, code_bv)) |>
  dplyr::filter(code_insee %in% plm_arr_valid)
agg_arr_t1 <- commune_table(arr_t1)

# ---- Pipeline commune ----

agg_dec <- commune_table(cog_remap(bv_dec))
agg_t1  <- commune_table(cog_remap(bv_t1))      # 1er tour, toutes circonscriptions

# Contrôle PLM : ville == somme des arrondissements (effectifs), au bureau « 0001 » de Lyon près
for (cty in plm_cities) {
  a <- agg_arr[substr(agg_arr$code_insee, 1, 3) == plm_prefix[[cty]], ]
  v <- agg_dec[agg_dec$code_insee == cty, ]
  gap <- if (cty == "69123") inscrits_hors else 0L
  stopifnot(sum(a$inscrits) + gap == v$inscrits)
  if (cty != "69123") stopifnot(sum(a$exprimes) == v$exprimes, sum(a$abstentions) == v$abstentions)
}

# Commune sans suffrage exprimé au tour décisif (ex. Geay 79131 : 0 exprimé au T2) : le T1 prend le relais.
sans_exprime <- agg_dec$code_insee[is.na(agg_dec$famille_vainqueur)]
if (length(sans_exprime)) {
  message("  Sans exprimé au tour décisif -> T1 : ", paste(sans_exprime, collapse = ", "))
  agg_dec <- dplyr::bind_rows(dplyr::filter(agg_dec, !code_insee %in% sans_exprime),
                              dplyr::filter(agg_t1, code_insee %in% sans_exprime))
}
out <- agg_dec

# T1 universel : toutes les communes votent au 1er tour ; parts par famille du T1 conservées
# (panneau Élections : T1 ET résultat décisif côte à côte). Couverture : inscrits du résultat
# affiché / inscrits au 1er tour (toutes circonscriptions). Avec la règle « tour décisif par
# bureau » tous les bureaux sont couverts ; l'écart résiduel est la différence observée d'inscrits
# publiés entre les deux tours (communes d'un seul bureau, p. ex. Louppy-le-Château 149 -> 128).
# Plafonnée à 1. Garde-fous : ratio > 0,8 partout, > 0,95 dès 500 inscrits, < 1,25 (< 1,02 dès 500).
add_t1_cov <- function(dec, t1) {
  extra <- t1 |>
    dplyr::transmute(
      code_insee,
      pct_NFP_t1        = pct_NFP,
      pct_ENS_t1        = pct_ENS,
      pct_RN_t1         = pct_RN,
      pct_LR_t1         = pct_LR,
      pct_Divers_t1     = pct_Divers,
      pct_abstention_t1 = pct_abstention,
      nuance_vainqueur_t1  = nuance_vainqueur,
      famille_vainqueur_t1 = famille_vainqueur,
      pct_vainqueur_t1     = pct_vainqueur,
      inscrits_t1 = inscrits)
  x <- dplyr::left_join(dec, extra, by = "code_insee") |>
    dplyr::mutate(ratio = inscrits / inscrits_t1, inscrits_t1 = NULL,
                  couverture_inscrits = round(pmin(1, ratio), 4))
  stopifnot(!anyNA(x$ratio), all(x$ratio < 1.25), all(x$ratio[x$inscrits >= 500] < 1.02),
            all(x$ratio > 0.8), all(x$ratio[x$inscrits >= 500] > 0.95))
  x$ratio <- NULL
  stopifnot(all(x$couverture_inscrits > 0 & x$couverture_inscrits <= 1), !anyNA(x$couverture_inscrits))
  x
}
out <- add_t1_cov(out, agg_t1)
agg_arr <- add_t1_cov(agg_arr, agg_arr_t1)
# Repli T1 (source sans suffrage exprimé au T2 : Geay) : drapeau pour un libellé véridique
# (le député y a été élu au 2nd tour mais la source ne publie aucune voix de ce tour).
out$repli_t1     <- out$code_insee %in% sans_exprime
agg_arr$repli_t1 <- FALSE
message(glue::glue("Couverture (inscrits décisifs / T1) : min {min(out$couverture_inscrits)}, ",
                   "{sum(out$couverture_inscrits < 0.99)} communes < 0,99"))

# Libellé officiel COG 2026 (le nom de la composante la plus peuplée serait faux pour
# une commune nouvelle : « La Longeville » pour Pays-de-Montbenoît).
pops <- arrow::read_parquet(here::here("data", "processed", "insee", "populations.parquet"))
cog_nm <- pops |> dplyr::filter(type_com == "COM") |> dplyr::select(code_insee = code_commune, nom_cog = nom_commune)
out <- out |>
  dplyr::left_join(cog_nm, by = "code_insee") |>
  dplyr::mutate(nom_commune = dplyr::coalesce(nom_cog, nom_commune), nom_cog = NULL)
stopifnot(!anyDuplicated(out$code_insee), all(out$code_insee %in% cog_valid))

arr_nm <- pops |> dplyr::filter(code_commune %in% plm_arr_valid) |>
  dplyr::select(code_insee = code_commune, nom_arr = nom_commune)
agg_arr <- agg_arr |>
  dplyr::left_join(arr_nm, by = "code_insee") |>
  dplyr::mutate(nom_commune = dplyr::coalesce(nom_arr, nom_commune), nom_arr = NULL)

# ---- Écriture ----

arrow::write_parquet(out, out_path)
arrow::write_parquet(agg_arr, arr_path)

# ---- Rapport ----

message(glue::glue("Couverture < 100 % : {sum(out$couverture_inscrits < 1)} communes ; < 80 % : {sum(out$couverture_inscrits < 0.8)}"))
n_communes <- nrow(out)
by_tour    <- out |> dplyr::count(tour, name = "n_communes")
by_tour_famille <- out |> dplyr::count(tour, famille_vainqueur) |>
  tidyr::pivot_wider(names_from = tour, values_from = n, values_fill = 0L)

cat(glue::glue("

  ✔ {n_communes} communes traitées → {out_path}
    ({round(file.size(out_path)/1024)} KB) ; {nrow(agg_arr)} arrondissements → {arr_path}

  Communes par tour affiché :
"))
print(by_tour)

cat("\n  Famille gagnante × tour :\n")
print(by_tour_famille)
