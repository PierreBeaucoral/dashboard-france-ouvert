# ============================================================
# scripts/27_energie_ore.R
#
# Bloc G (réouverture) — Consommation annuelle d'électricité et de gaz par
# commune × année × énergie × grand secteur (Agence ORE / opérateurs de réseaux).
#
# Source  : Agence ORE, portail opendata.agenceore.fr (plateforme Opendatasoft)
#           dataset `consommation-annuelle-d-electricite-et-gaz-par-commune`
#           (data.gouv.fr 65f1185fd7321d8e647ba548), Licence Ouverte 2.0.
#           ~3,4 M de lignes (opérateur × commune × année × secteur × NAF2).
#           Au lieu du CSV brut (~800 MB), on utilise l'agrégation côté serveur
#           (endpoint /exports/csv de l'API Explore v2.1 avec group_by + select) :
#           1 export par année, ~175 000 lignes, ~5 MB.
#
# Secret statistique : ORE masque (conso et nb_sites = NULL) toute maille
#   (opérateur × commune × secteur) ne respectant pas les seuils de secret
#   (< 10 points de livraison, ou un point > 80 % de la conso). Le champ
#   `nombre_de_mailles_secretisees` compte les mailles masquées. Une cellule dont
#   TOUTES les lignes sont masquées devient NA (jamais 0). Une cellule
#   partiellement masquée est une borne basse (`secret_partiel = TRUE`).
#   Une cellule absente du fichier n'est pas un secret : pas de consommation publiée
#   (ex. commune non desservie en gaz) -> reste absente / NA dans le wide.
#
# Rupture de série : à partir de 2018 le périmètre change (davantage de
#   distributeurs, tertiaire <36 kVA inclus, nb_sites x 5-10). Comparer
#   2011-2017 à 2018+ n'a pas de sens : les évolutions n'utilisent que >= 2018.
#
# Harmonisation COG 2026 : codes absents du COG 2026 remappés via
#   data/processed/insee/commune_fusions.parquet (script 06), MWh SOMMÉS
#   (jamais moyennés).
#
# Sorties :
#   data/processed/energie_ore/conso_commune_long.parquet
#       code_commune, annee, energie, secteur, conso_mwh, nb_pdl, n_mailles_secretisees, secret_partiel
#   data/processed/energie_ore/energie_ore_commune.parquet  (1 ligne / commune)
# ============================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(stringr)
  library(arrow); library(here); library(curl); library(jsonlite)
})

DATASET <- "consommation-annuelle-d-electricite-et-gaz-par-commune"
API     <- paste0("https://opendata.agenceore.fr/api/explore/v2.1/catalog/datasets/", DATASET)
URL_META <- paste0("https://opendata.agenceore.fr/data-fair/api/v1/datasets/", DATASET)

raw_dir <- here::here("data", "raw", "energie_ore")
out_dir <- here::here("data", "processed", "energie_ore")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Métadonnées (licence, millésime de mise à jour) ----
meta_path <- file.path(raw_dir, "metadata.json")
if (!file.exists(meta_path)) curl::curl_download(URL_META, meta_path, quiet = TRUE)
meta <- jsonlite::read_json(meta_path)
message(sprintf("ORE : %s | %s lignes | licence : %s | maj : %s",
                meta$title, format(meta$count, big.mark = " "),
                meta$license$title, meta$dataUpdatedAt))

# ---- Années disponibles (appel agrégé, repli sur le cache) ----
yrs_path <- file.path(raw_dir, "annees.json")
if (!file.exists(yrs_path)) {
  curl::curl_download(paste0(API, "/records?group_by=annee&limit=100"), yrs_path, quiet = TRUE)
}
annees <- sort(as.integer(vapply(jsonlite::read_json(yrs_path)$results, `[[`, "", "annee")))
message("Années : ", min(annees), "-", max(annees))

# ---- Export agrégé par année (cache brut, politesse : 1 s entre appels) ----
SELECT <- paste0("code_commune,annee,filiere,code_grand_secteur,",
                 "sum(conso_totale_mwh) as mwh,sum(nb_sites) as sites,",
                 "count(conso_totale_mwh) as n_val,",
                 "sum(nombre_de_mailles_secretisees) as secr,count(*) as n")
for (y in annees) {
  f <- file.path(raw_dir, sprintf("ore_commune_secteur_%d.csv", y))
  if (file.exists(f) && file.size(f) > 1e5) next
  message("Export agrégé ", y, " …")
  h <- curl::new_handle(timeout = 600)
  u <- paste0(API, "/exports/csv?group_by=code_commune,annee,filiere,code_grand_secteur",
              "&select=", URLencode(SELECT, reserved = TRUE),
              "&where=", URLencode(sprintf("annee='%d'", y), reserved = TRUE))
  curl::curl_download(u, paste0(f, ".part"), handle = h, quiet = TRUE)
  stopifnot(file.size(paste0(f, ".part")) > 1e5)
  file.rename(paste0(f, ".part"), f)
  Sys.sleep(1)
}

raw <- purrr::map_dfr(file.path(raw_dir, sprintf("ore_commune_secteur_%d.csv", annees)),
  readr::read_delim, delim = ";", show_col_types = FALSE,
  col_types = readr::cols(code_commune = "c", annee = "i", filiere = "c",
                          code_grand_secteur = "c", .default = "d"))
message("Lignes agrégées brutes : ", nrow(raw))

# ---- Normalisation énergie / secteur ----
long0 <- raw |>
  transmute(
    code_commune,
    annee,
    energie = if_else(str_detect(filiere, "^Gaz"), "gaz", "elec"),
    secteur = case_when(
      code_grand_secteur == "RESIDENTIEL" ~ "residentiel",
      code_grand_secteur == "TERTIAIRE"   ~ "tertiaire",
      code_grand_secteur == "INDUSTRIE"   ~ "industrie",
      code_grand_secteur == "AGRICULTURE" ~ "agriculture",
      TRUE                                ~ "inconnu"),   # INCONNU, NAP, vide
    mwh, sites, n_val, secr)

# ---- Harmonisation COG 2026 ----
pop <- arrow::read_parquet(here::here("data", "processed", "insee", "populations.parquet"))
cog_ok  <- unique(pop$code_commune[pop$type_com == "COM"])        # 34 875 communes
fus <- arrow::read_parquet(here::here("data", "processed", "insee", "commune_fusions.parquet")) |>
  filter(code_avant != code_apres) |>
  arrange(date_effet) |> distinct(code_avant, .keep_all = TRUE)
map_cog <- function(x) {          # suit les chaînes de fusions, uniquement pour les codes hors COG 2026
  for (i in 1:4) {
    j <- match(x, fus$code_avant)
    x <- if_else(!x %in% cog_ok & !is.na(j), fus$code_apres[j], x)
  }
  x
}
# 2011-2017 : ORE publie Paris/Lyon/Marseille par arrondissement -> commune (somme)
long0$code_commune <- case_when(
  str_detect(long0$code_commune, "^132(0[1-9]|1[0-6])$")     ~ "13055",
  str_detect(long0$code_commune, "^693(8[1-9])$")            ~ "69123",
  str_detect(long0$code_commune, "^751(0[1-9]|1[0-9]|20)$")  ~ "75056",
  TRUE ~ long0$code_commune)
n_remap <- sum(!long0$code_commune %in% cog_ok)
long0$code_commune <- map_cog(long0$code_commune)
message(sprintf("Lignes hors COG 2026 : %d ; après remappage encore hors COG : %d",
                n_remap, sum(!long0$code_commune %in% cog_ok)))
lost <- long0 |> filter(!code_commune %in% cog_ok) |> group_by(annee) |>
  summarise(mwh_perdus = sum(mwh, na.rm = TRUE) / 1e3, .groups = "drop")
message("GWh écartés (codes sans correspondance COG 2026) : ",
        paste(lost$annee, round(lost$mwh_perdus), sep = ":", collapse = " "))
long0 <- filter(long0, code_commune %in% cog_ok)   # ponytail: résidu (codes obsolètes sans mouvement) écarté

# ---- Somme sur communes fusionnées ; NA si TOUT est masqué ----
long <- long0 |>
  group_by(code_commune, annee, energie, secteur) |>
  summarise(conso_mwh = if (sum(n_val) == 0) NA_real_ else sum(mwh, na.rm = TRUE),
            nb_pdl    = if (sum(n_val) == 0) NA_real_ else sum(sites, na.rm = TRUE),
            n_mailles_secretisees = sum(secr, na.rm = TRUE),
            .groups = "drop") |>
  mutate(secret_partiel = n_mailles_secretisees > 0 & !is.na(conso_mwh))
arrow::write_parquet(long, file.path(out_dir, "conso_commune_long.parquet"))
message(sprintf("Long : %d lignes, %d communes, %.1f%% de cellules masquées (NA)",
                nrow(long), n_distinct(long$code_commune), 100 * mean(is.na(long$conso_mwh))))

# ---- Population (INSEE 2017-2021, année la plus proche hors plage) ----
# PLM : population de la commune = somme des arrondissements.
pop_l <- pop |>
  filter(!code_commune %in% c("13055", "69123", "75056")) |>   # ligne COM partielle : on somme les ARM
  mutate(code_commune = case_when(
    str_detect(code_commune, "^132(0[1-9]|1[0-6])$") ~ "13055",
    str_detect(code_commune, "^693(8[1-9])$")        ~ "69123",
    str_detect(code_commune, "^751(0[1-9]|1[0-9]|20)$") ~ "75056",
    TRUE ~ code_commune)) |>
  select(code_commune, starts_with("pop_20")) |>
  pivot_longer(-code_commune, names_to = "an", values_to = "pop", names_prefix = "pop_") |>
  filter(!is.na(pop)) |>
  group_by(code_commune, an) |> summarise(pop = sum(pop), .groups = "drop") |>   # doublons -> arrondissements / lignes NA déjà exclus
  mutate(annee_pop = as.integer(an))
# ponytail: doublons (code,année) identiques dans populations.parquet seraient sommés ; vérifié par stopifnot ci-dessous
pop_ref <- function(a) pmin(pmax(a, 2017L), 2021L)

# ---- Indicateurs ----
last_y <- max(long$annee); base_y <- 2019L
stopifnot(base_y %in% annees)
get <- function(en, se, y, nm) long |>
  filter(energie == en, secteur == se, annee == y) |>
  select(code_commune, !!nm := conso_mwh)
res_y <- function(y, sfx) {
  e <- long |> filter(energie == "elec", secteur == "residentiel", annee == y) |>
    select(code_commune, !!paste0("elec_res_mwh", sfx) := conso_mwh, !!paste0("elec_res_pdl", sfx) := nb_pdl)
  g <- get("gaz", "residentiel", y, paste0("gaz_res_mwh", sfx))
  full_join(e, g, by = "code_commune")
}
tot_y <- function(y, sfx) long |> filter(annee == y) |>
  group_by(code_commune) |>
  summarise(!!paste0("conso_tot_mwh", sfx) := sum(conso_mwh, na.rm = TRUE),
            !!paste0("secret_tot", sfx) := any(is.na(conso_mwh)), .groups = "drop")

pop_last <- pop_l |> filter(annee_pop == pop_ref(last_y)) |> select(code_commune, pop)

ind <- res_y(last_y, "") |>
  full_join(res_y(base_y, "_2019"), by = "code_commune") |>
  full_join(tot_y(last_y, ""), by = "code_commune") |>
  full_join(tot_y(base_y, "_2019"), by = "code_commune") |>
  left_join(pop_last, by = "code_commune") |>
  mutate(
    annee_ore      = last_y,
    annee_pop_ref  = pop_ref(last_y),
    gaz_res_publie = !is.na(gaz_res_mwh),
    elec_res_mwh_hab = elec_res_mwh / pop,
    elec_res_mwh_pdl = if_else(elec_res_pdl > 0, elec_res_mwh / elec_res_pdl, NA_real_),
    gaz_res_mwh_hab  = gaz_res_mwh / pop,
    part_gaz_res     = gaz_res_mwh / (gaz_res_mwh + elec_res_mwh),
    evol_conso_tot_19_last_pct   = if_else(conso_tot_mwh_2019 > 0,
                                    100 * (conso_tot_mwh / conso_tot_mwh_2019 - 1), NA_real_),
    evol_elec_res_19_last_pct    = if_else(elec_res_mwh_2019 > 0,
                                    100 * (elec_res_mwh / elec_res_mwh_2019 - 1), NA_real_),
    evol_gaz_res_19_last_pct     = if_else(gaz_res_mwh_2019 > 0,
                                    100 * (gaz_res_mwh / gaz_res_mwh_2019 - 1), NA_real_)) |>
  filter(!is.na(elec_res_mwh) | !is.na(gaz_res_mwh) | !is.na(conso_tot_mwh)) |>
  rename(nb_pdl_elec_res = elec_res_pdl, nb_pdl_elec_res_2019 = elec_res_pdl_2019, pop_hab = pop)
arrow::write_parquet(ind, file.path(out_dir, "energie_ore_commune.parquet"))
message(sprintf("Indicateurs : %d communes (dernière année %d, population %d)",
                nrow(ind), last_y, pop_ref(last_y)))

# ============================ CHECKS ============================
# 1. clés uniques
stopifnot(!anyDuplicated(long[c("code_commune", "annee", "energie", "secteur")]),
          !anyDuplicated(ind$code_commune),
          !anyDuplicated(pop_l[c("code_commune", "annee_pop")]))
# 2. totaux nationaux de la dernière année vs chiffres publiés de MÊME périmètre
#    (réseaux de distribution), tolérance +/- 10 %. Totaux ORE par opérateur : appel agrégé, mis en cache.
#    Électricité : ORE, opérateur Enedis  vs  Enedis, Bilan électrique 2024 (« Conso. clients » du
#      réseau Enedis, y c. ELD et autoconsommation) = 321,7 TWh
#      https://enedis.fr/media/4542/download (p. 73, lu le 2026-10-14).
#    Gaz : ORE, opérateur GRDF  vs  CRE, délibération n° 2025-122 du 14/05/2025 (§ 2.1.3.2) :
#      volumes acheminés par GRDF en 2024 = 230,7 TWh (prévision ATRD7 : 247,5)
#      https://www.cre.fr/fileadmin/Documents/Deliberations/2025/250514_2025-122_Evolution_ATRD7_GRDF_1er_juillet_2025.pdf
REF <- list(elec = c(op = "Enedis", twh = 321.7), gaz = c(op = "GRDF", twh = 230.7))
op_path <- file.path(raw_dir, sprintf("operateurs_%d.json", last_y))
if (!file.exists(op_path)) {
  curl::curl_download(paste0(API, "/records?group_by=filiere,operateur&order_by=m%20desc&limit=100&select=filiere,operateur,",
    URLencode("sum(conso_totale_mwh) as m", reserved = TRUE), "&where=",
    URLencode(sprintf("annee='%d'", last_y), reserved = TRUE)), op_path, quiet = TRUE)
}
ops <- bind_rows(lapply(jsonlite::read_json(op_path)$results, as.data.frame)) |>
  mutate(energie = if_else(str_detect(filiere, "^Gaz"), "gaz", "elec"))
for (en in names(REF)) {
  ore_op <- ops$m[ops$energie == en & ops$operateur == REF[[en]]["op"]] / 1e6
  ref    <- as.numeric(REF[[en]]["twh"])
  message(sprintf("Contrôle %s %d : ORE %s = %.1f TWh vs publié %.1f TWh (écart %+.1f %%)",
                  en, last_y, REF[[en]]["op"], ore_op, ref, 100 * (ore_op / ref - 1)))
  stopifnot(abs(ore_op / ref - 1) < 0.10)
  # cohérence interne : total du parquet (tous opérateurs) = total API (écart = codes non mappés)
  tot_pq  <- sum(long$conso_mwh[long$annee == last_y & long$energie == en], na.rm = TRUE) / 1e6
  tot_api <- sum(ops$m[ops$energie == en]) / 1e6
  stopifnot(abs(tot_pq / tot_api - 1) < 0.01)
}
stopifnot(between(ind$pop_hab[ind$code_commune == "75056"], 2.0e6, 2.3e6))   # Paris = somme des 20 ARM
# 3. plages plausibles par habitant (résidentiel, communes >= 50 hab.)
q <- ind |> filter(pop_hab >= 50)
message("elec_res_mwh_hab quantiles : ", paste(round(quantile(q$elec_res_mwh_hab, c(.01,.5,.99), na.rm = TRUE), 2), collapse = " / "))
message("gaz_res_mwh_hab  quantiles : ", paste(round(quantile(q$gaz_res_mwh_hab,  c(.01,.5,.99), na.rm = TRUE), 2), collapse = " / "))
stopifnot(between(median(q$elec_res_mwh_hab, na.rm = TRUE), 1.2, 3.5),
          between(median(q$gaz_res_mwh_hab, na.rm = TRUE), 0.5, 6),
          all(q$part_gaz_res >= 0 & q$part_gaz_res <= 1, na.rm = TRUE),
          all(ind$elec_res_mwh >= 0, na.rm = TRUE))
# 4. nombre de communes attendu (COG 2026 : 34 875 ; ORE couvre ~tout)
n_res <- sum(!is.na(ind$elec_res_mwh))
message("Communes avec élec résidentiel publiée : ", n_res, " / ", length(cog_ok))
stopifnot(n_res > 0.93 * length(cog_ok), all(ind$code_commune %in% cog_ok))
message("Tous les contrôles passent.")
