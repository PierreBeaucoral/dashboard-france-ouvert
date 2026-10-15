# ============================================================
# scripts/22_bpe_equipements.R
#
# Base permanente des équipements (BPE) 2025 : comptes d'équipements par
# commune et score "panier de la vie courante".
#
# Source : INSEE, BPE 2025 (millésime au 01/01/2025), publication 06/2026
#   https://www.insee.fr/fr/statistiques/fichier/8217527/DS_BPE_CSV_FR.zip  (Melodi, format long)
#   dictionnaire : https://www.insee.fr/fr/metadonnees/source/fichier/TYPEQU_2025.csv
#   Licence Ouverte / Etalab (conditions INSEE).
#
# Règles :
#   - ligne retenue : GEO_OBJECT == "COM" (les codes GEO sont aussi ceux d'ARR,
#     UU2020, EPCI...) et FACILITY_TYPE = code TYPEQU (lignes de total _T écartées) ;
#   - une ligne absente = 0 équipement de ce type (pas NA) ;
#   - Paris/Lyon/Marseille : commune mère (75056, 69123, 13055), pas d'arrondissements ;
#   - harmonisation COG 2025 -> 2026 : comptes sommés, présence/score recalculés après ;
#   - taux pour 1 000 hab. avec pop_latest de populations.parquet (RP 2021),
#     NA si population < 50.
#
# PANIER VIE COURANTE (choix à consigner dans docs/decisions.qmd) : score 0-10 =
# nombre de familles présentes parmi : médecin généraliste (D265), pharmacie (D307),
# boulangerie-pâtisserie (B207), alimentation générale (B104/B105/B201/B202),
# école (C107/C108/C109), poste (A206/A207/A208), banque (A203), coiffure (A501),
# restaurant (A504), chirurgien-dentiste (D277).
# Caveat : présence dans la commune != accès (une commune sans pharmacie peut en
# être à 2 min) ; temps d'accès non calculé ici.
#
# Sortie : data/processed/insee_extra/bpe_equipements.parquet
#   code_commune, bpe_n_{services,commerces,enseignement,sante,transport,sport_culture,tourisme},
#   bpe_n_total, bpe_n_medecin_gen, bpe_n_pharmacie, bpe_n_boulangerie, bpe_n_ecole,
#   bpe_n_alimentation, bpe_n_poste, bpe_n_banque, bpe_n_coiffure, bpe_n_restaurant, bpe_n_dentiste,
#   bpe_n_urgences, bpe_n_gare, bpe_n_maison_sante, bpe_n_france_services,
#   bpe_<domaine>_pour1000 (7 domaines + total), bpe_panier_score (0-10), bpe_panier_manquants
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(arrow); library(here)
  library(curl);  library(glue); library(stringr)
})
source(here::here("scripts", "cog_helpers.R"))

URL <- "https://www.insee.fr/fr/statistiques/fichier/8217527/DS_BPE_CSV_FR.zip"
raw_dir <- here::here("data", "raw", "bpe")
out_dir <- here::here("data", "processed", "insee_extra")
zip_path <- file.path(raw_dir, "bpe.zip")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
if (!file.exists(zip_path) || file.size(zip_path) < 5e6) {
  message("Télécharge : ", URL)
  curl::curl_download(URL, zip_path, quiet = TRUE, handle = curl::new_handle(useragent = "Mozilla/5.0"))
} else message("  Cache : bpe.zip")

tmp <- tempfile(); dir.create(tmp); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
unzip(zip_path, files = "DS_BPE_2025_data.csv", exdir = tmp)
d <- readr::read_delim(file.path(tmp, "DS_BPE_2025_data.csv"), delim = ";", show_col_types = FALSE,
                       col_select = c(GEO, GEO_OBJECT, FACILITY_TYPE, OBS_VALUE),
                       col_types = readr::cols(.default = "c", OBS_VALUE = "d")) |>
  filter(GEO_OBJECT == "COM", str_detect(FACILITY_TYPE, "^[A-G][0-9]{3}$"))
message("Lignes COM x type : ", nrow(d), " ; communes : ", n_distinct(d$GEO))

cmap <- cog_map("2025-01-01")
valid <- arrow::read_parquet(here::here("data", "processed", "insee", "populations.parquet")) |>
  filter(type_com == "COM") |> select(code_commune, pop_latest)
d <- d |> mutate(code_commune = to_cog2026(GEO, cmap)) |>
  group_by(code_commune, FACILITY_TYPE) |> summarise(n = sum(OBS_VALUE), .groups = "drop") |>
  filter(code_commune %in% valid$code_commune)

# ---- Comptes par domaine ----
dom <- c(A = "services", B = "commerces", C = "enseignement", D = "sante",
         E = "transport", F = "sport_culture", G = "tourisme")
by_dom <- d |> mutate(dom = paste0("bpe_n_", dom[substr(FACILITY_TYPE, 1, 1)])) |>
  group_by(code_commune, dom) |> summarise(n = sum(n), .groups = "drop") |>
  pivot_wider(names_from = dom, values_from = n, values_fill = 0)

cnt <- function(types, nm) d |> filter(FACILITY_TYPE %in% types) |>
  group_by(code_commune) |> summarise(!!nm := sum(n), .groups = "drop")

fam <- list(  # famille du panier -> codes TYPEQU
  medecin_gen = "D265", pharmacie = "D307", boulangerie = "B207",
  alimentation = c("B104", "B105", "B201", "B202"), ecole = c("C107", "C108", "C109"),
  poste = c("A206", "A207", "A208"), banque = "A203", coiffure = "A501",
  restaurant = "A504", dentiste = "D277")
fam_cnt <- lapply(names(fam), \(k) cnt(fam[[k]], paste0("bpe_n_", k)))
extra <- list(cnt("D106", "bpe_n_urgences"), cnt(c("E107", "E108", "E109"), "bpe_n_gare"),
              cnt("D113", "bpe_n_maison_sante"), cnt("A128", "bpe_n_france_services"))

out <- valid |> select(code_commune) |> left_join(by_dom, "code_commune")
for (x in c(fam_cnt, extra)) out <- left_join(out, x, "code_commune")
out <- out |> mutate(across(starts_with("bpe_n_"), \(x) as.integer(coalesce(x, 0))))
dom_cols <- paste0("bpe_n_", dom)
out$bpe_n_total <- rowSums(out[dom_cols])

# ---- Panier : présence de chaque famille (indicatrices recalculées après harmonisation) ----
pres <- sapply(names(fam), \(k) out[[paste0("bpe_n_", k)]] > 0)
out$bpe_panier_score <- as.integer(rowSums(pres))
out$bpe_panier_manquants <- apply(pres, 1, \(r) paste(names(fam)[!r], collapse = ","))

# ---- Pour 1 000 habitants ----
out <- out |> left_join(valid, "code_commune")
for (c in c(dom_cols, "bpe_n_total")) {
  out[[sub("bpe_n_", "bpe_", paste0(c, "_pour1000"))]] <-
    ifelse(out$pop_latest >= 50, round(1000 * out[[c]] / out$pop_latest, 2), NA_real_)
}
out <- select(out, -pop_latest)

arrow::write_parquet(out, file.path(out_dir, "bpe_equipements.parquet"), compression = "zstd")
cat(glue("\n✔ {nrow(out)} communes x {ncol(out)} colonnes → data/processed/insee_extra/bpe_equipements.parquet\n"))
print(table(out$bpe_panier_score))
print(summary(out$bpe_total_pour1000))

# ---- Contrôles ----
stopifnot(
  !anyDuplicated(out$code_commune),
  abs(nrow(out) / 34875 - 1) < 0.01,
  all(out$bpe_panier_score >= 0 & out$bpe_panier_score <= 10),
  all(out$bpe_n_total >= 0),
  sum(out$bpe_n_pharmacie > 0 | out$bpe_n_medecin_gen > 0) > 8000,
  # ordre de grandeur national : ~20 000 pharmacies, ~ 60 000+ médecins gén. équipements
  between(sum(out$bpe_n_pharmacie), 18000, 24000),
  between(sum(out$bpe_n_boulangerie), 40000, 60000),
  all(c("75056", "69123", "13055", "97611") %in% out$code_commune),
  out$bpe_panier_score[out$code_commune == "75056"] >= 9
)
cat("\n✔ stopifnot() OK\n")
