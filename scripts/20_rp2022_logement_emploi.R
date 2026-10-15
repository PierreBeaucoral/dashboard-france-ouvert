# ============================================================
# scripts/20_rp2022_logement_emploi.R
#
# RP 2022 — logement (résidences secondaires, vacance, propriétaires, ménages
# sans voiture, maisons) et emploi (taux de chômage au sens du recensement,
# taux d'activité, concentration de l'emploi).
#
# Sources : INSEE, recensement de la population 2022, bases infracommunales
# "base-cc" (licence Ouverte / Etalab, conditions de réutilisation INSEE) :
#   Logement : https://www.insee.fr/fr/statistiques/fichier/8581474/base-cc-logement-2022_csv.zip
#   Emploi   : https://www.insee.fr/fr/statistiques/fichier/8581444/base-cc-emploi-pop-active-2022_csv.zip
# Géographie : COG au 01/01/2025 -> harmonisée COG 2026 (effectifs sommés,
# puis taux recalculés). France hors Mayotte (non couverte par le RP 2022 base-cc).
#
# Sortie : data/processed/insee_extra/rp2022_logement_emploi.parquet
#   effectifs : rp_log, rp_rp, rp_rsecocc, rp_logvac, rp_maison, rp_men,
#               rp_prop, rp_men_voit1p, rp_pop1564, rp_act1564, rp_chom1564,
#               rp_actocc, rp_emplt, rp_log_2011, rp_rsecocc_2011
#   taux (%)  : pct_res_secondaires, pct_vacants, pct_proprietaires,
#               pct_menages_sans_voiture, pct_maisons, taux_chomage_rp,
#               taux_activite_rp, indic_concentration_emploi (emplois/100 actifs occupés),
#               delta_pct_res_secondaires_2011_2022 (points)
# Caveats : communes < 10 000 hab. = 5 ans de collecte exhaustive, >= 10 000 =
# échantillon : effectifs faibles imprécis (taux masqués si dénominateur < 50).
# "Vacant" RP != vacant LOVAC. Chômage RP != DEFM (BIT-like auto-déclaré).
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(arrow); library(here)
  library(curl);  library(glue)
})
source(here::here("scripts", "cog_helpers.R"))

URL_LOG <- "https://www.insee.fr/fr/statistiques/fichier/8581474/base-cc-logement-2022_csv.zip"
URL_EMP <- "https://www.insee.fr/fr/statistiques/fichier/8581444/base-cc-emploi-pop-active-2022_csv.zip"
raw_dir <- here::here("data", "raw", "rp2022")
out_dir <- here::here("data", "processed", "insee_extra")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

dl_if_missing <- function(url, dest, min_bytes) {
  if (file.exists(dest) && file.size(dest) >= min_bytes) { message("  Cache : ", basename(dest)); return(invisible()) }
  message("  Télécharge : ", url)
  curl::curl_download(url, dest, quiet = TRUE, handle = curl::new_handle(useragent = "Mozilla/5.0"))
}
log_zip <- file.path(raw_dir, "logement.zip"); emp_zip <- file.path(raw_dir, "emploi.zip")
dl_if_missing(URL_LOG, log_zip, 3e7); dl_if_missing(URL_EMP, emp_zip, 4e7)
read_zip <- function(zip, member, cols) {
  tmp <- tempfile(); dir.create(tmp); on.exit(unlink(tmp, recursive = TRUE))
  unzip(zip, files = member, exdir = tmp)
  readr::read_delim(file.path(tmp, member), delim = ";", show_col_types = FALSE,
                    col_select = dplyr::all_of(cols), col_types = readr::cols(CODGEO = "c", .default = "d"))
}

lg <- read_zip(log_zip, "base-cc-logement-2022.CSV",
               c("CODGEO", "P22_LOG", "P22_RP", "P22_RSECOCC", "P22_LOGVAC", "P22_MAISON",
                 "P22_MEN", "P22_RP_PROP", "P22_RP_VOIT1P", "P11_LOG", "P11_RSECOCC"))
em <- read_zip(emp_zip, "base-cc-emploi-pop-active-2022.CSV",
               c("CODGEO", "P22_POP1564", "P22_ACT1564", "P22_CHOM1564", "P22_ACTOCC", "P22_EMPLT"))
message("Logement : ", nrow(lg), " lignes ; Emploi : ", nrow(em), " lignes")

# ---- Harmonisation COG 2025 -> 2026 : somme des effectifs ----
cmap <- cog_map("2025-01-01")
cnt <- full_join(lg, em, by = "CODGEO") |>
  mutate(code_commune = to_cog2026(CODGEO, cmap)) |>
  select(-CODGEO) |>
  group_by(code_commune) |>
  summarise(across(everything(), \(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)), .groups = "drop") |>
  rename(rp_log = P22_LOG, rp_rp = P22_RP, rp_rsecocc = P22_RSECOCC, rp_logvac = P22_LOGVAC,
         rp_maison = P22_MAISON, rp_men = P22_MEN, rp_prop = P22_RP_PROP,
         rp_men_voit1p = P22_RP_VOIT1P, rp_log_2011 = P11_LOG, rp_rsecocc_2011 = P11_RSECOCC,
         rp_pop1564 = P22_POP1564, rp_act1564 = P22_ACT1564, rp_chom1564 = P22_CHOM1564,
         rp_actocc = P22_ACTOCC, rp_emplt = P22_EMPLT)

valid <- arrow::read_parquet(here::here("data", "processed", "insee", "populations.parquet"))$code_commune
message("Codes hors COG 2026 après remappage : ", sum(!cnt$code_commune %in% valid))
cnt <- filter(cnt, code_commune %in% valid)

# ---- Taux (calculés après harmonisation ; masqués si dénominateur < 50) ----
rate <- function(num, den, min_den = 50) ifelse(is.na(den) | den < min_den, NA_real_, round(100 * num / den, 2))
out <- cnt |>
  mutate(
    pct_res_secondaires      = rate(rp_rsecocc, rp_log),
    pct_vacants              = rate(rp_logvac, rp_log),
    pct_proprietaires        = rate(rp_prop, rp_rp),
    pct_menages_sans_voiture = rate(rp_men - rp_men_voit1p, rp_men),
    pct_maisons              = rate(rp_maison, rp_log),
    taux_chomage_rp          = rate(rp_chom1564, rp_act1564),
    taux_activite_rp         = rate(rp_act1564, rp_pop1564),
    indic_concentration_emploi = rate(rp_emplt, rp_actocc),
    delta_pct_res_secondaires_2011_2022 =
      round(pct_res_secondaires - rate(rp_rsecocc_2011, rp_log_2011), 2)
  ) |>
  mutate(across(where(is.numeric) & starts_with("rp_"), \(x) round(x, 1)))

arrow::write_parquet(out, file.path(out_dir, "rp2022_logement_emploi.parquet"), compression = "zstd")
cat(glue("\n✔ {nrow(out)} communes x {ncol(out)} colonnes → data/processed/insee_extra/rp2022_logement_emploi.parquet\n"))
print(summary(out[c("pct_res_secondaires", "pct_vacants", "pct_proprietaires",
                    "pct_menages_sans_voiture", "taux_chomage_rp", "indic_concentration_emploi")]))
cat("Mayotte (976xx) présente :", any(startsWith(out$code_commune, "976")), "\n")

# ---- Contrôles ----
tot <- function(v) sum(out[[v]], na.rm = TRUE)
stopifnot(
  !anyDuplicated(out$code_commune),
  abs(nrow(out) / 34900 - 1) < 0.05,
  all(c("75056", "69123", "13055") %in% out$code_commune) || all(c("75101", "69381", "13201") %in% out$code_commune),
  all(out$pct_res_secondaires >= 0 & out$pct_res_secondaires <= 100, na.rm = TRUE),
  all(out$pct_vacants >= 0 & out$pct_vacants <= 100, na.rm = TRUE),
  all(out$pct_proprietaires >= 0 & out$pct_proprietaires <= 100, na.rm = TRUE),
  all(out$pct_menages_sans_voiture >= 0 & out$pct_menages_sans_voiture <= 100, na.rm = TRUE),
  all(out$taux_chomage_rp >= 0 & out$taux_chomage_rp <= 100, na.rm = TRUE),
  # ordre de grandeur national RP 2022 : ~37,5 M logements (hors Mayotte), ~5,7 % de rés. secondaires
  tot("rp_log") > 36e6 & tot("rp_log") < 40e6,
  abs(100 * tot("rp_rsecocc") / tot("rp_log") - 9.7) < 1.5
)
cat("\n✔ stopifnot() OK\n")
