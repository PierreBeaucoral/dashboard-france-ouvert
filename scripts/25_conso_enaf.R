# ============================================================
# scripts/25_conso_enaf.R
#
# Consommation d'espaces naturels, agricoles et forestiers (ENAF) 2011-2025 :
# artificialisation (ZAN) par commune.
#
# Source : Cerema, "Consommation d'espaces NAF du 1er janvier 2011 au 1er janvier
#   2025" (Fichiers fonciers retraités ; data.gouv.fr 6a01b9280dd2d45907e5fc61,
#   licence Ouverte 2.0). Fichier conso-com.csv, géographie COG 2025.
#   Dictionnaire : https://doc-datafoncier.cerema.fr/doc/conso-enaf/
#   Unités du fichier : m2 (flux annuels naf{AA}art{AA+1}, art{AA}hab{AA+1}, ...).
#
# Règles :
#   - flux (m2) sommés après remappage COG 2025 -> 2026, ratios recalculés ;
#   - période de référence ZAN = 2011-2021 (10 ans) vs 2021-2025 (4 ans) :
#     variation = (ha/an 2021-25) / (ha/an 2011-21) - 1, NA si référence < 5 ha (bruit) ;
#   - par habitant : ha / pop_latest (RP 2021, INSEE) x 1 000, NA si pop < 50 ;
#   - m2 d'habitat par ménage supplémentaire 2011-2022 = art11hab22 /(men22-men11),
#     NA si gain de ménages < 10 (ratio non défini sinon) ;
#   - c'est un FLUX, pas un stock : ne pas diviser par la surface communale pour
#     obtenir un "% artificialisé".
# Couverture : métropole + 971-974 (pas de Mayotte). Caveat : issu des Fichiers
# fonciers (parcelles cadastrées) : non-cadastré et domaine public manquants.
#
# Sortie : data/processed/risques/enaf_commune.parquet
#   code_commune, enaf_ha_2011_2025, enaf_ha_2011_2021, enaf_ha_2021_2025,
#   enaf_habitat_ha_2011_2025, enaf_part_habitat_pct, enaf_variation_vs_ref_pct,
#   enaf_ha_par_1000hab_2011_2025, enaf_hab_m2_par_nouveau_menage_1122
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(arrow); library(here); library(curl); library(glue)
})
source(here::here("scripts", "cog_helpers.R"))

URL <- paste0("https://static.data.gouv.fr/resources/",
              "consommation-despaces-naturels-agricoles-et-forestiers-du-1er-janvier-2011-au-1er-janvier-2025/",
              "20260724-142909/conso-com.csv")
raw_dir <- here::here("data", "raw", "enaf")
out_dir <- here::here("data", "processed", "risques")
csv_path <- file.path(raw_dir, "conso-com.csv")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
if (!file.exists(csv_path) || file.size(csv_path) < 1e7) {
  message("Télécharge : ", URL)
  curl::curl_download(URL, csv_path, quiet = TRUE)
} else message("  Cache : conso-com.csv")

need <- c("idcom", "naf11art25", "naf11art21", "naf21art25", "art11hab25", "art11hab22",
          "men11", "men22")
d <- readr::read_csv(csv_path, show_col_types = FALSE, col_select = all_of(need),
                     col_types = readr::cols(.default = "d", idcom = "c"))
message("Lignes : ", nrow(d))

pop <- arrow::read_parquet(here::here("data", "processed", "insee", "populations.parquet")) |>
  filter(type_com == "COM") |> select(code_commune, pop_latest)
cmap <- cog_map("2025-01-01")
d <- d |> mutate(code_commune = to_cog2026(idcom, cmap)) |> select(-idcom) |>
  group_by(code_commune) |> summarise(across(everything(), \(x) sum(x, na.rm = TRUE)), .groups = "drop") |>
  inner_join(pop, "code_commune")

out <- d |> transmute(
  code_commune,
  enaf_ha_2011_2025 = round(naf11art25 / 1e4, 2),
  enaf_ha_2011_2021 = round(naf11art21 / 1e4, 2),
  enaf_ha_2021_2025 = round(naf21art25 / 1e4, 2),
  enaf_habitat_ha_2011_2025 = round(art11hab25 / 1e4, 2),
  enaf_part_habitat_pct = ifelse(naf11art25 >= 1e4, round(100 * art11hab25 / naf11art25, 1), NA_real_),
  enaf_variation_vs_ref_pct = ifelse(naf11art21 >= 5e4,
                                     round(100 * ((naf21art25 / 4) / (naf11art21 / 10) - 1), 1), NA_real_),
  enaf_ha_par_1000hab_2011_2025 = ifelse(pop_latest >= 50 & !code_commune %in% retablies_codes(), round(1000 * naf11art25 / 1e4 / pop_latest, 3), NA_real_),
  enaf_hab_m2_par_nouveau_menage_1122 = ifelse(men22 - men11 >= 10, round(art11hab22 / (men22 - men11), 0), NA_real_)
)

arrow::write_parquet(out, file.path(out_dir, "enaf_commune.parquet"), compression = "zstd")
cat(glue("\n✔ {nrow(out)} communes → data/processed/risques/enaf_commune.parquet\n"))
print(summary(out[c("enaf_ha_2011_2025", "enaf_part_habitat_pct", "enaf_variation_vs_ref_pct",
                    "enaf_ha_par_1000hab_2011_2025")]))
cat("National ha 2011-2025 :", round(sum(out$enaf_ha_2011_2025)), " ; 2011-2021 :", round(sum(out$enaf_ha_2011_2021)),
    " ; 2021-2025 :", round(sum(out$enaf_ha_2021_2025)), "\n")

# ---- Contrôles ----
stopifnot(
  !anyDuplicated(out$code_commune),
  abs(nrow(out) / 34860 - 1) < 0.05,
  all(out[c("enaf_ha_2011_2025", "enaf_ha_2011_2021", "enaf_ha_2021_2025")] >= 0),
  # additivité des périodes (tolérance d'arrondi)
  all(abs(out$enaf_ha_2011_2021 + out$enaf_ha_2021_2025 - out$enaf_ha_2011_2025) < 0.05),
  all(out$enaf_part_habitat_pct >= 0 & out$enaf_part_habitat_pct <= 100.5, na.rm = TRUE),
  # Cerema : ~ 20 000-25 000 ha/an en 2011-2021 -> ~ 200-250 k ha sur 10 ans
  between(sum(out$enaf_ha_2011_2021), 150e3, 350e3),
  all(c("75056", "13055", "97411") %in% out$code_commune),
  !any(startsWith(out$code_commune, "976"))
)
cat("\n✔ stopifnot() OK\n")
