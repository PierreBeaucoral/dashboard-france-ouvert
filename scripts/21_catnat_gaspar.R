# ============================================================
# scripts/21_catnat_gaspar.R
#
# Arrêtés de catastrophe naturelle (CatNat) et PPRN par commune.
#
# Source : MTE / Géorisques, base GASPAR (licence Ouverte 2.0), fichier officiel
#   http://files.georisques.fr/GASPAR/gaspar.zip (extraction du 2026-10-05,
#   catnat_gaspar_*.csv = 1 ligne par arrêté x commune x péril ; pprn_gaspar_*.csv).
#   Vintage : événements 1982-08 -> 2025-12 (les arrêtés récents arrivent avec
#   plusieurs mois de retard : 2025 est sous-compté).
#
# Règles :
#   - unité = arrêté distinct (id_gaspar) ; lignes dupliquées (même arrêté,
#     commune, péril) dédoublonnées ;
#   - harmonisation COG 2026 : les codes sont remappés AVANT comptage, puis on
#     recompte les arrêtés DISTINCTS par commune (un arrêté qui visait deux
#     communes fusionnées compte 1 fois, pas 2) ;
#   - périls : inondation = ICB+IRN+COB ; secheresse = SEC ; mvt_terrain =
#     MVT+GLT+EFA+ECB+LVT+GET+GER ; submersion = CMV+RAZ ; tempete = TMP+VCY ;
#     n_catnat_total = tous périls (y c. grêle, neige, séisme, ...).
#   - commune sans arrêté = 0 (vrai zéro), annee_dernier_catnat = NA.
#   - Mayotte incluse (76 arrêtés environ).
# Caveat : un arrêté est accordé sur DEMANDE de la commune : mesure d'exposition
# reconnue administrativement, pas d'intensité de l'aléa ; petites communes
# sous-représentées ; sécheresse concentrée sur les zones d'argile.
#
# Sortie : data/processed/risques/catnat_commune.parquet
#   code_commune, n_catnat_total, n_catnat_since_2015, n_catnat_inondation,
#   n_catnat_secheresse, n_catnat_mvt_terrain, n_catnat_submersion,
#   n_catnat_tempete, annee_dernier_catnat, pprn_opposable, pprn_inondation_opposable,
#   pprn_rga_opposable (0/1)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(arrow); library(here)
  library(curl);  library(glue)
})
source(here::here("scripts", "cog_helpers.R"))

URL <- "http://files.georisques.fr/GASPAR/gaspar.zip"
raw_dir <- here::here("data", "raw", "catnat")
out_dir <- here::here("data", "processed", "risques")
zip_path <- file.path(raw_dir, "gaspar.zip")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(zip_path) || file.size(zip_path) < 5e6) {
  message("Télécharge : ", URL)
  curl::curl_download(URL, zip_path, quiet = TRUE)
} else message("  Cache : gaspar.zip")

tmp <- tempfile(); dir.create(tmp); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
members <- unzip(zip_path, list = TRUE)$Name
f_cat  <- grep("^catnat_gaspar", members, value = TRUE)
f_pprn <- grep("^pprn_gaspar",   members, value = TRUE)
stopifnot(length(f_cat) == 1, length(f_pprn) == 1)
unzip(zip_path, files = c(f_cat, f_pprn), exdir = tmp)

valid <- arrow::read_parquet(here::here("data", "processed", "insee", "populations.parquet")) |>
  filter(type_com == "COM") |> pull(code_commune)
cmap <- cog_map("1982-01-01")

# ---- CatNat ----
cn <- readr::read_delim(file.path(tmp, f_cat), delim = ";", show_col_types = FALSE,
                        col_types = readr::cols(.default = "c"),
                        col_select = c(id_gaspar, code_commune, num_risque_jo, date_debut)) |>
  mutate(code_commune = to_cog2026(code_commune, cmap),
         annee = as.integer(substr(date_debut, 1, 4)),
         peril = case_when(
           num_risque_jo %in% c("ICB", "IRN", "COB") ~ "inondation",
           num_risque_jo == "SEC" ~ "secheresse",
           num_risque_jo %in% c("MVT", "GLT", "EFA", "ECB", "LVT", "GET", "GER") ~ "mvt_terrain",
           num_risque_jo %in% c("CMV", "RAZ") ~ "submersion",
           num_risque_jo %in% c("TMP", "VCY") ~ "tempete",
           TRUE ~ "autre"))
message("Lignes catnat : ", nrow(cn), " ; hors COG 2026 : ", sum(!cn$code_commune %in% valid))
cn <- filter(cn, code_commune %in% valid)

n_distinct_by <- function(df, nm) df |> distinct(code_commune, id_gaspar) |> count(code_commune, name = nm)
tot   <- n_distinct_by(cn, "n_catnat_total")
since <- n_distinct_by(filter(cn, annee >= 2015), "n_catnat_since_2015")
last  <- cn |> group_by(code_commune) |> summarise(annee_dernier_catnat = max(annee), .groups = "drop")
by_p  <- cn |> distinct(code_commune, id_gaspar, peril) |> count(code_commune, peril) |>
  tidyr::pivot_wider(names_from = peril, values_from = n, values_fill = 0L, names_prefix = "n_catnat_") |>
  select(code_commune, n_catnat_inondation, n_catnat_secheresse, n_catnat_mvt_terrain,
         n_catnat_submersion, n_catnat_tempete)

# ---- PPRN (opposable = approuvé ou appliqué par anticipation) ----
pp <- readr::read_delim(file.path(tmp, f_pprn), delim = ";", show_col_types = FALSE,
                        col_types = readr::cols(.default = "c")) |>
  transmute(code_commune = to_cog2026(`CODE INSEE COMMUNE`, cmap),
            modele = `CODE MODELE`, etat = `LIBELLE ETAT`) |>
  filter(etat == "Opposable", code_commune %in% valid) |>
  group_by(code_commune) |>
  summarise(pprn_opposable = 1L,
            pprn_inondation_opposable = as.integer(any(modele == "PPRN-I")),
            pprn_rga_opposable = as.integer(any(modele == "PPRN-RGA")), .groups = "drop")

zero_cols <- c("n_catnat_total", "n_catnat_since_2015", "n_catnat_inondation", "n_catnat_secheresse",
               "n_catnat_mvt_terrain", "n_catnat_submersion", "n_catnat_tempete",
               "pprn_opposable", "pprn_inondation_opposable", "pprn_rga_opposable")
out <- tibble(code_commune = valid) |>
  left_join(tot, "code_commune") |> left_join(since, "code_commune") |>
  left_join(by_p, "code_commune") |> left_join(last, "code_commune") |>
  left_join(pp, "code_commune") |>
  mutate(across(all_of(zero_cols), \(x) as.integer(coalesce(x, 0L)))) |>
  select(code_commune, all_of(zero_cols[1:7]), annee_dernier_catnat, all_of(zero_cols[8:10]))

arrow::write_parquet(out, file.path(out_dir, "catnat_commune.parquet"), compression = "zstd")
cat(glue("\n✔ {nrow(out)} communes → data/processed/risques/catnat_commune.parquet\n"))
print(summary(out$n_catnat_total))
cat("Communes avec >=1 arrêté :", sum(out$n_catnat_total > 0), "\n")
cat("PPRN opposable :", sum(out$pprn_opposable), " dont inondation :", sum(out$pprn_inondation_opposable), "\n")

# ---- Contrôles ----
stopifnot(
  !anyDuplicated(out$code_commune),
  abs(nrow(out) / length(valid) - 1) < 0.001,
  all(out$n_catnat_total >= pmax(out$n_catnat_since_2015, out$n_catnat_inondation, out$n_catnat_secheresse)),
  all(out$annee_dernier_catnat >= 1982 & out$annee_dernier_catnat <= 2026, na.rm = TRUE),
  # ~34 700 communes distinctes avec arrêté dans l'extraction brute (~34 600 après fusions)
  between(sum(out$n_catnat_total > 0), 0.95 * 34600, 1.05 * 34700),
  between(sum(out$pprn_opposable), 0.95 * 12000, 1.05 * 14300),
  all(c("75056", "13055", "97611") %in% out$code_commune)
)
cat("\n✔ stopifnot() OK\n")
