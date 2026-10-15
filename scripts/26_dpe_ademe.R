# ============================================================
# scripts/26_dpe_ademe.R
#
# DPE (diagnostics de performance énergétique) logements existants, agrégés par
# commune côté serveur : part de logements F/G ("passoires thermiques").
#
# Source : ADEME, "DPE Logements existants (depuis juillet 2021)" (dpe03existant),
#   API data-fair, licence Ouverte 2.0, ~15,8 M DPE, mis à jour quotidiennement.
#   https://data.ademe.fr/data-fair/api/v1/datasets/dpe03existant/values_agg
#   (field=code_insee_ban;etiquette_dpe, filtre code_departement_ban, ~100 appels,
#   aucun téléchargement en masse). Cache JSON par département dans data/raw/dpe/
#   (rafraîchi si > 30 jours).
#
# Règles :
#   - comptes (n par étiquette A-G) sommés après remappage COG -> 2026 ;
#     taux calculés après ; NA si n < 30 DPE ;
#   - un DPE = un diagnostic, pas un logement : les DPE sont établis à la vente
#     ou à la location, donc l'échantillon n'est PAS représentatif du parc
#     (ADEME/SDES le disent) ; les doublons de logement ne sont pas dédoublonnables
#     en agrégé ; le changement de méthode du 01/07/2024 (logements < 40 m2) déplace
#     des étiquettes. À afficher comme "part des DPE réalisés depuis 07/2021".
#   - aucun département tronqué : on vérifie total_values <= agg_size.
#
# Sortie : data/processed/insee_extra/dpe_commune.parquet
#   code_commune, dpe_n, dpe_n_A ... dpe_n_G, dpe_pct_fg, dpe_pct_ab, dpe_date_extraction
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(arrow); library(here); library(curl)
  library(jsonlite); library(glue)
})
source(here::here("scripts", "cog_helpers.R"))

BASE <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe03existant/values_agg"
AGG_SIZE <- 1000L  # maximum accepté par l'API
raw_dir <- here::here("data", "raw", "dpe")
out_dir <- here::here("data", "processed", "insee_extra")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

pop <- arrow::read_parquet(here::here("data", "processed", "insee", "populations.parquet"))
deps <- sort(unique(pop$code_dept))
message("Départements : ", length(deps))

fetch_dep <- function(dep, tries = 4) {
  path <- file.path(raw_dir, paste0("dep_", dep, ".json"))
  if (file.exists(path) && difftime(Sys.time(), file.mtime(path), units = "days") < 30)
    return(jsonlite::read_json(path))
  url <- glue("{BASE}?field=code_insee_ban;etiquette_dpe&qs=code_departement_ban:{dep}&agg_size={AGG_SIZE}&size=0")
  for (i in seq_len(tries)) {
    r <- try(curl::curl_fetch_memory(url), silent = TRUE)
    if (!inherits(r, "try-error") && r$status_code == 200) {
      writeBin(r$content, path)
      return(jsonlite::fromJSON(rawToChar(r$content), simplifyVector = FALSE))
    }
    Sys.sleep(2^i)  # back-off
  }
  stop("API ADEME en échec pour le département ", dep)
}

parse_dep <- function(j, dep) {
  stopifnot(j$total_values <= AGG_SIZE)  # pas de troncature
  if (length(j$aggs) == 0) return(NULL)
  bind_rows(lapply(j$aggs, function(a) {
    if (length(a$aggs) == 0) return(NULL)
    tibble(code_insee = a$value,
           etiquette = vapply(a$aggs, \(b) b$value, ""),
           n = vapply(a$aggs, \(b) as.numeric(b$total), 0))
  }))
}

res <- lapply(deps, \(dep) { message("  ", dep); parse_dep(fetch_dep(dep), dep) })
raw <- bind_rows(res)
message("Lignes commune x étiquette : ", nrow(raw), " ; communes : ", n_distinct(raw$code_insee))

cmap <- cog_map("2021-01-01")
valid <- pop$code_commune
lab <- c("A", "B", "C", "D", "E", "F", "G")
wide <- raw |>
  filter(etiquette %in% lab) |>
  mutate(code_commune = to_cog2026(code_insee, cmap)) |>
  group_by(code_commune, etiquette) |> summarise(n = sum(n), .groups = "drop") |>
  pivot_wider(names_from = etiquette, values_from = n, values_fill = 0, names_prefix = "dpe_n_")
for (l in lab) if (!paste0("dpe_n_", l) %in% names(wide)) wide[[paste0("dpe_n_", l)]] <- 0
message("Codes hors COG 2026 écartés : ", sum(!wide$code_commune %in% valid))

out <- wide |>
  filter(code_commune %in% valid) |>
  mutate(dpe_n = dpe_n_A + dpe_n_B + dpe_n_C + dpe_n_D + dpe_n_E + dpe_n_F + dpe_n_G,
         dpe_pct_fg = ifelse(dpe_n >= 30, round(100 * (dpe_n_F + dpe_n_G) / dpe_n, 2), NA_real_),
         dpe_pct_ab = ifelse(dpe_n >= 30, round(100 * (dpe_n_A + dpe_n_B) / dpe_n, 2), NA_real_),
         dpe_date_extraction = as.character(Sys.Date())) |>
  select(code_commune, dpe_n, starts_with("dpe_n_"), dpe_pct_fg, dpe_pct_ab, dpe_date_extraction)

arrow::write_parquet(out, file.path(out_dir, "dpe_commune.parquet"), compression = "zstd")
cat(glue("\n✔ {nrow(out)} communes avec >= 1 DPE ({sum(!is.na(out$dpe_pct_fg))} avec n >= 30), {sum(out$dpe_n)} DPE → data/processed/insee_extra/dpe_commune.parquet\n"))
print(summary(out$dpe_pct_fg))
cat("Mayotte (976xx) présente :", any(startsWith(out$code_commune, "976")), "\n")

# ---- Contrôles ----
stopifnot(
  !anyDuplicated(out$code_commune),
  all(out$code_commune %in% valid),
  between(nrow(out), 0.9 * 34875, 34875),
  all(out$dpe_pct_fg >= 0 & out$dpe_pct_fg <= 100, na.rm = TRUE),
  all(out$dpe_pct_fg + out$dpe_pct_ab <= 100.01, na.rm = TRUE),
  # ~15,8 M DPE dans la base ; on doit en retrouver l'essentiel (> 95 %)
  sum(out$dpe_n) > 0.95 * 15.8e6 * 0.98,
  all(c("75056", "69123", "13055") %in% out$code_commune) || all(c("75101", "69381", "13201") %in% out$code_commune)
)
cat("\n✔ stopifnot() OK\n")
