# ============================================================
# scripts/23_caf_allocataires.R
#
# CAF : allocataires et personnes couvertes par prestation, par commune.
#
# Source : CNAF, data.caf.fr, jeu "s_ben_com_f" — Toutes prestations, répartition
#   des allocataires selon la prestation [communal] (licence Ouverte 2.0)
#   https://data.caf.fr/api/explore/v2.1/catalog/datasets/s_ben_com_f/exports/csv
#   Mois de droit = décembre de l'année N (dernier millésime lu à l'exécution :
#   2024-12) ; géographie = COG au 1er janvier N+1.
#
# Définitions : indfoy_* = foyers allocataires ; indnbp_* = personnes couvertes
# (allocataire + conjoint + enfants) ; compteurs arrondis à 5 par la CNAF.
# Prestations retenues : RSA, PPA (prime d'activité), AF (allocations familiales),
# aides au logement (AL = ndural = APL+ALF+ALS), ensemble des prestations (ndur).
#
# Règles :
#   - harmonisation COG 2025 -> 2026 : comptes sommés (somme de multiples de 5 =
#     multiple de 5), taux calculés après ;
#   - taux = personnes couvertes / population (pop_latest, RP 2021) x 100,
#     affichés UNIQUEMENT si pop >= 1 000 (sinon NA) : l'arrondi à 5 et le secret
#     rendent les taux des petites communes illisibles ;
#   - Paris/Lyon/Marseille : la CAF publie par arrondissement ; on garde les
#     arrondissements ET on ajoute la commune mère (somme) ;
#   - CAVEAT : régime général CAF uniquement. Les allocataires MSA (régime
#     agricole) sont EXCLUS : taux sous-estimés dans les communes rurales/agricoles.
#   - Mayotte absente (CSSM, pas une CAF).
#   - taux > 100 % (incohérence pop RP / domiciliation CAF) mis à NA.
#   - Un taux "RSA pour 100 hab." mesure la population COUVERTE par le RSA, pas le
#     taux de pauvreté.
#
# Sortie : data/processed/insee_extra/caf_allocataires.parquet
#   code_commune, caf_annee, caf_foyers_{ndur,rsa,ppa,af,al}, caf_pers_{...},
#   caf_pct_pop_{ndur,rsa,ppa,af,al}, caf_taux_affiche (bool : pop >= 1000)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(arrow); library(here); library(curl); library(glue)
})
source(here::here("scripts", "cog_helpers.R"))

URL <- "https://data.caf.fr/api/explore/v2.1/catalog/datasets/s_ben_com_f/exports/csv?delimiter=%3B"
raw_dir <- here::here("data", "raw", "caf")
out_dir <- here::here("data", "processed", "insee_extra")
csv_path <- file.path(raw_dir, "s_ben_com_f.csv")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
if (!file.exists(csv_path) || file.size(csv_path) < 1e7) {
  message("Télécharge : ", URL)
  curl::curl_download(URL, csv_path, quiet = TRUE)
} else message("  Cache : s_ben_com_f.csv")

pres <- c(ndur = "ndur", rsa = "rsa", ppa = "ppa", af = "af", al = "ndural")
cols <- c("dtreffre", "numcomdo", as.vector(rbind(paste0("indfoy_", pres), paste0("indnbp_", pres))))
d <- readr::read_delim(csv_path, delim = ";", show_col_types = FALSE, col_select = all_of(cols),
                       col_types = readr::cols(.default = "d", dtreffre = "c", numcomdo = "c"))
last <- max(d$dtreffre)
d <- filter(d, dtreffre == last)
message("Dernier mois de droit : ", last, " ; communes : ", nrow(d))
annee <- as.integer(substr(last, 1, 4))
names(d) <- sub("^indfoy_", "caf_foyers_", sub("^indnbp_", "caf_pers_", names(d)))
names(d) <- sub("_ndural$", "_al", names(d))

pop <- arrow::read_parquet(here::here("data", "processed", "insee", "populations.parquet")) |>
  select(code_commune, type_com, pop_latest)

cmap <- cog_map("2025-01-01")
cnt_cols <- grep("^caf_(foyers|pers)_", names(d), value = TRUE)
d <- d |> mutate(code_commune = to_cog2026(numcomdo, cmap)) |>
  group_by(code_commune) |> summarise(across(all_of(cnt_cols), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

# commune mère PLM = somme des arrondissements
plm <- tibble::tribble(~mere, ~pat, "75056", "^751", "69123", "^6938", "13055", "^132")
mere <- bind_rows(lapply(seq_len(nrow(plm)), \(i) {
  d |> filter(grepl(plm$pat[i], code_commune), nchar(code_commune) == 5, code_commune != plm$mere[i]) |>
    summarise(across(all_of(cnt_cols), sum)) |> mutate(code_commune = plm$mere[i])
}))
d <- bind_rows(filter(d, !code_commune %in% plm$mere), mere)

out <- d |> inner_join(pop, "code_commune") |>
  mutate(caf_annee = annee, caf_taux_affiche = !is.na(pop_latest) & pop_latest >= 1000)
for (k in names(pres)) {
  out[[paste0("caf_pct_pop_", k)]] <-
    ifelse(out$caf_taux_affiche, round(100 * out[[paste0("caf_pers_", k)]] / out$pop_latest, 2), NA_real_)
  # taux > 100 % = incohérence population RP 2021 / domiciliation CAF (1 commune) -> NA
  out[[paste0("caf_pct_pop_", k)]][out[[paste0("caf_pct_pop_", k)]] > 100] <- NA_real_
}
out <- out |> select(code_commune, caf_annee, all_of(cnt_cols), starts_with("caf_pct_pop_"), caf_taux_affiche)

arrow::write_parquet(out, file.path(out_dir, "caf_allocataires.parquet"), compression = "zstd")
cat(glue("\n✔ {nrow(out)} communes (dont {sum(out$caf_taux_affiche)} avec taux affiché, pop >= 1000) → data/processed/insee_extra/caf_allocataires.parquet\n"))
print(summary(out[c("caf_pct_pop_rsa", "caf_pct_pop_af", "caf_pct_pop_al", "caf_pct_pop_ndur")]))

# ---- Contrôles ----
stopifnot(
  !anyDuplicated(out$code_commune),
  abs(nrow(out) / 34850 - 1) < 0.05,
  all(out[cnt_cols] >= 0), all(out[cnt_cols] %% 5 == 0),
  all(out$caf_pct_pop_ndur >= 0 & out$caf_pct_pop_ndur <= 100, na.rm = TRUE),
  all(out$caf_pct_pop_rsa >= 0 & out$caf_pct_pop_rsa <= 75, na.rm = TRUE),
  all(is.na(out$caf_pct_pop_rsa[!out$caf_taux_affiche])),
  all(c("75056", "69123", "13055") %in% out$code_commune),
  # national : ~ 6-7 % de la population couverte par le RSA (régime général)
  between(100 * sum(out$caf_pers_rsa) / sum(pop$pop_latest[pop$type_com == "COM"], na.rm = TRUE), 4, 9)
)
cat("\n✔ stopifnot() OK\n")
