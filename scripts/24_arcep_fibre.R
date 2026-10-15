# ============================================================
# scripts/24_arcep_fibre.R
#
# Couverture FttH (fibre jusqu'à l'abonné) par commune, dernier trimestre ARCEP.
#
# Source : ARCEP, "Le marché du haut et très haut débit fixe - déploiements"
#   (data.gouv.fr, licence Ouverte 2.0), ressource "{AAAA}T{T}-Commune" (shapefile
#   zippé). L'URL change à chaque trimestre : la dernière ressource est résolue via
#   l'API data.gouv.fr (dataset le-marche-du-haut-et-tres-haut-debit-fixe-deploiements).
#   Dernier trimestre vérifié : 2026 T2 (publié 2026-09-10, 34 919 lignes).
#
# Colonnes DBF utilisées : INSEE_COM, Locaux (locaux du référentiel IPE ARCEP),
# ftth (locaux raccordables FttH). Le DBF est lu par un lecteur minimal (le DBF
# contient un nom de colonne non-UTF-8 qui fait échouer sf::st_read / foreign).
#
# Règles : comptes sommés après remappage COG ; arrondissements PLM sommés vers la
# commune mère (arrondissements conservés aussi) ; taux = ftth / Locaux x 100,
# NA si Locaux < 20 ; borné à 100.
# Caveat : éligibilité théorique (raccordable), pas abonnement ; le référentiel de
# locaux évolue donc le taux peut baisser d'un trimestre à l'autre. Mayotte (97611...)
# a un taux FttH très bas : réel, pas une erreur.
#
# Sortie : data/processed/insee_extra/arcep_fibre.parquet
#   code_commune, arcep_trimestre, arcep_locaux, arcep_locaux_ftth, arcep_pct_ftth
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(arrow); library(here); library(curl); library(jsonlite); library(glue)
})
source(here::here("scripts", "cog_helpers.R"))

API <- "https://www.data.gouv.fr/api/1/datasets/le-marche-du-haut-et-tres-haut-debit-fixe-deploiements/"
raw_dir <- here::here("data", "raw", "arcep")
out_dir <- here::here("data", "processed", "insee_extra")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Résolution de la dernière ressource commune ----
res <- jsonlite::fromJSON(API)$resources
res <- res[grepl("^[0-9]{4}T[1-4][-_]commune$", res$title, ignore.case = TRUE), ]
res$trim <- toupper(substr(res$title, 1, 6))
res <- res[order(res$trim, decreasing = TRUE), ]
trim <- res$trim[1]; url <- res$url[1]
message("Dernier trimestre : ", trim, "  ", url)

zip_path <- file.path(raw_dir, paste0(tolower(trim), ".zip"))
if (!file.exists(zip_path) || file.size(zip_path) < 1e7) curl::curl_download(url, zip_path, quiet = TRUE) else message("  Cache : ", basename(zip_path))
tmp <- tempfile(); dir.create(tmp); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
dbf_name <- grep("\\.dbf$", unzip(zip_path, list = TRUE)$Name, value = TRUE, ignore.case = TRUE)
unzip(zip_path, files = dbf_name, exdir = tmp)

# ---- Lecteur DBF minimal ----
read_dbf <- function(path) {
  con <- file(path, "rb"); on.exit(close(con))
  h <- readBin(con, "raw", 32)
  n <- readBin(h[5:8], "integer", 1, size = 4, endian = "little")
  hl <- readBin(h[9:10], "integer", 1, size = 2, endian = "little")
  rl <- readBin(h[11:12], "integer", 1, size = 2, endian = "little")
  fd <- readBin(con, "raw", hl - 32)
  nf <- (hl - 32 - 1) %/% 32
  nm <- character(nf); ln <- integer(nf); ty <- character(nf)
  for (i in seq_len(nf)) {
    b <- fd[((i - 1) * 32 + 1):(i * 32)]
    nm[i] <- iconv(rawToChar(b[1:11][b[1:11] != as.raw(0)]), "UTF-8", "ASCII", sub = "_")
    ty[i] <- rawToChar(b[12]); ln[i] <- as.integer(b[17])
  }
  st <- cumsum(c(2, ln))[seq_len(nf)]; en <- st + ln - 1
  list(names = nm, types = ty, start = st, end = en, nrec = n)
}
read_dbf_cols <- function(path, cols) {
  m <- read_dbf(path)
  con <- file(path, "rb"); on.exit(close(con))
  h <- readBin(con, "raw", 32)
  hl <- readBin(h[9:10], "integer", 1, size = 2, endian = "little")
  rl <- readBin(h[11:12], "integer", 1, size = 2, endian = "little")
  seek(con, hl)
  raw <- readBin(con, "raw", m$nrec * rl)
  mat <- matrix(raw, nrow = rl)
  out <- lapply(cols, function(cn) {
    j <- match(cn, m$names)
    v <- vapply(seq_len(m$nrec), function(i) trimws(iconv(rawToChar(mat[m$start[j]:m$end[j], i]), "UTF-8", "UTF-8", sub = "?")), "")
    if (m$types[j] == "N") as.numeric(v) else v
  })
  names(out) <- cols
  tibble::as_tibble(out)
}

d <- read_dbf_cols(file.path(tmp, dbf_name), c("INSEE_COM", "Locaux", "ftth"))
message("Lignes : ", nrow(d))

pop <- arrow::read_parquet(here::here("data", "processed", "insee", "populations.parquet"))
valid <- pop$code_commune
is_com <- function(x) x %in% pop$code_commune[pop$type_com == "COM"]
cmap <- cog_map("2025-01-01")
d <- d |> mutate(code_commune = to_cog2026(INSEE_COM, cmap)) |>
  group_by(code_commune) |> summarise(arcep_locaux = sum(Locaux), arcep_locaux_ftth = sum(ftth), .groups = "drop")
plm <- tibble::tribble(~mere, ~pat, "75056", "^751", "69123", "^6938", "13055", "^132")
mere <- bind_rows(lapply(seq_len(nrow(plm)), \(i) {
  d |> filter(grepl(plm$pat[i], code_commune), code_commune != plm$mere[i]) |>
    summarise(arcep_locaux = sum(arcep_locaux), arcep_locaux_ftth = sum(arcep_locaux_ftth)) |>
    mutate(code_commune = plm$mere[i])
}))
out <- bind_rows(filter(d, !code_commune %in% plm$mere), mere) |>
  filter(code_commune %in% valid) |>
  mutate(arcep_trimestre = trim,
         arcep_pct_ftth = ifelse(arcep_locaux >= 20, pmin(100, round(100 * arcep_locaux_ftth / arcep_locaux, 2)), NA_real_)) |>
  select(code_commune, arcep_trimestre, arcep_locaux, arcep_locaux_ftth, arcep_pct_ftth)

arrow::write_parquet(out, file.path(out_dir, "arcep_fibre.parquet"), compression = "zstd")
cat(glue("\n✔ {nrow(out)} communes ({trim}) → data/processed/insee_extra/arcep_fibre.parquet\n"))
print(summary(out$arcep_pct_ftth))
cat("National FttH (pondéré locaux, communes hors arrondissements) :", round(100 * sum(out$arcep_locaux_ftth[is_com(out$code_commune)]) / sum(out$arcep_locaux[is_com(out$code_commune)]), 1), "%\n")

# ---- Contrôles ----
com <- out$code_commune
stopifnot(
  !anyDuplicated(com),
  abs(nrow(out) / 34900 - 1) < 0.05,
  all(out$arcep_pct_ftth >= 0 & out$arcep_pct_ftth <= 100, na.rm = TRUE),
  all(out$arcep_locaux_ftth <= out$arcep_locaux * 1.02),
  all(c("75056", "69123", "13055", "97611", "97411") %in% com),
  # ~ 35-40 M locaux en France ; FttH ~ 85-95 % des locaux en 2026
  between(sum(out$arcep_locaux[is_com(com)]), 30e6, 50e6)
)
cat("\n✔ stopifnot() OK\n")
