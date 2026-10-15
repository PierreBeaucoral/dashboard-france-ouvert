# ============================================================
# R/build_thumbnails.R
# Pré-rendu Quarto : vignettes cartographiques de la page d'accueil.
#
# Chaque vignette est la VRAIE carte de la thématique (indicateur par défaut,
# mêmes palettes et mêmes seuils que la page, via R/theme.R et
# cmap_breaks()), dessinée en PNG statique. Aucune image d'illustration.
# NA : gris froid hachuré non reproductible à cette échelle -> gris clair.
# Sorties : assets/thumbs/<theme>.png (régénérées à chaque build).
# Un échec sur une vignette n'interrompt pas le build.
# ============================================================

suppressPackageStartupMessages({
  library(sf); library(arrow); library(dplyr); library(here)
})
source(here::here("R", "theme.R"))
source(here::here("R", "map_helpers.R"))

out_dir <- here::here("assets", "thumbs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

com <- sf::read_sf(here::here("data", "processed", "geo", "communes.geojson"), quiet = TRUE)
dep <- sf::read_sf(here::here("data", "processed", "geo", "departements.geojson"), quiet = TRUE)
com <- com[!substr(com$code, 1, 3) %in% c("971", "972", "973", "974", "976"), ]
dep <- dep[!dep$code %in% c("971", "972", "973", "974", "976"), ]
com <- sf::st_transform(com, 2154)            # Lambert-93 : France non déformée
dep <- sf::st_transform(dep, 2154)
merged <- arrow::read_parquet(here::here("data", "processed", "explorer", "commune_merged.parquet"))
mun    <- arrow::read_parquet(here::here("data", "processed", "municipales_2026", "resultats.parquet"))

NA_COL <- "#E4E6E8"

#' Dessine une vignette à partir d'un vecteur de couleurs (aligné sur `com`).
draw_thumb <- function(name, cols) {
  f <- file.path(out_dir, paste0(name, ".png"))
  tryCatch({
    grDevices::png(f, width = 560, height = 520, res = 144, type = "cairo", bg = "white")
    on.exit(grDevices::dev.off(), add = TRUE)
    par(mar = c(0, 0, 0, 0))
    plot(sf::st_geometry(com), col = cols, border = NA, reset = FALSE)
    plot(sf::st_geometry(dep), add = TRUE, border = adjustcolor(TOK[["ink"]], 0.45), lwd = 0.35)
    message("[thumb] ", f)
  }, error = function(e) message("[thumb] ECHEC ", name, " : ", conditionMessage(e)))
}

by_code <- function(df, col) df[[col]][match(com$code, df$code_commune)]

#' Couleurs d'un indicateur quantitatif (mêmes règles que les pages).
ramp_cols <- function(v, palette, scale = "quantile") {
  st <- cmap_breaks(v, scale)
  ifelse(is.finite(v), palette[cmap_class(v, st$breaks)], NA_COL)
}

# Politique
fam_cols <- function(fam, pal) ifelse(is.na(fam), NA_COL, unname(pal[as.character(fam)]))
draw_thumb("elections",   fam_cols(by_code(merged, "famille_vainqueur"), palette_pol))
draw_thumb("municipales", fam_cols(mun$famille_vainqueur[match(com$code, mun$code_insee)], .mun_palette))

# Quantitatifs
draw_thumb("immobilier",  ramp_cols(by_code(merged, "dvf_prix_mai"), PAL$green))   # défaut de la page : Maison
atm <- by_code(merged, "air_qual")
draw_thumb("air",         ifelse(is.finite(atm) & round(atm) %in% 1:6, ATMO_COLORS[as.character(round(atm))], NA_COL))
draw_thumb("securite",    ramp_cols(by_code(merged, "del_cambriolages"), PAL$red, "log_quantile"))   # indicateur par défaut de la page
draw_thumb("finances",    ramp_cols(by_code(merged, "fin_recettes"), PAL$blue))
apl <- by_code(merged, "apl_medecins")
draw_thumb("sante",       ifelse(is.finite(apl), PAL$div[cmap_class(apl, c(0, 1.5, 2.5, 3.5, 5, 50))], NA_COL))
draw_thumb("revenus",     ramp_cols(by_code(merged, "revenu_median"), PAL$green))
draw_thumb("demographie", ramp_cols(by_code(merged, "ctx_pop_latest"), PAL$green, "log_quantile"))
draw_thumb("energie",     ramp_cols(by_code(merged, "carb_hab_total"), PAL$ochre))
draw_thumb("logement",    ramp_cols(by_code(merged, "pct_res_secondaires"), PAL$green))
draw_thumb("risques",     ramp_cols(by_code(merged, "n_catnat_since_2015"), PAL$red))
draw_thumb("services",    ramp_cols(by_code(merged, "bpe_panier_score"), PAL$blue))
draw_thumb("mobilite",    ramp_cols(by_code(merged, "mob_pct_voiture"), PAL$green))
draw_thumb("agriculture", ramp_cols(by_code(merged, "bio_surface_ha"), PAL$green, "log_quantile"))

# Bivariée : terciles croisés RN x prix m² appartement (défaut de la page)
x <- by_code(merged, "elec_pct_rn"); y <- by_code(merged, "dvf_prix_app")
ok <- is.finite(x) & is.finite(y)
tx <- stats::quantile(x[ok], c(1/3, 2/3)); ty <- stats::quantile(y[ok], c(1/3, 2/3))
xb <- 1 + (x >= tx[1]) + (x >= tx[2]); yb <- 1 + (y >= ty[1]) + (y >= ty[2])
biv <- unname(TOK[paste0("biv-", yb, xb)])
draw_thumb("bivariate", ifelse(ok, biv, NA_COL))

# ---- Vignettes des analyses croisées : vrais graphiques, pas des cartes ----
# Dessinées en base R avec les mêmes tokens ; données = les artefacts de scripts/05.
fig_thumb <- function(name, expr) {
  f <- file.path(out_dir, paste0(name, ".png"))
  tryCatch({
    grDevices::png(f, width = 560, height = 520, res = 144, type = "cairo", bg = "white")
    on.exit(grDevices::dev.off(), add = TRUE)
    par(mar = c(0.4, 0.4, 0.4, 0.4)); force(expr)
    message("[thumb] ", f)
  }, error = function(e) message("[thumb] ECHEC ", name, " : ", conditionMessage(e)))
}
corr <- arrow::read_parquet(here::here("data", "processed", "explorer", "correlations.parquet"))
cr <- corr[corr$method == "spearman" & corr$strate == "Toutes", ]

# Explorer : mini-heatmap de Spearman (24 indicateurs, ordre hiérarchique)
vv <- c("elec_pct_rn", "elec_pct_nfp", "elec_abstention", "dvf_prix_app", "dvf_prix_mai", "revenu_median", "pct_imposes",
        "gini", "apl_medecins", "carb_hab_total", "mob_pct_voiture", "mob_pct_velo", "air_pm25", "defm_taux_pop",
        "acc_taux_pop", "densite_rang", "pct_vacants", "pct_proprietaires", "bpe_panier_score", "arcep_pct_ftth",
        "ore_elec_res_mwh_hab", "caf_pct_pop_rsa", "enaf_ha_par_1000hab_2011_2025", "pct_res_secondaires")
vv <- vv[vv %in% unique(c(cr$v1, cr$v2))]
M <- matrix(NA_real_, length(vv), length(vv), dimnames = list(vv, vv)); diag(M) <- 1
for (i in seq_len(nrow(cr))) if (cr$v1[i] %in% vv && cr$v2[i] %in% vv) M[cr$v1[i], cr$v2[i]] <- M[cr$v2[i], cr$v1[i]] <- cr$rho[i]
ord <- tryCatch(stats::hclust(stats::as.dist(1 - abs(replace(M, is.na(M), 0))))$order, error = function(e) seq_along(vv))
M <- M[ord, ord]
dv <- PAL$div; cp <- grDevices::colorRampPalette(c(dv[1], dv[3], dv[5]))(101)
fig_thumb("explorer", {
  image(seq_len(nrow(M)), seq_len(ncol(M)), M[, rev(seq_len(ncol(M)))], zlim = c(-1, 1), col = cp, axes = FALSE, xlab = "", ylab = "")
  box(col = TOK[["rule"]])
})

# Profil : radar à 8 axes (commune fictive illustrative = percentiles de Lyon si disponible, sinon médiane)
rad_vars <- c("revenu_median", "dvf_prix_app", "apl_medecins", "mob_pct_voiture", "air_pm25", "pct_proprietaires", "bpe_panier_score", "arcep_pct_ftth")
pr <- vapply(rad_vars, function(v) { x <- merged[[v]]; r <- match("69123", merged$code_commune)
  if (is.na(r) || is.na(x[r])) 0.5 else stats::ecdf(x[is.finite(x)])(x[r]) }, numeric(1))
fig_thumb("profil", {
  th <- seq(pi / 2, by = -2 * pi / 8, length.out = 8)
  plot.new(); plot.window(xlim = c(-1.25, 1.25), ylim = c(-1.25, 1.25), asp = 1)
  for (r in c(.25, .5, .75, 1)) polygon(r * cos(c(th, th[1])), r * sin(c(th, th[1])), border = TOK[["rule"]], lwd = 1)
  segments(0, 0, cos(th), sin(th), col = TOK[["rule"]])
  polygon(pr * cos(th), pr * sin(th), col = adjustcolor(PAL$green[4], 0.45), border = PAL$green[5], lwd = 2)
  polygon(0.5 * cos(th), 0.5 * sin(th), border = TOK[["ink-3"]], lty = 2)
})

# Semblables : trois distributions horizontales, la commune (point) et ses semblables (traits)
# Pairs de Clermont-Ferrand approchés par la taille (vignette illustrative ; la page calcule les vrais pairs).
sm_vars <- c("revenu_median", "dvf_prix_mai", "apl_medecins")
sm_r <- match("63113", merged$code_commune)
sm_ok <- is.finite(merged$ctx_log_pop) & merged$densite_rang %in% 6
sm_peer <- order(abs(merged$ctx_log_pop - merged$ctx_log_pop[sm_r]) + ifelse(sm_ok, 0, 9))[2:16]
fig_thumb("semblables", {
  plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 3))
  for (j in seq_along(sm_vars)) {
    x <- merged[[sm_vars[j]]]; q <- stats::quantile(x, c(.01, .99), na.rm = TRUE)
    sc <- function(v) pmin(1, pmax(0, (v - q[1]) / (q[2] - q[1])))
    dd <- stats::density(sc(x[is.finite(x)]), from = 0, to = 1, adjust = 1.5)
    y0 <- 3 - j + 0.1
    polygon(c(dd$x, 1, 0), y0 + c(dd$y / max(dd$y) * 0.6, 0, 0), col = adjustcolor(TOK[["ink-3"]], 0.3), border = NA)
    segments(sc(x[sm_peer]), y0, sc(x[sm_peer]), y0 + 0.3, col = TOK[["ink-2"]], lwd = 1.5)
    points(sc(x[sm_r]), y0 + 0.15, pch = 21, bg = TOK[["ink"]], col = "white", cex = 2.4, lwd = 2)
  }
})

# Classements : barres horizontales top 10 (revenu médian, communes >= 1 000 hab.)
tp <- merged[is.finite(merged$revenu_median) & is.finite(merged$ctx_pop_latest) & merged$ctx_pop_latest >= 1000, ]
tp <- tp[order(-tp$revenu_median), ][1:10, ]
fig_thumb("classements", {
  par(mar = c(1, 1, 1, 1))
  barplot(rev(tp$revenu_median), horiz = TRUE, col = c(rep(PAL$green[5], 10)), border = NA, axes = FALSE,
          space = 0.35, xlim = c(0, max(tp$revenu_median) * 1.05))
})

# Départements : petits multiples (une tuile par département, barre = revenu pondéré)
da <- arrow::read_parquet(here::here("data", "processed", "explorer", "departements_agg.parquet"))
da <- da[order(da$code_dept), ]; da <- da[is.finite(da$revenu), ]
fig_thumb("departements", {
  n <- nrow(da); nc <- 10; nr <- ceiling(n / nc)
  plot.new(); plot.window(xlim = c(0, nc), ylim = c(nr, 0))
  v <- da$revenu; cls <- cmap_class(v, cmap_breaks(v, "quantile")$breaks)
  for (i in seq_len(n)) {
    x <- (i - 1) %% nc; y <- (i - 1) %/% nc
    rect(x + 0.06, y + 0.06, x + 0.94, y + 0.94, col = PAL$green[cls[i]], border = NA)
  }
})

# Constellation : les départements en cercles de Dorling (surface ∝ population), couleur = revenu médian pondéré
cj <- here::here("assets", "data", "constellation.json")
if (file.exists(cj)) {
  cs <- jsonlite::fromJSON(cj)
  dd <- da[match(cs$dep$c, da$code_dept), ]
  ok <- is.finite(cs$dep$dx) & is.finite(dd$revenu)
  cl <- cmap_class(dd$revenu[ok], cmap_breaks(dd$revenu[ok], "quantile")$breaks)
  fig_thumb("constellation", {
    plot.new(); plot.window(xlim = c(0, cs$w), ylim = c(cs$hm, 0), asp = 1)
    symbols(cs$dep$dx[ok], cs$dep$dy[ok], circles = cs$dep$r[ok], inches = FALSE, add = TRUE,
            bg = PAL$green[cl], fg = "white", lwd = 0.6)
  })
}
