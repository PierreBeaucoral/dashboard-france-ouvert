# ============================================================
# tests/check_eb_smoothing.R
# Vérification exécutable du lissage Poisson-gamma (R/socio_helpers.R::eb_poisson_gamma)
# et du jumeau R du binning (R/map_helpers.R::cmap_breaks / cmap_class).
# Lancer depuis la racine : Rscript tests/check_eb_smoothing.R
# Échoue (stopifnot) si le comportement attendu est violé.
# ============================================================
suppressPackageStartupMessages({ library(here) })
source(here::here("R", "theme.R"))
source(here::here("R", "map_helpers.R"))
source(here::here("R", "socio_helpers.R"))

set.seed(20261014)                       # seule graine du script

# ---- 1. EB : simulation Poisson-gamma, vrai prior Gamma(a = 4, b = 2e3) (moyenne 2 ‰) ----
n  <- c(rep(80, 600), rep(500, 600), rep(50000, 600))          # petites, moyennes, grandes communes
lam <- rgamma(length(n), shape = 4, rate = 2e3)
y  <- rpois(length(n), lam * n)
e  <- eb_poisson_gamma(y, n)
mu <- sum(y) / sum(n)

small <- n == 80; large <- n == 50000
# (a) petites populations : fort rétrécissement vers la moyenne (poids faible)
stopifnot(mean(e$w[small]) < 0.15)
stopifnot(mean(abs(e$eb[small] - mu)) < 0.5 * mean(abs(e$raw[small] - mu)))
# (b) grandes populations : quasiment inchangées (< 5 % d'écart relatif médian)
stopifnot(mean(e$w[large]) > 0.9)
stopifnot(median(abs(e$eb[large] - e$raw[large]) / e$raw[large]) < 0.05)
# (c) l'EB réduit l'erreur quadratique vers le vrai taux sur les petites communes
mse <- function(est, i) mean((est[i] - lam[i])^2)
stopifnot(mse(e$eb, small) < mse(e$raw, small))
# (d) le taux lissé est toujours entre le taux brut et la moyenne (combinaison convexe)
inb <- (e$eb >= pmin(e$raw, mu) - 1e-12) & (e$eb <= pmax(e$raw, mu) + 1e-12)
stopifnot(all(inb))
# (e) entrées invalides -> NA, pas d'erreur
z <- eb_poisson_gamma(c(1, NA, 3), c(0, 10, NA))
stopifnot(all(is.na(z$eb)))
# (f) sans surdispersion (pur Poisson, taux constant) : lissage complet vers la moyenne
y0 <- rpois(2000, 0.002 * 1000); e0 <- eb_poisson_gamma(y0, rep(1000, 2000))
stopifnot(all(e0$w <= 1e-9 | e0$w < 0.2))

# ---- 2. Binning : classes cohérentes, garde-fou zéros ----
v  <- c(rep(0, 700), rexp(300, 0.1))
st <- cmap_breaks(v, "quantile")
stopifnot(st$zero, length(st$breaks) == 6L, st$breaks[1] == 0, !is.unsorted(st$breaks))
cl <- cmap_class(v, st$breaks)
stopifnot(all(cl[v == 0] == 1L), all(cl %in% 1:5), all(cl[v > 0] >= 1L))
# les classes 2..5 reçoivent chacune ~ un quart des valeurs positives
tab <- table(cl[v > 0 & cl >= 2]) / sum(v > 0)
stopifnot(all(abs(as.numeric(tab) - 0.25) < 0.05))

# ---- 3. Arrondissements PLM : la ville est dupliquée sur ses arrondissements absents ----
df <- data.frame(code_commune = c("75056", "69123", "13055", "01001"), value = 1:4)
ex <- .plm_expand(df)
stopifnot(nrow(ex) == 4 + 20 + 9 + 16, sum(ex$value[grepl("^751", ex$code_commune)]) == 20 * 1L + 0L)
stopifnot(!anyDuplicated(ex$code_commune))

cat("check_eb_smoothing.R : OK\n")
