# ============================================================
# R/dorling.R : disposition « Dorling » (cercles sans chevauchement)
#
# dorling_pack(x, y, r, rect) place chaque cercle au plus près de sa position
# géographique (x, y) sans jamais en chevaucher un autre. Algorithme glouton :
# du plus grand au plus petit, un cercle est posé sur sa position idéale ; s'il
# gêne, on teste des positions sur des anneaux de rayon croissant (pas = rayon du
# cercle) et on retient la première libre dont le cercle tient dans `rect`.
# Aucun chevauchement par construction (contrôlé : tests/check_constellation.js),
# chaque cercle est placé (anneaux jusqu'à trouver ; repli hors `rect` en dernier recours).
# Déterministe, sans graine. Complexité : grille de pas `cs` pour les petits
# cercles (r <= rbig), liste linéaire pour les grands.
# x, y, r : vecteurs ; rect : c(xmin, xmax, ymin, ymax). Renvoie list(x, y).
# ============================================================
dorling_pack <- function(x, y, r, rect, rbig = 4, cs = 8) {
  n <- length(x); px <- numeric(n); py <- numeric(n)
  ord <- order(-r, seq_len(n))
  big_x <- numeric(0); big_y <- numeric(0); big_r <- numeric(0)
  nx <- ceiling((rect[2] - rect[1] + 400) / cs); ny <- ceiling((rect[4] - rect[3] + 400) / cs)
  ox <- rect[1] - 200; oy <- rect[3] - 200
  grid <- vector("list", nx * ny)               # cellule -> indices (petits cercles posés)
  cell <- function(xx, yy) {
    cx <- pmin(nx - 1, pmax(0, floor((xx - ox) / cs))); cy <- pmin(ny - 1, pmax(0, floor((yy - oy) / cs)))
    cy * nx + cx + 1
  }
  free <- function(cx, cy, cr) {
    if (length(big_x) && any((big_x - cx)^2 + (big_y - cy)^2 < (big_r + cr)^2 - 1e-9)) return(FALSE)
    gx <- floor((cx - ox) / cs); gy <- floor((cy - oy) / cs)
    reach <- ceiling((cr + rbig) / cs)
    for (j in (gy - reach):(gy + reach)) for (i in (gx - reach):(gx + reach)) {
      if (i < 0 || j < 0 || i >= nx || j >= ny) next
      ids <- grid[[j * nx + i + 1]]
      if (length(ids) && any((px[ids] - cx)^2 + (py[ids] - cy)^2 < (r[ids] + cr)^2 - 1e-9)) return(FALSE)
    }
    TRUE
  }
  inside <- function(cx, cy, cr) cx - cr >= rect[1] && cx + cr <= rect[2] && cy - cr >= rect[3] && cy + cr <= rect[4]
  for (k in ord) {
    cr <- r[k]; ix <- x[k]; iy <- y[k]; done <- FALSE
    # position idéale ramenée dans le cadre (un point côtier peut sortir du rect)
    ix <- min(max(ix, rect[1] + cr), rect[2] - cr); iy <- min(max(iy, rect[3] + cr), rect[4] - cr)
    if (free(ix, iy, cr)) { px[k] <- ix; py[k] <- iy; done <- TRUE }
    rho <- 0; ring <- 0
    while (!done) {
      ring <- ring + 1; rho <- ring * max(cr * 0.9, 0.35)
      na <- max(6L, min(48L, as.integer(ceiling(2 * pi * rho / max(cr, 0.35)))))
      th <- (seq_len(na) - 1) * 2 * pi / na + ring * 2.399963   # angle d'or : évite l'alignement
      cx <- ix + rho * cos(th); cy <- iy + rho * sin(th)
      ok <- which(vapply(seq_len(na), function(a) inside(cx[a], cy[a], cr) || ring > 400, logical(1)))
      for (a in ok) if (free(cx[a], cy[a], cr)) { px[k] <- cx[a]; py[k] <- cy[a]; done <- TRUE; break }
    }
    if (cr > rbig) { big_x <- c(big_x, px[k]); big_y <- c(big_y, py[k]); big_r <- c(big_r, cr) }
    else { c0 <- cell(px[k], py[k]); grid[[c0]] <- c(grid[[c0]], k) }
  }
  list(x = px, y = py)
}
