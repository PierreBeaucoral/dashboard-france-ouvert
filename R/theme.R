# ============================================================
# R/theme.R
# Palettes + thème ggplot2.
#
# SOURCE UNIQUE : styles/tokens.scss. Ce fichier ne contient AUCUN hex :
# il parse les lignes `$nom: #RRGGBB;` du fichier de tokens (le même que
# Sass compile pour le CSS du site). Changer une couleur = éditer
# tokens.scss, rien d'autre.
# ============================================================

#' Lit styles/tokens.scss et renvoie un vecteur nommé de couleurs.
#'
#' @param path Chemin du fichier de tokens (relatif à la racine du projet).
#' @return Vecteur nommé (noms sans `$`), valeurs en "#RRGGBB" majuscules.
read_tokens <- function(path = here::here("styles", "tokens.scss")) {
  stopifnot(file.exists(path))
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  hit   <- regmatches(lines, regexec(
    "^\\$([A-Za-z0-9_-]+):\\s*(#[0-9A-Fa-f]{6})\\s*;", lines))
  hit   <- hit[lengths(hit) == 3L]
  vals  <- vapply(hit, function(h) toupper(h[3]), character(1))
  stats::setNames(vals, vapply(hit, function(h) h[2], character(1)))
}

TOK <- read_tokens()

# Rampe de N classes d'une famille de tokens (`seq-green` -> seq-green-1..5).
tok_ramp <- function(prefix, n = 5L) {
  nm <- paste0(prefix, "-", seq_len(n))
  stopifnot(all(nm %in% names(TOK)))
  unname(TOK[nm])
}

# ---- Palettes du site (toutes dérivées des tokens) ----

palette_brand <- list(
  paper      = unname(TOK["paper"]),
  surface    = unname(TOK["surface"]),
  surface_2  = unname(TOK["surface-2"]),
  rule       = unname(TOK["rule"]),
  rule_strong = unname(TOK["rule-strong"]),
  ink        = unname(TOK["ink"]),
  ink_soft   = unname(TOK["ink-2"]),
  ink_muted  = unname(TOK["ink-3"]),
  na_fill    = unname(TOK["na-fill"]),
  na_line    = unname(TOK["na-line"])
)

palette_pol <- c(
  NFP    = unname(TOK["pol-nfp"]),
  ENS    = unname(TOK["pol-ens"]),
  RN     = unname(TOK["pol-rn"]),
  LR     = unname(TOK["pol-lr"]),
  Divers = unname(TOK["pol-divers"])
)

PAL <- list(
  green  = tok_ramp("seq-green"),   # grandeur : prix, revenus, bio, mobilité
  red    = tok_ramp("seq-red"),     # risque : sécurité
  blue   = tok_ramp("seq-blue"),    # finances
  ochre  = tok_ramp("seq-ochre"),   # énergie / carbone
  div    = tok_ramp("div"),         # divergente brun -> sarcelle
  dens   = tok_ramp("dens", 6L)     # typologie de densité INSEE
)

# Compatibilité avec l'ancien nom (séquentielle principale, divergente).
palette_seq <- PAL$green
palette_div <- PAL$div

# Bivariée 3x3 indexée (ligne = classe Y, colonne = classe X)
palette_bivariate_3x3 <- matrix(
  unname(TOK[paste0("biv-", rep(1:3, each = 3), rep(1:3, times = 3))]),
  nrow = 3, byrow = TRUE,
  dimnames = list(paste0("X", 1:3), paste0("Y", 1:3))
)

# Indice ATMO officiel 0..6
ATMO_COLORS <- stats::setNames(unname(TOK[paste0("atmo-", 0:6)]), as.character(0:6))

NA_FILL <- unname(TOK["na-fill"])   # fond des hachures "non publié"
NA_LINE <- unname(TOK["na-line"])   # trait des hachures

# ---- ggplot2 thème (chrome achromatique) ----

theme_dashboard <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "IBM Plex Sans") +
    ggplot2::theme(
      plot.background   = ggplot2::element_rect(fill = palette_brand$surface, colour = NA),
      panel.background  = ggplot2::element_rect(fill = palette_brand$surface, colour = NA),
      panel.grid.major  = ggplot2::element_line(colour = palette_brand$rule, linewidth = 0.3),
      panel.grid.minor  = ggplot2::element_blank(),
      axis.title        = ggplot2::element_text(colour = palette_brand$ink_soft,
                                                size = ggplot2::rel(0.9)),
      axis.text         = ggplot2::element_text(colour = palette_brand$ink_soft,
                                                size = ggplot2::rel(0.85)),
      plot.caption      = ggplot2::element_text(colour = palette_brand$ink_muted,
                                                size = ggplot2::rel(0.75), hjust = 0),
      legend.background = ggplot2::element_rect(fill = palette_brand$surface, colour = NA),
      legend.key        = ggplot2::element_rect(fill = palette_brand$surface, colour = NA),
      legend.title      = ggplot2::element_text(face = "bold", size = ggplot2::rel(0.85)),
      strip.background  = ggplot2::element_rect(fill = palette_brand$surface_2, colour = NA),
      strip.text        = ggplot2::element_text(face = "bold", colour = palette_brand$ink_soft)
    )
}
