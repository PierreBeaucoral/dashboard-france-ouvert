# ============================================================
# R/theme.R
# Thème ggplot2 et palettes — alignés sur styles/custom.scss
# ============================================================

# ---- Palettes ----

palette_brand <- list(
  cream     = "#F5F2EC",
  ink       = "#1A1A1A",
  ink_soft  = "#4A4A4A",
  ink_muted = "#7A7A7A",
  rule      = "#DCD8CF",
  paper     = "#FFFFFF",
  accent    = "#2A6F97"
)

palette_pol <- c(
  NFP    = "#C73659",
  ENS    = "#E6B800",
  RN     = "#1B3A57",
  LR     = "#4A90D9",
  Divers = "#8A8A8A"
)

# Palette séquentielle pour DVF (prix m²)
palette_seq <- c("#F5F2EC", "#D4C9B0", "#9FB191", "#5B8A6F", "#2A5547")

# Palette divergente pour qualité air (autour de seuils OMS)
palette_div <- c("#2A6F97", "#7FB3D5", "#F5F2EC", "#E59866", "#A4243B")

# Palette bivariate 3x3 (Stevens-style), indexée (1,1) à (3,3)
# Stockée comme matrice pour accès direct
palette_bivariate_3x3 <- matrix(
  c(
    "#E8E8E8", "#B0D5DF", "#64ACBE",
    "#E4ACAC", "#AD9EA5", "#627F8C",
    "#C85A5A", "#985356", "#574249"
  ),
  nrow = 3, byrow = TRUE,
  dimnames = list(paste0("X", 1:3), paste0("Y", 1:3))
)

# ---- ggplot2 thème ----

theme_dashboard <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "Inter") +
    ggplot2::theme(
      plot.background       = ggplot2::element_rect(fill = palette_brand$paper,
                                                    colour = NA),
      panel.background      = ggplot2::element_rect(fill = palette_brand$paper,
                                                    colour = NA),
      panel.grid.major      = ggplot2::element_line(colour = palette_brand$rule,
                                                    linewidth = 0.3),
      panel.grid.minor      = ggplot2::element_blank(),
      axis.title            = ggplot2::element_text(colour = palette_brand$ink_soft,
                                                    size = ggplot2::rel(0.9)),
      axis.text             = ggplot2::element_text(colour = palette_brand$ink_soft,
                                                    size = ggplot2::rel(0.85),
                                                    family = "IBM Plex Mono"),
      plot.title            = ggplot2::element_text(face = "bold",
                                                    colour = palette_brand$ink,
                                                    size = ggplot2::rel(1.05),
                                                    margin = ggplot2::margin(b = 6)),
      plot.subtitle         = ggplot2::element_text(colour = palette_brand$ink_soft,
                                                    size = ggplot2::rel(0.9),
                                                    margin = ggplot2::margin(b = 10)),
      plot.caption          = ggplot2::element_text(colour = palette_brand$ink_muted,
                                                    size = ggplot2::rel(0.75),
                                                    hjust = 0),
      legend.background     = ggplot2::element_rect(fill = palette_brand$paper,
                                                    colour = NA),
      legend.key            = ggplot2::element_rect(fill = palette_brand$paper,
                                                    colour = NA),
      legend.title          = ggplot2::element_text(face = "bold",
                                                    size = ggplot2::rel(0.85)),
      strip.background      = ggplot2::element_rect(fill = palette_brand$cream,
                                                    colour = NA),
      strip.text            = ggplot2::element_text(face = "bold",
                                                    colour = palette_brand$ink_soft)
    )
}
