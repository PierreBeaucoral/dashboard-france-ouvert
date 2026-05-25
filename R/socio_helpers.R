# ============================================================
# R/socio_helpers.R
#
# Helpers génériques pour les onglets Sécurité (SSMSI) et Finances
# (DGFiP). Pattern identique : choroplèthe sequentielle + radio selector
# JS + panneau cliquable.
# ============================================================

# Palettes sequentielles 5 classes (calées sur l'identité visuelle)
.palette_red  <- c("#F5F2EC", "#E8C3B0", "#D49687", "#B85F5E", "#7A2424")
.palette_blue <- c("#F5F2EC", "#C9D4DE", "#8FA8BC", "#5A7D97", "#2A4D63")
.socio_na_color <- "#E8E4DC"

#' Choroplèthe générique pour un indicateur socio-éco (1 valeur par commune).
#'
#' @param com_sf      sf des communes (métropole), avec colonne `code`.
#' @param dep_sf      sf des départements (sur-couche).
#' @param values_df   tibble (code_commune, value) à joindre.
#' @param palette     vecteur 5 couleurs.
#' @param legend_title texte de légende.
#' @param value_unit   suffixe d'unité ("‰", "€/hab", "%").
#' @param value_fmt    fonction de formatage pour le tooltip.
choropleth_socio <- function(com_sf, dep_sf, values_df,
                             palette = .palette_red,
                             legend_title = "Indicateur",
                             value_unit = "",
                             value_fmt = function(x) format(round(x, 1),
                                                            big.mark = " ",
                                                            decimal.mark = ","),
                             breaks = NULL,
                             scale = c("quantile", "log_quantile", "linear")) {
  joined <- com_sf |>
    dplyr::left_join(values_df, by = c("code" = "code_commune"))

  vals <- joined$value
  scale <- match.arg(scale)

  if (is.null(breaks)) {
    if (scale == "log_quantile") {
      # Quantiles sur log(1+v) — donne plus de résolution dans les valeurs
      # élevées d'une distribution skewée (cas typique : taux délinquance,
      # consommation énergie, etc.)
      logv <- log1p(vals[!is.na(vals) & vals >= 0])
      logbr <- unname(quantile(logv, probs = seq(0, 1, by = 0.2), na.rm = TRUE))
      breaks <- expm1(logbr)
    } else if (scale == "linear") {
      breaks <- seq(min(vals, na.rm=TRUE), max(vals, na.rm=TRUE), length.out = 6)
    } else {
      breaks <- unname(quantile(vals, probs = seq(0, 1, by = 0.2),
                                na.rm = TRUE))
    }
    if (length(unique(breaks)) < 6) {
      breaks <- seq(min(vals, na.rm=TRUE), max(vals, na.rm=TRUE), length.out = 6)
    }
  }

  pick_color <- function(v) {
    ifelse(
      is.na(v), .socio_na_color,
      palette[pmin(5L, pmax(1L,
        findInterval(v, breaks[-1], left.open = FALSE) + 1L))]
    )
  }

  fillcols <- pick_color(vals)
  fillop   <- ifelse(is.na(vals), 0.30, 0.85)

  tooltips <- sprintf(
    "<div style='font-family:Inter,sans-serif;'>
       <strong>%s</strong> <span style='color:#7A7A7A;'>(%s)</span><br>
       <span style='font-family:IBM Plex Mono,monospace;'>%s%s</span>
     </div>",
    htmltools::htmlEscape(joined$nom),
    joined$code,
    ifelse(is.na(vals), "—", vapply(vals, value_fmt, character(1))),
    ifelse(is.na(vals), "", paste0(" ", value_unit))
  ) |> lapply(htmltools::HTML)

  leaflet::leaflet(
    joined,
    options = leaflet::leafletOptions(
      preferCanvas = TRUE, zoomControl = TRUE,
      attributionControl = FALSE, minZoom = 4, maxZoom = 12
    )
  ) |>
    .fit_to_sf(joined, padding = c(20, 20)) |>
    leaflet::addPolygons(
      data         = joined,
      fillColor    = fillcols,
      fillOpacity  = fillop,
      color        = "#FFFFFF",
      weight       = 0.1,
      opacity      = 0.4,
      smoothFactor = 0.4,
      label        = tooltips,
      labelOptions = leaflet::labelOptions(
        style = list(
          "padding"    = "6px 10px",
          "background" = "#FFFFFF",
          "border"     = "1px solid #DCD8CF",
          "box-shadow" = "0 2px 6px rgba(0,0,0,0.06)"
        ),
        direction = "auto",
        offset    = c(8, 0)
      ),
      highlightOptions = leaflet::highlightOptions(
        weight       = 1.6,
        color        = "#1A1A1A",
        bringToFront = TRUE
      ),
      layerId = joined$code
    ) |>
    leaflet::addPolygons(
      data         = dep_sf,
      fill         = FALSE,
      color        = "#1A1A1A",
      weight       = 0.6,
      opacity      = 0.55,
      smoothFactor = 0.4,
      options      = leaflet::pathOptions(interactive = FALSE)
    ) |>
    leaflet::addControl(
      html      = .socio_legend_html(breaks, palette, legend_title, value_unit, value_fmt),
      position  = "bottomright",
      className = "socio-legend"
    ) |>
    .add_resize_handler()
}

.socio_legend_html <- function(breaks, palette, title, unit, fmt) {
  labels <- vapply(seq_len(5), function(i) {
    sprintf("%s – %s%s",
            fmt(breaks[i]), fmt(breaks[i + 1]),
            if (nzchar(unit)) paste0(" ", unit) else "")
  }, character(1))
  rows <- paste0(
    "<div class='lgd-row'>",
    "<span class='lgd-swatch' style='background:", palette, ";'></span>",
    "<span class='lgd-label'>", labels, "</span>",
    "<span class='lgd-hex'>", toupper(palette), "</span>",
    "</div>",
    collapse = ""
  )
  htmltools::HTML(paste0(
    "<div class='lgd-box'>",
    "<div class='lgd-title'>", title, "</div>",
    rows,
    "<div class='lgd-row' style='margin-top:6px;border-top:1px dashed #DCD8CF;padding-top:6px;'>",
    "<span class='lgd-swatch' style='background:", .socio_na_color, ";'></span>",
    "<span class='lgd-label' style='color:#7A7A7A;'>Non publié</span>",
    "<span class='lgd-hex'>", toupper(.socio_na_color), "</span>",
    "</div>",
    "</div>"
  ))
}
