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
    "<details class='lgd-box' open>",
    "<summary class='lgd-title'>", title, "</summary>",
    rows,
    "<div class='lgd-row' style='margin-top:6px;border-top:1px dashed #DCD8CF;padding-top:6px;'>",
    "<span class='lgd-swatch' style='background:", .socio_na_color, ";'></span>",
    "<span class='lgd-label' style='color:#7A7A7A;'>Non publié</span>",
    "<span class='lgd-hex'>", toupper(.socio_na_color), "</span>",
    "</div>",
    "</details>"
  ))
}

#' Choroplèthe catégorielle (typologie densité INSEE, etc.).
#'
#' @param com_sf       sf communes métropole.
#' @param dep_sf       sf départements (sur-couche).
#' @param values_df    tibble (code_commune, value) — `value` chaîne.
#' @param levels       niveaux ordonnés (rural → urbain).
#' @param palette      couleurs (length(levels)).
#' @param legend_title titre de la légende.
choropleth_socio_cat <- function(com_sf, dep_sf, values_df,
                                 levels, palette,
                                 legend_title = "Catégorie") {
  joined <- com_sf |>
    dplyr::left_join(values_df, by = c("code" = "code_commune"))

  ix <- match(as.character(joined$value), levels)
  fillcols <- ifelse(is.na(ix), .socio_na_color, palette[ix])
  fillop   <- ifelse(is.na(ix), 0.30, 0.85)

  tooltips <- sprintf(
    "<div style='font-family:Inter,sans-serif;'>
       <strong>%s</strong> <span style='color:#7A7A7A;'>(%s)</span><br>
       <span style='font-family:IBM Plex Mono,monospace;'>%s</span>
     </div>",
    htmltools::htmlEscape(joined$nom),
    joined$code,
    ifelse(is.na(joined$value), "—",
           htmltools::htmlEscape(as.character(joined$value)))
  ) |> lapply(htmltools::HTML)

  rows <- paste0(
    "<div class='lgd-row'>",
    "<span class='lgd-swatch' style='background:", palette, ";'></span>",
    "<span class='lgd-label'>", levels, "</span>",
    "<span class='lgd-hex'>", toupper(palette), "</span>",
    "</div>",
    collapse = ""
  )
  lgd_html <- htmltools::HTML(paste0(
    "<details class='lgd-box' open>",
    "<summary class='lgd-title'>", legend_title, "</summary>",
    rows,
    "<div class='lgd-row' style='margin-top:6px;border-top:1px dashed #DCD8CF;padding-top:6px;'>",
    "<span class='lgd-swatch' style='background:", .socio_na_color, ";'></span>",
    "<span class='lgd-label' style='color:#7A7A7A;'>Non publié</span>",
    "<span class='lgd-hex'>", toupper(.socio_na_color), "</span>",
    "</div></details>"
  ))

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
      html      = lgd_html,
      position  = "bottomright",
      className = "socio-legend"
    ) |>
    .add_resize_handler()
}

#' Cartouche DROM avec choroplèthe socio générique.
#'
#' Variante de `choropleth_drom_inset()` (élections) qui prend des valeurs
#' continues + palette/breaks. Utilisée pour Sécurité, Finances, Santé,
#' Revenus, Démographie. Calcul des classes identique à `choropleth_socio()`
#' pour cohérence visuelle entre la grande carte et les cartouches.
#'
#' @param com_drom_sf  sf des communes DROM (avec colonne `code`).
#' @param drom_code    "971", "972", "973", "974", "976".
#' @param values_df    tibble (code_commune, value).
#' @param palette      5 couleurs (séquentielle).
#' @param breaks       6 breaks (output `choropleth_socio()`). Optionnel —
#'                     si NULL, calculé localement sur le DROM.
#' @param scale        "quantile" | "log_quantile" | "linear".
choropleth_drom_socio_inset <- function(com_drom_sf, drom_code, values_df,
                                        palette = .palette_red,
                                        breaks  = NULL,
                                        scale   = c("quantile",
                                                    "log_quantile",
                                                    "linear")) {
  scale <- match.arg(scale)
  one <- com_drom_sf[substr(com_drom_sf$code, 1, 3) == drom_code, ]
  one <- one |>
    dplyr::left_join(values_df, by = c("code" = "code_commune"))

  vals <- one$value
  if (is.null(breaks)) {
    finite_vals <- vals[is.finite(vals)]
    if (length(finite_vals) < 2L) {
      # Garde-fou : pas assez de valeurs → breaks dégénérés, tout en couleur NA.
      breaks <- c(0, 1, 2, 3, 4, 5)
    } else if (scale == "log_quantile") {
      logv <- log1p(finite_vals[finite_vals >= 0])
      if (length(logv) >= 5L) {
        logbr <- unname(quantile(logv, probs = seq(0, 1, by = 0.2),
                                 na.rm = TRUE))
        breaks <- expm1(logbr)
      } else {
        breaks <- seq(0, max(finite_vals) + 1, length.out = 6)
      }
    } else if (scale == "linear") {
      breaks <- seq(min(finite_vals), max(finite_vals),
                    length.out = 6)
    } else {
      breaks <- unname(quantile(finite_vals, probs = seq(0, 1, by = 0.2),
                                na.rm = TRUE))
    }
    if (length(unique(breaks)) < 6) {
      breaks <- seq(
        if (length(finite_vals) > 0L) min(finite_vals) else 0,
        if (length(finite_vals) > 0L) max(finite_vals) + 1 else 5,
        length.out = 6
      )
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

  leaflet::leaflet(
    one,
    options = leaflet::leafletOptions(
      preferCanvas       = TRUE,
      zoomControl        = FALSE,
      attributionControl = FALSE,
      dragging           = FALSE,
      scrollWheelZoom    = FALSE,
      doubleClickZoom    = FALSE,
      boxZoom            = FALSE,
      touchZoom          = FALSE,
      keyboard           = FALSE
    )
  ) |>
    .fit_to_sf(one, padding = c(6, 6)) |>
    leaflet::addPolygons(
      fillColor    = fillcols,
      fillOpacity  = fillop,
      color        = "#FFFFFF",
      weight       = 0.3,
      smoothFactor = 0.3
    ) |>
    .add_resize_handler()
}

#' Cartouche DROM avec une palette catégorielle (typologie densité INSEE).
#'
#' @param com_drom_sf  sf communes DROM, contient `code`.
#' @param drom_code    "971"..."976".
#' @param values_df    tibble (code_commune, value) où `value` est un facteur
#'                     ou chaîne avec niveaux dans `levels`.
#' @param levels       Niveaux ordonnés (1 → n).
#' @param palette      Couleurs (length(levels)).
choropleth_drom_cat_inset <- function(com_drom_sf, drom_code, values_df,
                                      levels, palette) {
  one <- com_drom_sf[substr(com_drom_sf$code, 1, 3) == drom_code, ]
  one <- one |>
    dplyr::left_join(values_df, by = c("code" = "code_commune"))
  ix <- match(as.character(one$value), levels)
  fillcols <- ifelse(is.na(ix), .socio_na_color, palette[ix])
  fillop   <- ifelse(is.na(ix), 0.30, 0.85)

  leaflet::leaflet(
    one,
    options = leaflet::leafletOptions(
      preferCanvas       = TRUE,
      zoomControl        = FALSE,
      attributionControl = FALSE,
      dragging           = FALSE,
      scrollWheelZoom    = FALSE,
      doubleClickZoom    = FALSE,
      boxZoom            = FALSE,
      touchZoom          = FALSE,
      keyboard           = FALSE
    )
  ) |>
    .fit_to_sf(one, padding = c(6, 6)) |>
    leaflet::addPolygons(
      fillColor    = fillcols,
      fillOpacity  = fillop,
      color        = "#FFFFFF",
      weight       = 0.3,
      smoothFactor = 0.3
    ) |>
    .add_resize_handler()
}
