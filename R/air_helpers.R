# ============================================================
# R/air_helpers.R
# Helpers pour la couche Qualité de l'air (indice ATMO).
# ============================================================

# Palette officielle Atmo France (indices 0-6)
.atmo_colors <- c(
  "0" = "#BBBBBB",   # Absent
  "1" = "#50F0E6",   # Bon
  "2" = "#50CCAA",   # Moyen
  "3" = "#F0E641",   # Dégradé
  "4" = "#FF5050",   # Mauvais
  "5" = "#960032",   # Très mauvais
  "6" = "#7D2181"    # Extrêmement mauvais
)
.atmo_labels <- c(
  "0" = "Absent",
  "1" = "Bon",
  "2" = "Moyen",
  "3" = "Dégradé",
  "4" = "Mauvais",
  "5" = "Très mauvais",
  "6" = "Extrêmement mauvais"
)
.atmo_na_color <- "#E8E4DC"

#' Choroplèthe qualité de l'air (indice ATMO global par commune).
choropleth_atmo <- function(com_sf, dep_sf, atmo_df, indice_col = "qual_indice",
                            display_date = NULL) {
  joined <- com_sf |>
    dplyr::left_join(atmo_df, by = c("code" = "code_commune"))

  vals <- joined[[indice_col]]
  fillcols <- ifelse(
    is.na(vals), .atmo_na_color,
    .atmo_colors[as.character(vals)]
  )

  tooltips <- sprintf(
    "<div style='font-family:Inter,sans-serif;'>
       <strong>%s</strong> <span style='color:#7A7A7A;'>(%s)</span><br>
       <span class='atmo-pill atmo-pill-%s'>%s</span>
       <span style='color:#7A7A7A;font-size:0.82em;'> · %s</span>
     </div>",
    htmltools::htmlEscape(joined$nom),
    joined$code,
    ifelse(is.na(vals), "na", as.character(vals)),
    ifelse(is.na(vals), "—", .atmo_labels[as.character(vals)]),
    ifelse(is.na(joined$source_aasqa), "—", htmltools::htmlEscape(joined$source_aasqa))
  ) |> lapply(htmltools::HTML)

  leaflet::leaflet(
    joined,
    options = leaflet::leafletOptions(
      preferCanvas       = TRUE,
      zoomControl        = TRUE,
      attributionControl = FALSE,
      minZoom            = 4,
      maxZoom            = 12
    )
  ) |>
    .fit_to_sf(joined, padding = c(20, 20)) |>
    leaflet::addPolygons(
      data         = joined,
      fillColor    = fillcols,
      fillOpacity  = ifelse(is.na(vals), 0.25, 0.85),
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
      layerId = ~code
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
      html      = .atmo_legend_html(display_date),
      position  = "bottomright",
      className = "atmo-legend"
    ) |>
    .add_resize_handler()
}

.atmo_legend_html <- function(display_date = NULL) {
  rows <- paste0(
    "<div class='lgd-row'>",
    "<span class='lgd-swatch' style='background:", .atmo_colors[as.character(1:6)], ";'></span>",
    "<span class='lgd-label'>", as.character(1:6), " — ", .atmo_labels[as.character(1:6)], "</span>",
    "</div>",
    collapse = ""
  )
  date_html <- if (!is.null(display_date))
    paste0("<div class='lgd-sub'>", as.character(display_date), "</div>")
  else ""
  htmltools::HTML(paste0(
    "<div class='lgd-box'>",
    "<div class='lgd-title'>Indice ATMO</div>",
    date_html,
    rows,
    "</div>"
  ))
}
