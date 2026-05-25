# ============================================================
# R/map_helpers.R
# Helpers pour les cartes leaflet — carte principale + cartouches DROM
# ============================================================

# Noms longs (utilisés dans les titres de cartes / tooltips)
drom_long_names <- c(
  "971" = "Guadeloupe",
  "972" = "Martinique",
  "973" = "Guyane",
  "974" = "La Réunion",
  "976" = "Mayotte"
)

# Helper interne : fit bounds depuis un objet sf, avec padding en pixels
.fit_to_sf <- function(map, sf_obj, padding = c(10, 10)) {
  bb <- sf::st_bbox(sf_obj)
  leaflet::fitBounds(
    map,
    lng1 = unname(bb["xmin"]), lat1 = unname(bb["ymin"]),
    lng2 = unname(bb["xmax"]), lat2 = unname(bb["ymax"]),
    options = list(padding = padding)
  )
}

# ---- Carte principale (métropole) ---------------------------

#' Carte de France métropolitaine — couche départements sans données métier.
#'
#' Sera complétée en Étape 2 par un overlay choroplèthe par commune
#' (résultats Législatives 2024).
#'
#' @param sf_data Objet `sf` des départements métropolitains.
#' @param height  Hauteur CSS de la carte.
#' @param style   Liste optionnelle de paramètres de style.
base_map_metropole <- function(sf_data,
                               fill   = "#E8E4DC",
                               border = "#4A4A4A") {
  # Pas de width/height : on laisse htmlwidgets + Quarto Dashboard
  # remplir la card automatiquement.
  leaflet::leaflet(
    sf_data,
    options = leaflet::leafletOptions(
      zoomControl        = TRUE,
      attributionControl = FALSE,
      minZoom            = 4,
      maxZoom            = 11,
      worldCopyJump      = FALSE
    )
  ) |>
    .fit_to_sf(sf_data, padding = c(20, 20)) |>
    leaflet::addPolygons(
      fillColor    = fill,
      fillOpacity  = 0.9,
      color        = border,
      weight       = 0.6,
      opacity      = 0.9,
      smoothFactor = 0.4,
      highlightOptions = leaflet::highlightOptions(
        weight       = 2,
        color        = "#2A6F97",
        fillColor    = "#2A6F97",
        fillOpacity  = 0.15,
        bringToFront = TRUE
      ),
      label = ~paste0(nom, " (", code, ")"),
      labelOptions = leaflet::labelOptions(
        style = list(
          "font-family"   = "Inter, sans-serif",
          "font-size"     = "0.85rem",
          "font-weight"   = "500",
          "padding"       = "4px 8px",
          "color"         = "#1A1A1A",
          "background"    = "#FFFFFF",
          "border"        = "1px solid #DCD8CF",
          "box-shadow"    = "0 2px 6px rgba(0,0,0,0.06)"
        ),
        direction = "auto"
      ),
      layerId = ~code
    )
}

# ---- Cartouche DROM ----------------------------------------

#' Mini-carte d'un département d'outre-mer (un seul polygone, vue fixe).
#'
#' Utilisée pour les 5 cartouches en pied de page.
#'
#' @param sf_data    `sf` de TOUS les DROM (filtrage interne).
#' @param drom_code  Code INSEE du DROM ("971", "972", "973", "974", "976").
#' @param height     Hauteur CSS.
drom_inset <- function(sf_data, drom_code) {
  stopifnot(drom_code %in% names(drom_long_names))

  one_drom <- sf_data[sf_data$code == drom_code, ]

  leaflet::leaflet(
    one_drom,
    options = leaflet::leafletOptions(
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
    .fit_to_sf(one_drom, padding = c(8, 8)) |>
    leaflet::addPolygons(
      fillColor    = "#E8E4DC",
      fillOpacity  = 0.9,
      color        = "#4A4A4A",
      weight       = 0.8,
      smoothFactor = 0.3
    )
}
