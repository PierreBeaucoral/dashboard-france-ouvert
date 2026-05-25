# ============================================================
# R/map_helpers.R
# Helpers pour les cartes leaflet — carte principale + cartouches DROM
# ============================================================

# Centre & zoom France métropolitaine
center_metropole <- list(lng = 2.5, lat = 46.5, zoom = 6)

# Centres / zoom des cartouches DROM (clé = code département INSEE)
centers_drom <- list(
  "971" = list(lng = -61.55, lat =  16.25, zoom = 8),   # Guadeloupe
  "972" = list(lng = -61.02, lat =  14.65, zoom = 9),   # Martinique
  "973" = list(lng = -53.20, lat =   4.00, zoom = 6),   # Guyane
  "974" = list(lng =  55.55, lat = -21.10, zoom = 9),   # La Réunion
  "976" = list(lng =  45.16, lat = -12.83, zoom = 10)   # Mayotte
)

# Noms longs (utilisés dans les titres de cartes / tooltips)
drom_long_names <- c(
  "971" = "Guadeloupe",
  "972" = "Martinique",
  "973" = "Guyane",
  "974" = "La Réunion",
  "976" = "Mayotte"
)

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
                               height = "100%",
                               fill   = "#E8E4DC",
                               border = "#4A4A4A") {
  leaflet::leaflet(
    sf_data,
    width  = "100%",
    height = height,
    options = leaflet::leafletOptions(
      zoomControl        = TRUE,
      attributionControl = FALSE,
      minZoom            = 5,
      maxZoom            = 11,
      worldCopyJump      = FALSE
    )
  ) |>
    leaflet::setView(
      lng  = center_metropole$lng,
      lat  = center_metropole$lat,
      zoom = center_metropole$zoom
    ) |>
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
drom_inset <- function(sf_data, drom_code, height = "100%") {
  stopifnot(drom_code %in% names(centers_drom))

  one_drom <- sf_data[sf_data$code == drom_code, ]
  center   <- centers_drom[[drom_code]]

  leaflet::leaflet(
    one_drom,
    width  = "100%",
    height = height,
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
    leaflet::setView(
      lng  = center$lng,
      lat  = center$lat,
      zoom = center$zoom
    ) |>
    leaflet::addPolygons(
      fillColor    = "#E8E4DC",
      fillOpacity  = 0.9,
      color        = "#4A4A4A",
      weight       = 0.8,
      smoothFactor = 0.3
    )
}
