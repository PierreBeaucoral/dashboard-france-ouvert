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

# Helper : attache un handler de clic qui remplit le panneau latéral
# (#commune-panel) à partir d'un lookup JS global `window.communeLookup`
# (construit en amont par dashboard.qmd à partir d'un JSON embed).
.attach_panel_click <- function(map) {
  htmlwidgets::onRender(map,
    "function(el, x) {
       var self = this;
       function attach() {
         self.eachLayer(function(layer) {
           if (layer.options && layer.options.layerId && !layer._cpClickOn) {
             layer._cpClickOn = true;
             layer.on('click', function(e) {
               var code = e.target.options.layerId;
               var data = window.communeLookup ? window.communeLookup[code] : null;
               if (data && window.updateCommunePanel) {
                 window.updateCommunePanel(data);
               }
             });
           }
         });
       }
       setTimeout(attach, 60);
       self.on('layeradd', function() { setTimeout(attach, 0); });
     }")
}

# Helper interne : ajoute un hook JS qui re-trigge map.invalidateSize() à
# chaque changement de taille du conteneur (expansion modale de la card
# Quarto Dashboard, redimensionnement fenêtre, etc.). Sans ce hook, leaflet
# garde la taille initiale et la carte reste minuscule dans la modale.
.add_resize_handler <- function(map) {
  htmlwidgets::onRender(map,
    "function(el, x) {
       var self = this;
       var refresh = function () {
         setTimeout(function () { self.invalidateSize(); }, 80);
       };
       if (typeof ResizeObserver !== 'undefined') {
         new ResizeObserver(refresh).observe(el);
       }
       window.addEventListener('resize', refresh);
     }")
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
      layerId = sf_data$code     # vecteur explicite (cf. choropleth_metropole)
    ) |>
    .add_resize_handler()
}

# ---- Cartouche DROM ----------------------------------------

#' Mini-carte d'un département d'outre-mer (un seul polygone, vue fixe).
#'
#' Utilisée pour les 5 cartouches en pied de page.
#'
#' @param sf_data    `sf` de TOUS les DROM (filtrage interne).
#' @param drom_code  Code INSEE du DROM ("971", "972", "973", "974", "976").
#' @param height     Hauteur CSS.
# ---- Palette politique (familles) --------------------------

# Palette nommée pour les 5 familles + NA.
# Synchronisée avec R/theme.R::palette_pol.
.famille_colors <- c(
  "NFP"    = "#C73659",
  "ENS"    = "#E6B800",
  "RN"     = "#1B3A57",
  "LR"     = "#4A90D9",
  "Divers" = "#8A8A8A"
)

#' Mappeur famille → couleur (factor → color via leaflet::colorFactor).
famille_palette <- leaflet::colorFactor(
  palette  = unname(.famille_colors),
  levels   = names(.famille_colors),
  na.color = "#E8E4DC"
)

# ---- Choroplèthe (commune × famille politique) -------------

#' Choroplèthe principal métropole — communes colorées par famille gagnante,
#' contours départementaux en sur-couche pour la lisibilité.
#'
#' @param com_sf `sf` des communes (métropole), avec colonnes `code`, `nom`,
#'   `famille_vainqueur`, `nuance_vainqueur`, `pct_vainqueur`, `marge_vainqueur`.
#' @param dep_sf `sf` des départements (métropole) — sur-couche fine.
choropleth_metropole <- function(com_sf, dep_sf) {
  # Tooltip HTML par commune
  tooltips <- sprintf(
    "<div style='font-family:Inter,sans-serif;'>
       <strong>%s</strong> <span style='color:#7A7A7A;'>(%s)</span><br>
       <span style='font-family:IBM Plex Mono,monospace;'>%s</span> — %s%%<br>
       <span style='color:#7A7A7A;font-size:0.85em;'>Abstention : %s%%</span>
     </div>",
    htmltools::htmlEscape(com_sf$nom),
    com_sf$code,
    ifelse(is.na(com_sf$nuance_vainqueur), "—", com_sf$nuance_vainqueur),
    ifelse(is.na(com_sf$pct_vainqueur), "—",
           format(round(com_sf$pct_vainqueur, 1), nsmall = 1)),
    ifelse(is.na(com_sf$pct_abstention), "—",
           format(round(com_sf$pct_abstention, 1), nsmall = 1))
  ) |> lapply(htmltools::HTML)

  # Opacité graduée par marge (faible marge = couleur plus pâle)
  opacity <- ifelse(
    is.na(com_sf$marge_vainqueur),
    0.30,
    pmin(0.92, 0.45 + (com_sf$marge_vainqueur / 100) * 1.1)
  )

  leaflet::leaflet(
    com_sf,
    options = leaflet::leafletOptions(
      preferCanvas       = TRUE,   # canvas = perfs avec ~30k polygones
      zoomControl        = TRUE,
      attributionControl = FALSE,
      minZoom            = 4,
      maxZoom            = 12,
      worldCopyJump      = FALSE
    )
  ) |>
    .fit_to_sf(com_sf, padding = c(20, 20)) |>
    # Couche communes (choroplèthe)
    leaflet::addPolygons(
      data         = com_sf,
      fillColor    = ~famille_palette(famille_vainqueur),
      fillOpacity  = opacity,
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
      layerId = com_sf$code      # vecteur explicite (la formule ~code
                                  # ne se résout pas correctement après
                                  # left_join sur un objet sf)
    ) |>
    # Couche départements (sur-couche pour structure visuelle)
    leaflet::addPolygons(
      data         = dep_sf,
      fill         = FALSE,
      color        = "#1A1A1A",
      weight       = 0.6,
      opacity      = 0.55,
      smoothFactor = 0.4,
      options      = leaflet::pathOptions(interactive = FALSE)
    ) |>
    # Légende custom (addControl, mieux stylable que addLegend)
    leaflet::addControl(
      html     = .famille_legend_html(),
      position = "bottomright",
      className = "famille-legend"
    ) |>
    .add_resize_handler()
}

#' Légende HTML des familles politiques (utilisée par addControl).
.famille_legend_html <- function() {
  labels <- c(
    "NFP" = "NFP (UG, FI, SOC)",
    "ENS" = "Ensemble (ENS + HOR)",
    "RN"  = "RN / UXD / EXD",
    "LR"  = "LR / UDI",
    "Divers" = "Divers (DV*, REG, ECO)"
  )
  rows <- paste0(
    "<div class='lgd-row'>",
    "<span class='lgd-swatch' style='background:", .famille_colors, ";'></span>",
    "<span class='lgd-label'>", labels[names(.famille_colors)], "</span>",
    "</div>",
    collapse = ""
  )
  htmltools::HTML(paste0(
    "<div class='lgd-box'>",
    "<div class='lgd-title'>Famille gagnante · T2</div>",
    rows,
    "</div>"
  ))
}

#' Cartouche DROM avec choroplèthe communal.
choropleth_drom_inset <- function(com_drom_sf, drom_code) {
  stopifnot(drom_code %in% names(drom_long_names))
  one <- com_drom_sf[substr(com_drom_sf$code, 1, 3) == drom_code, ]

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
      fillColor    = ~famille_palette(famille_vainqueur),
      fillOpacity  = 0.75,
      color        = "#FFFFFF",
      weight       = 0.3,
      smoothFactor = 0.3
    ) |>
    .add_resize_handler()
}

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
    ) |>
    .add_resize_handler()
}
