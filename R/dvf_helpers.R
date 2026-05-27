# ============================================================
# R/dvf_helpers.R
# Helpers pour la couche DVF (prix m² médian).
# ============================================================

# Palette séquentielle cream → vert sombre, 5 classes
# (alignée avec l'identité visuelle du dashboard).
.dvf_palette <- c("#EFE9D9", "#C9D0AC", "#94B091", "#5E876E", "#2F5145")
.dvf_na_color <- "#E8E4DC"  # estimation insuffisante / commune absente

# Bornes par quintile, calculées séparément pour App et Maison
# (les ordres de grandeur diffèrent fortement).
compute_dvf_breaks <- function(dvf_df) {
  filtered <- dvf_df |>
    dplyr::filter(!estimation_insuffisante,
                  !is.na(prix_m2_median),
                  is.finite(prix_m2_median))
  list(
    Appartement = unname(quantile(
      filtered$prix_m2_median[filtered$type_local == "Appartement"],
      probs = seq(0, 1, by = 0.2), na.rm = TRUE)),
    Maison = unname(quantile(
      filtered$prix_m2_median[filtered$type_local == "Maison"],
      probs = seq(0, 1, by = 0.2), na.rm = TRUE))
  )
}

#' Construit le choroplèthe DVF pour un (annee, type_local) donné.
#'
#' La carte est rendue avec le type/année par défaut ; les contrôles
#' JS (radios) dans le dashboard ré-appellent setStyle() sur chaque polygone
#' selon `window.dvfData[code][type][annee]`.
#'
#' @param com_sf      `sf` communes métropole avec colonnes `code`, `nom`.
#' @param dep_sf      `sf` départements pour sur-couche.
#' @param dvf_default tibble (code_commune, prix_m2_median) du défaut affiché.
#' @param breaks      vecteur de 6 valeurs (5 bins) pour la couleur.
choropleth_dvf <- function(com_sf, dep_sf, dvf_default, breaks) {

  # Color picker par bin
  pick_color <- function(prix) {
    ifelse(
      is.na(prix), .dvf_na_color,
      .dvf_palette[pmin(
        5L,
        pmax(1L, findInterval(prix, breaks[-1], left.open = FALSE) + 1L)
      )]
    )
  }

  # Préparer la donnée de départ
  joined <- com_sf |>
    dplyr::left_join(dvf_default, by = c("code" = "code_commune"))

  fillcols <- pick_color(joined$prix_m2_median)
  fillop   <- ifelse(is.na(joined$prix_m2_median), 0.30, 0.85)

  # Tooltip
  tooltips <- sprintf(
    "<div style='font-family:Inter,sans-serif;'>
       <strong>%s</strong> <span style='color:#7A7A7A;'>(%s)</span><br>
       <span style='font-family:IBM Plex Mono,monospace;'>%s €/m²</span>
       <span style='color:#7A7A7A;font-size:0.85em;'> · %s ventes</span>
     </div>",
    htmltools::htmlEscape(joined$nom),
    joined$code,
    ifelse(is.na(joined$prix_m2_median), "—",
           format(round(joined$prix_m2_median), big.mark = " ")),
    ifelse(is.na(joined$n_transactions), "0",
           format(joined$n_transactions, big.mark = " "))
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
      layerId = joined$code      # vecteur explicite (cf. choropleth_metropole)
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
      html      = .dvf_legend_html(breaks),
      position  = "bottomright",
      className = "dvf-legend"
    ) |>
    .add_resize_handler()
}

#' Légende DVF (5 bins séquentiels).
.dvf_legend_html <- function(breaks) {
  # breaks: 6 valeurs → 5 intervalles
  labels <- vapply(seq_len(5), function(i) {
    sprintf("%s – %s €/m²",
            format(round(breaks[i]),     big.mark = " "),
            format(round(breaks[i + 1]), big.mark = " "))
  }, character(1))
  rows <- paste0(
    "<div class='lgd-row'>",
    "<span class='lgd-swatch' style='background:", .dvf_palette, ";'></span>",
    "<span class='lgd-label'>", labels, "</span>",
    "<span class='lgd-hex'>", toupper(.dvf_palette), "</span>",
    "</div>",
    collapse = ""
  )
  htmltools::HTML(paste0(
    "<details class='lgd-box' open>",
    "<summary class='lgd-title'>Prix m² médian · quintiles</summary>",
    rows,
    "<div class='lgd-row' style='margin-top:6px;border-top:1px dashed #DCD8CF;padding-top:6px;'>",
    "<span class='lgd-swatch' style='background:", .dvf_na_color, ";'></span>",
    "<span class='lgd-label' style='color:#7A7A7A;'>N &lt; 5 ou non publié</span>",
    "<span class='lgd-hex'>", toupper(.dvf_na_color), "</span>",
    "</div>",
    "</details>"
  ))
}
