# ============================================================
# R/map_helpers.R
# Helpers pour les cartes leaflet — carte principale + cartouches DROM
# (à brancher en Étape 1)
# ============================================================

# Centre & zoom France métropolitaine
center_metropole <- list(lng = 2.5, lat = 46.5, zoom = 6)

# Centres approximatifs des DROM (pour les cartouches)
centers_drom <- list(
  guadeloupe = list(lng = -61.55, lat = 16.25, zoom = 8),
  martinique = list(lng = -61.02, lat = 14.65, zoom = 9),
  guyane     = list(lng = -53.20, lat =  4.00, zoom = 6),
  reunion    = list(lng =  55.55, lat = -21.10, zoom = 9),
  mayotte    = list(lng =  45.16, lat = -12.83, zoom = 10)
)

# base_map() — Étape 1
# choropleth_layer() — Étape 2+
# bivariate_layer() — Étape 5
# drom_inset() — Étape 1
