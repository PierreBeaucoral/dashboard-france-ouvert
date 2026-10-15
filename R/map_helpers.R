# ============================================================
# R/map_helpers.R
# Cartes : fabrique unique de widgets leaflet "sans polygones communaux".
#
# Architecture (cf. assets/cmap.js) :
#   - R produit un widget leaflet vide + un petit objet `data` :
#       codes[], fill[] (hex ou NA), op[], tip[] (courte chaîne par commune),
#       bounds (emprise DES DONNÉES), état de légende ;
#   - la géométrie (assets/geo/communes.json) est téléchargée une seule fois
#     par le navigateur et partagée par les 12 pages ;
#   - les infobulles sont construites au survol, pas pré-rendues en HTML.
# Résultat : plus de polygones ni d'infobulles dans les pages HTML.
#
# Toutes les couleurs viennent de R/theme.R (donc de styles/tokens.scss).
# ============================================================

# Noms longs (titres de cartes)
drom_long_names <- c(
  "971" = "Guadeloupe", "972" = "Martinique", "973" = "Guyane",
  "974" = "La Réunion", "976" = "Mayotte"
)

# Couleurs de bordure / contour issues des tokens
.CM_BORDER <- list(color = "#FFFFFF", weight = 0.1, opacity = 0.4)
.CM_DEP    <- unname(TOK["ink"])

# onRender générique : délègue à CMap.mount (attend cmap.js si besoin)
.CMAP_JS <- paste0(
  "function(el, x, d) { var m = this; (function go(n) {",
  " if (window.CMap) { CMap.mount(m, el, d); }",
  " else if (n < 120) { setTimeout(function () { go(n + 1); }, 50); }",
  " })(0); }"
)

# ---- Binning : jumeau R de CMap.makeBreaks (assets/cmap.js) ---------------

#' Seuils de classes (6 valeurs) avec garde-fou zéros.
#'
#' Algorithme identique à `CMap.makeBreaks()` : toute modification doit être
#' reportée dans assets/cmap.js (la page vérifie l'égalité des deux en test).
#'
#' @param vals  Vecteur numérique (NA admis).
#' @param scale "quantile" | "log_quantile" | "linear".
#' @return list(breaks = <6 valeurs>, zero = <TRUE si la classe 1 = {0}>).
cmap_breaks <- function(vals, scale = c("quantile", "log_quantile", "linear")) {
  scale <- match.arg(scale)
  v <- vals[is.finite(vals)]
  if (length(v) < 2L) return(list(breaks = c(0, 1, 2, 3, 4, 5), zero = FALSE))
  pos   <- v[v > 0]
  n_zero <- sum(v == 0)
  linear <- seq(min(v), max(v), length.out = 6)
  q <- function(x, p) unname(stats::quantile(x, probs = p, type = 7))
  if (scale == "log_quantile" && length(pos) >= 5L) {
    br <- c(0, expm1(q(log1p(pos), seq(0, 1, by = 0.25)))); zero <- TRUE
  } else if (scale == "linear") {
    br <- linear; zero <- FALSE
  } else if (scale != "log_quantile" && length(pos) >= 5L && n_zero >= length(v) * 0.3) {
    br <- c(0, q(pos, seq(0, 1, by = 0.25))); zero <- TRUE
  } else {
    br <- q(v, seq(0, 1, by = 0.2)); zero <- FALSE
  }
  if (length(unique(br)) < 6L) { br <- linear; zero <- FALSE }
  list(breaks = br, zero = zero)
}

#' Classe 1..5 : 1 + nombre de seuils breaks[2..6] <= v (bornée à 5).
cmap_class <- function(v, breaks) {
  pmin(5L, pmax(1L, findInterval(v, breaks[-1], left.open = FALSE) + 1L))
}

#' Mise en forme française d'un nombre (espace fine insécable, virgule).
cmap_num <- function(x, digits = 0L) {
  out <- formatC(x, format = "f", digits = digits, big.mark = " ", decimal.mark = ",")
  out[!is.finite(x)] <- "—"
  out
}

# ---- Briques communes ------------------------------------------------------

# Emprise des données : c(lat1, lng1, lat2, lng2)
.bounds_of <- function(sf_obj) {
  bb <- sf::st_bbox(sf_obj)
  unname(c(bb["ymin"], bb["xmin"], bb["ymax"], bb["xmax"]))
}

# Rétro-compatibilité (anciens helpers dvf/air/socio non utilisés par les pages)
.fit_to_sf <- function(map, sf_obj, padding = c(10, 10)) {
  b <- .bounds_of(sf_obj)
  leaflet::fitBounds(map, lng1 = b[2], lat1 = b[1], lng2 = b[4], lat2 = b[3],
                     options = list(padding = padding))
}
.add_resize_handler <- function(map) map

#' Widget carte communale (sans polygones).
#'
#' @param com_sf   sf des communes affichées (colonne `code`) : donne l'emprise.
#' @param dep_sf   sf des départements (contours), ou NULL.
#' @param codes,fill,op,tip  vecteurs alignés sur `com_sf$code` (fill NA = non publié).
#' @param legend   list(title, palette, breaks, zero, unit, digits, naLabel,
#'                 sub, note, cats) ou NULL (pas de légende).
#' @param inset    TRUE pour un cartouche DROM (carte figée, sans survol).
#' @param main     TRUE pour la carte principale (source des seuils de recoloration).
#' @param border   list(color, weight, opacity) des contours communaux.
#' @param bc,bw    couleurs / épaisseurs de contour par commune (optionnel).
cmap_widget <- function(com_sf, dep_sf = NULL, codes = com_sf$code,
                        fill, op = 0.9, tip = NULL, legend = NULL,
                        inset = FALSE, main = !inset, border = .CM_BORDER,
                        bc = NULL, bw = NULL,
                        pad = if (inset) c(4, 4) else c(8, 8)) {
  opts <- if (inset) {
    leaflet::leafletOptions(
      preferCanvas = TRUE, zoomControl = FALSE, attributionControl = FALSE,
      dragging = FALSE, scrollWheelZoom = FALSE, doubleClickZoom = FALSE,
      boxZoom = FALSE, touchZoom = FALSE, keyboard = FALSE,
      zoomSnap = 0.1, zoomDelta = 0.5, minZoom = 3, maxZoom = 14)
  } else {
    leaflet::leafletOptions(
      preferCanvas = TRUE, zoomControl = TRUE, attributionControl = FALSE,
      zoomSnap = 0.25, zoomDelta = 0.5, minZoom = 4, maxZoom = 13,
      worldCopyJump = FALSE)
  }
  m <- leaflet::leaflet(options = opts)
  if (!is.null(dep_sf)) {
    m <- leaflet::addPolygons(m, data = dep_sf, fill = FALSE, color = .CM_DEP,
                              weight = 0.6, opacity = 0.5, smoothFactor = 0.6,
                              options = leaflet::pathOptions(interactive = FALSE))
  }
  if (!is.null(legend)) {
    m <- leaflet::addControl(m, html = "<div class='cmap-lgd'></div>",
                             position = "bottomright", className = "cmap-lgd-ctl")
  }
  b <- .bounds_of(com_sf)
  m <- leaflet::fitBounds(m, lng1 = b[2], lat1 = b[1], lng2 = b[4], lat2 = b[3])
  data <- list(
    inset  = inset, main = main, bounds = b, pad = pad,
    layer  = list(codes = I(as.character(codes)), fill = I(fill),
                  op = if (length(op) > 1L) I(round(op, 3)) else op,
                  tip = if (is.null(tip)) NULL else I(tip),
                  bc = if (is.null(bc)) NULL else I(bc),     # contour par commune (option)
                  bw = if (is.null(bw)) NULL else I(bw),
                  border = border),
    legend = legend
  )
  htmlwidgets::onRender(m, .CMAP_JS, data = data)
}

#' Ajoute du JS de page (onRender) exécuté quand CMap est chargé.
#'
#' @param body Corps JS ; variables disponibles : `self` (carte Leaflet), `el`.
cmap_on_render <- function(map, body) {
  htmlwidgets::onRender(map, paste0(
    "function(el, x) { var self = this; (function w(n) {",
    " if (!window.CMap) { if (n < 120) setTimeout(function () { w(n + 1); }, 50); return; }",
    body, " })(0); }"))
}

#' Émet `window.<name>` : lookup code -> ligne, depuis un data.frame sérialisé
#' PAR COLONNES (clés non répétées : 3 à 5 fois plus léger que par lignes).
#'
#' À appeler depuis un chunk `results: asis`. Les colonnes doivent être déjà
#' arrondies ; NA -> null.
#' @param df    data.frame avec une colonne `code`.
#' @param name  nom de la variable globale JS.
emit_table <- function(name, df) {
  stopifnot("code" %in% names(df))
  df <- .plm_expand(df, "code")   # arrondissements PLM sans ligne propre : ligne de la ville
  cols <- c(list(c = df$code), as.list(df[setdiff(names(df), "code")]))
  json <- jsonlite::toJSON(cols, na = "null", digits = NA, auto_unbox = FALSE)
  cat("<script>window.", name, " = (function (t) { var o = {}, ks = Object.keys(t);",
      " for (var i = 0; i < t.c.length; i++) { var r = { code: t.c[i] };",
      " for (var j = 0; j < ks.length; j++) { if (ks[j] !== 'c') r[ks[j]] = t[ks[j]][i]; }",
      " o[t.c[i]] = r; } return o; })(", as.character(json), ");</script>\n", sep = "")
}

# ---- Choroplèthe quantitatif ------------------------------------------------

#' Choroplèthe générique (1 valeur par commune), seuils + légende pilotés par
#' `cmap_breaks`. La page peut ensuite recolorer via CMap.recolor().
#'
#' @param values_df tibble (code_commune, value).
#' @param palette   5 couleurs (cf. PAL$green, PAL$red, ...).
#' @param breaks    NULL (calculés) ou list(breaks, zero) imposés.
#' @param unit,digits unité et décimales (légende et infobulle).
#' @param tip_extra NULL ou fonction(joined) -> vecteur de suffixes d'infobulle.
cmap_choropleth <- function(com_sf, dep_sf, values_df, palette,
                            legend_title, unit = "", digits = 1L,
                            scale = "quantile", breaks = NULL,
                            na_label = "Non publié (secret statistique)",
                            sub = NULL, note = NULL, tip_extra = NULL) {
  joined <- com_sf |>
    dplyr::left_join(.plm_expand(values_df), by = c("code" = "code_commune"))
  vals <- joined$value
  st <- if (is.null(breaks)) cmap_breaks(vals, scale) else breaks
  cls <- cmap_class(vals, st$breaks)
  fill <- ifelse(is.finite(vals), palette[cls], NA_character_)
  tip  <- ifelse(is.finite(vals),
                 paste0(cmap_num(vals, digits), if (nzchar(unit)) paste0(" ", unit) else ""),
                 "Non publié")
  if (!is.null(tip_extra)) tip <- paste0(tip, tip_extra(joined))
  cmap_widget(joined, dep_sf, fill = fill, op = 0.9, tip = tip,
              legend = list(title = legend_title, palette = palette,
                            breaks = st$breaks, zero = st$zero, unit = unit,
                            digits = digits, naLabel = na_label, sub = sub, note = note))
}

#' Choroplèthe catégoriel (typologie de densité, etc.).
cmap_categorical <- function(com_sf, dep_sf, values_df, levels, palette,
                             legend_title, na_label = "Non classée") {
  joined <- com_sf |>
    dplyr::left_join(.plm_expand(values_df), by = c("code" = "code_commune"))
  ix <- match(as.character(joined$value), levels)
  fill <- ifelse(is.na(ix), NA_character_, palette[ix])
  tip  <- ifelse(is.na(ix), "Non classée", as.character(joined$value))
  cmap_widget(joined, dep_sf, fill = fill, op = 0.9, tip = tip,
              legend = list(title = legend_title, palette = palette, cats = I(levels),
                            naLabel = na_label))
}

#' Cartouche DROM : mêmes seuils, même palette que la carte métropole.
#'
#' @param st  list(breaks, zero) de la carte principale (voir cmap_choropleth).
cmap_inset <- function(com_drom_sf, drom_code, values_df, palette, st) {
  one <- com_drom_sf[substr(com_drom_sf$code, 1, 3) == drom_code, ]
  one <- dplyr::left_join(one, values_df, by = c("code" = "code_commune"))
  vals <- one$value
  fill <- ifelse(is.finite(vals), palette[cmap_class(vals, st$breaks)], NA_character_)
  cmap_widget(one, NULL, fill = fill, op = 0.9, tip = NULL, inset = TRUE)
}

#' Cartouche DROM catégoriel.
cmap_inset_cat <- function(com_drom_sf, drom_code, values_df, levels, palette) {
  one <- com_drom_sf[substr(com_drom_sf$code, 1, 3) == drom_code, ]
  one <- dplyr::left_join(one, values_df, by = c("code" = "code_commune"))
  ix <- match(as.character(one$value), levels)
  cmap_widget(one, NULL, fill = ifelse(is.na(ix), NA_character_, palette[ix]),
              op = 0.9, inset = TRUE)
}

#' Cartouche DROM avec couleurs déjà calculées (politique, ATMO...).
cmap_inset_fill <- function(com_drom_sf, drom_code, fill, op = 0.9) {
  stopifnot(length(fill) == nrow(com_drom_sf))
  keep <- substr(com_drom_sf$code, 1, 3) == drom_code
  one  <- com_drom_sf[keep, ]
  cmap_widget(one, NULL, fill = fill[keep],
              op = if (length(op) > 1L) op[keep] else op, inset = TRUE)
}

#' Emplacement Mayotte (ou autre territoire) sans donnée : carte vide + mention.
cmap_inset_empty <- function(label = "Non couvert") {
  htmltools::div(class = "inset-empty", label)
}

# ---- Élections (famille gagnante) -------------------------------------------

.famille_colors <- palette_pol
.famille_labels <- c(
  NFP = "NFP (UG, FI, SOC)", ENS = "Ensemble (ENS + HOR)", RN = "RN / UXD / EXD",
  LR = "LR / UDI", Divers = "Divers (DV*, REG, ECO)"
)

#' Opacité = marge de victoire (faible marge -> couleur plus pâle).
.margin_opacity <- function(marge) {
  ifelse(is.na(marge), 0.3, pmin(0.92, 0.45 + (marge / 100) * 1.1))
}

cmap_elections <- function(com_sf, dep_sf) {
  fam  <- as.character(com_sf$famille_vainqueur)
  fill <- unname(.famille_colors[fam])
  tip  <- ifelse(is.na(fam), "Pas de résultat publié",
                 paste0(ifelse(is.na(com_sf$nuance_vainqueur), fam, com_sf$nuance_vainqueur),
                        " · ", cmap_num(com_sf$pct_vainqueur, 1), " %",
                        " · abstention ", cmap_num(com_sf$pct_abstention, 1), " %"))
  cmap_widget(com_sf, dep_sf, fill = fill, op = .margin_opacity(com_sf$marge_vainqueur),
              tip = tip,
              legend = list(title = "Famille gagnante · 2nd tour",
                            palette = unname(.famille_colors),
                            cats = I(unname(.famille_labels[names(.famille_colors)])),
                            naLabel = "Pas de résultat publié",
                            note = "Teinte plus pâle = marge de victoire plus faible."))
}

cmap_elections_inset <- function(com_drom_sf, drom_code) {
  fam <- as.character(com_drom_sf$famille_vainqueur)
  cmap_inset_fill(com_drom_sf, drom_code, unname(.famille_colors[fam]),
                  .margin_opacity(com_drom_sf$marge_vainqueur))
}

# Municipales : "Divers" dominant (~80 %) -> teinte dédiée, contour T1/T2.
.mun_palette <- c(palette_pol[c("NFP", "ENS", "RN", "LR")],
                  Divers = unname(TOK["pol-divers-mun"]))
.MUN_T1_LINE <- unname(TOK["ink"])

cmap_municipales <- function(com_sf, dep_sf) {
  fam  <- as.character(com_sf$famille_vainqueur)
  fill <- unname(.mun_palette[fam])
  # Contour : communes décidées au T1 en trait foncé (un seul trait : la
  # couleur ne code pas le tour, l'épaisseur oui, cf. légende).
  tip <- ifelse(is.na(fam), "< 1 000 hab. : scrutin individuel",
                paste0(fam, " · ", ifelse(is.na(com_sf$nuance_vainqueur), "—", com_sf$nuance_vainqueur),
                       " · ", cmap_num(com_sf$pct_vainqueur, 1), " % au ",
                       ifelse(is.na(com_sf$tour), "—", com_sf$tour)))
  cmap_widget(com_sf, dep_sf, fill = fill, op = 0.9, tip = tip,
              legend = list(title = "Famille gagnante",
                            palette = unname(.mun_palette), cats = I(names(.mun_palette)),
                            naLabel = "< 1 000 hab. (scrutin individuel)"))
}

# ---- Qualité de l'air (indice ATMO officiel, exception réglementaire) --------

.ATMO_LABELS <- c("1 \u00b7 Bon", "2 \u00b7 Moyen", "3 \u00b7 Dégradé",
                  "4 \u00b7 Mauvais", "5 \u00b7 Très mauvais", "6 \u00b7 Extrêmement mauvais")

cmap_atmo <- function(com_sf, dep_sf, atmo_df, display_date = NULL) {
  joined <- dplyr::left_join(com_sf, .plm_expand(atmo_df), by = c("code" = "code_commune"))
  v <- joined$qual_indice
  v[!(v %in% 1:6)] <- NA
  fill <- ifelse(is.na(v), NA_character_, unname(ATMO_COLORS[as.character(v)]))
  tip  <- ifelse(is.na(v), "Pas d'indice publié",
                 paste0("ATMO ", .ATMO_LABELS[pmax(1L, v)],
                        ifelse(is.na(joined$source_aasqa), "", paste0(" \u00b7 ", joined$source_aasqa))))
  cmap_widget(joined, dep_sf, fill = fill, op = 0.9, tip = tip,
              legend = list(title = "Indice ATMO, moyenne sur 30 jours",
                            sub = if (is.null(display_date)) NULL else as.character(display_date),
                            palette = unname(ATMO_COLORS[as.character(1:6)]),
                            cats = I(.ATMO_LABELS), naLabel = "Pas d'indice publié"))
}

cmap_atmo_inset <- function(com_drom_sf, drom_code, atmo_df) {
  v <- atmo_df$qual_indice[match(com_drom_sf$code, atmo_df$code_commune)]
  v[!(v %in% 1:6)] <- NA
  cmap_inset_fill(com_drom_sf, drom_code,
                  ifelse(is.na(v), NA_character_, unname(ATMO_COLORS[as.character(v)])))
}

# ---- Cartes de repli (données absentes) --------------------------------------

#' Départements seuls : carte de repli si les parquets sont absents.
base_map_metropole <- function(sf_data, fill = unname(TOK["na-fill"]),
                               border = unname(TOK["ink-2"])) {
  b <- .bounds_of(sf_data)
  leaflet::leaflet(sf_data, options = leaflet::leafletOptions(
    zoomControl = TRUE, attributionControl = FALSE, zoomSnap = 0.25, minZoom = 4, maxZoom = 11)) |>
    leaflet::fitBounds(lng1 = b[2], lat1 = b[1], lng2 = b[4], lat2 = b[3]) |>
    leaflet::addPolygons(fillColor = fill, fillOpacity = 1, color = border, weight = 0.6,
                         opacity = 0.9, smoothFactor = 0.4,
                         label = ~paste0(nom, " (", code, ")"))
}

#' Cartouche DROM de repli (département seul).
drom_inset <- function(sf_data, drom_code) {
  stopifnot(drom_code %in% names(drom_long_names))
  one <- sf_data[sf_data$code == drom_code, ]
  b <- .bounds_of(one)
  leaflet::leaflet(one, options = leaflet::leafletOptions(
    zoomControl = FALSE, attributionControl = FALSE, dragging = FALSE,
    scrollWheelZoom = FALSE, doubleClickZoom = FALSE, boxZoom = FALSE,
    touchZoom = FALSE, keyboard = FALSE)) |>
    leaflet::fitBounds(lng1 = b[2], lat1 = b[1], lng2 = b[4], lat2 = b[3]) |>
    leaflet::addPolygons(fillColor = unname(TOK["na-fill"]), fillOpacity = 1,
                         color = unname(TOK["ink-2"]), weight = 0.8, smoothFactor = 0.3)
}

# ---- Branchement standard d'une page cartographique ---------------------------

#' Spécification d'une ligne « repères » (rang centile) du panneau commune.
#' @param f      colonne de commune_merged (assets/data/col/<f>.json).
#' @param log10  TRUE si la colonne est en log10 (affichage en 10^x).
ctx_spec <- function(f, label, unit = "", d = 0L, log10 = FALSE) {
  list(f = f, label = label, unit = unit, d = d, log10 = log10)
}

#' Corps JS standard d'une page à radios : mémorise la carte, branche le clic
#' (panneau de la page + repères centiles), les radios, et recolore au premier
#' affichage (légende avec parts communes/population, gradient urbain-rural).
#'
#' @param prefix   préfixe des globales JS (ex. "fin" -> finMap).
#' @param panel_fn nom de la fonction JS qui remplit le panneau (ou NULL).
#' @param content  id du conteneur du panneau (ex. "fin-content").
#' @param radios   attribut name des radios (ex. "fin-ind").
#' @param recolor  nom de la fonction JS de recoloration (reçoit la valeur du radio).
#' @param default  valeur initiale du radio.
#' @param ctx      liste de ctx_spec() ou NULL.
#' @param extra_pick JS ajouté dans le callback de clic (variable `code`).
cmap_page_hooks <- function(prefix, panel_fn, content, radios, recolor, default, ctx = NULL,
                            ctx_title = "Repères : rang centile", extra_pick = "") {
  specs <- if (is.null(ctx)) "null" else as.character(jsonlite::toJSON(ctx, auto_unbox = TRUE))
  paste0(
    "window.", prefix, "Map = self;\n",
    "CMap.pick(self, function (code) {\n",
    if (!is.null(panel_fn)) paste0("  if (window.", panel_fn, ") window.", panel_fn, "(code);\n") else "",
    extra_pick,
    "  CMap.ctx.append('", content, "', code, ", specs, ", ", jsonlite::toJSON(ctx_title, auto_unbox = TRUE), ");\n});\n",
    "CMap.onRadios('", radios, "', function (v) { window.", recolor, "(v); });\n",
    "self.on('cmap:ready', function () { window.", recolor, "('", default, "'); });\n"
  )
}
