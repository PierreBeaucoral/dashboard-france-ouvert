# ============================================================
# R/page_helpers.R
# Helpers des pages thématiques pilotées par configuration
# (logement, risques, services ; énergie et revenus pour leurs couches ajoutées).
# Le JS correspondant est assets/thematic.js. Sourcé APRÈS setup_dashboard.R.
#
# Un « champ » est une liste :
#   col     colonne du data.frame source        label  texte du bouton radio
#   key     clé JS courte (colonne émise)       title  titre de légende
#   unit, digits, pal ("seq"|"div"|...), scale ("quantile"|"log_quantile"),
#   breaks (seuils fixes, 6 valeurs) + cats (étiquettes de classes), sub, note,
#   na (libellé « non publié »), badge (bandeau permanent), levels (binaire)
# ============================================================

#' Duplique les lignes des villes PLM (Paris, Lyon, Marseille) sur leurs
#' arrondissements, pour les sources publiées au seul code ville : les cartes
#' sont dessinées à l'arrondissement (cf. plm_map dans setup_dashboard.R).
th_plm <- function(df, code = "code_commune") {
  stopifnot(exists("plm_map"))
  add <- plm_map[!plm_map$arrond %in% df[[code]] & plm_map$city %in% df[[code]], ]
  if (nrow(add) == 0L) return(df)
  ex <- df[match(add$city, df[[code]]), , drop = FALSE]
  ex[[code]] <- add$arrond
  dplyr::bind_rows(df, ex)
}

#' Table JS (`window.<name>`) + configuration Thematic.setup, depuis un tibble
#' dont les colonnes sont déjà les `col` des champs (+ colonnes de panneau).
#' @param keys nommé : clé JS = colonne du tibble `df` (arrondie ici selon `dec`)
#' @param dec  décimales d'arrondi par clé (défaut 2)
th_emit <- function(prefix, panel_fn, df, keys, dec = list(), fields, panel, pals, first) {
  out <- tibble::tibble(code = df$code_commune)
  for (k in names(keys)) {
    v <- df[[keys[[k]]]]
    out[[k]] <- if (is.character(v)) v else round(v, dec[[k]] %||% 2L)
  }
  has <- rowSums(!is.na(out[setdiff(names(out), "code")])) > 0
  has <- has & !duplicated(out$code)
  emit_table(paste0(prefix, "Data"), out[has, ])
  fields <- lapply(fields, function(f) { if (!is.null(f$levels)) f$offset <- 1L; f })
  fj <- lapply(fields, function(f) f[intersect(c("key", "title", "unit", "digits", "pal", "scale",
    "breaks", "cats", "sub", "note", "na", "badge", "levels", "offset"), names(f))])
  cfg <- list(prefix = prefix, panelFn = panel_fn, data = paste0(prefix, "Data"),
              pals = pals, fields = fj, first = first, panel = panel)
  cat("<script>Thematic.setup(", as.character(jsonlite::toJSON(cfg, auto_unbox = TRUE, null = "null")),
      ");</script>\n", sep = "")
}

#' Boutons radio (un par champ) + conteneur du bandeau permanent.
th_radios <- function(prefix, fields, label = "Indicateur") {
  ids <- names(fields)
  lab <- vapply(seq_along(ids), function(i) sprintf(
    '<label><input type="radio" name="%s-ind" value="%s"%s> %s</label>',
    prefix, ids[i], if (i == 1L) " checked" else "", fields[[i]]$label), character(1))
  htmltools::HTML(sprintf(paste0(
    '<div class="dvf-controls"><div class="dvf-ctrl-group"><span class="dvf-ctrl-label">%s</span>',
    '<div class="dvf-ctrl-radios" id="%s-radios">%s</div></div>',
    '<div id="%s-badge" class="th-badge"></div></div>'),
    label, prefix, paste(lab, collapse = "\n"), prefix))
}

#' Valeurs (code_commune, value) d'un champ, étendues aux arrondissements PLM.
th_values <- function(df, f) th_plm(dplyr::transmute(df, code_commune, value = .data[[f$col]]))

#' Seuils d'un champ (fixes si `breaks`, sinon calculés sur la métropole).
th_breaks <- function(vals, f) {
  if (!is.null(f$levels)) return(list(breaks = c(0, 1, 2, 3, 4, 5), zero = FALSE))
  if (!is.null(f$breaks)) return(list(breaks = f$breaks, zero = FALSE))
  cmap_breaks(vals$value[vals$code_commune %in% com_metro_only$code], f$scale %||% "quantile")
}

#' Carte principale métropole pour le champ `f` ; le JS prend ensuite le relais.
th_map <- function(prefix, f, vals, st, pals) {
  pal <- pals[[f$pal]]
  if (!is.null(f$levels)) {                       # binaire : 2 classes
    m <- cmap_categorical(com_metro_only, dep_metro,
                          dplyr::mutate(vals, value = f$levels[value + 1L]), f$levels, pal,
                          f$title, na_label = f$na %||% "Non publié")
  } else {
    m <- cmap_choropleth(com_metro_only, dep_metro, vals, palette = pal,
                         legend_title = f$title, unit = f$unit %||% "", digits = f$digits %||% 0L,
                         breaks = st, na_label = f$na %||% "Non publié", sub = f$sub, note = f$note)
  }
  cmap_on_render(m, sprintf("Thematic.attach(self, '%s');", prefix))
}

#' Cartouche DROM cohérent avec la carte principale.
th_inset <- function(code, f, vals, st, pals) {
  pal <- pals[[f$pal]]
  if (!is.null(f$levels)) {
    return(cmap_inset_cat(com_drom_only, code, dplyr::mutate(vals, value = f$levels[value + 1L]), f$levels, pal))
  }
  cmap_inset(com_drom_only, code, vals, pal, st)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
