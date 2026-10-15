# ============================================================
# R/build_web_assets.R
# Pré-rendu Quarto (cf. _quarto.yml `project: pre-render`).
#
# Produit des fichiers statiques MIS EN CACHE par le navigateur, chargés
# par assets/cmap.js (cartes) et par les pages d'analyse, au lieu d'être
# recopiés dans chacune des 12 pages HTML :
#
#   assets/geo/communes.json          géométrie des ~35 900 communes, quantifiée
#                                     (1e-4 deg ~ 10 m) et codée en deltas
#   assets/data/communes.json         index des communes (code, nom, dept, strate)
#   assets/data/col/<champ>.json      une colonne numérique de commune_merged
#                                     (alignée sur l'index ci-dessus)
#
# Format géométrie : {q, c[], n[], g[]}; g[i] = polygones de la commune i,
# chaque polygone = liste d'anneaux, chaque anneau = entiers à plat
# [x0, y0, dx1, dy1, ...] (x = longitude * q, y = latitude * q), anneau
# non refermé. Décodage : assets/cmap.js::decodeGeo().
#
# Déterministe, sans graine, chemins relatifs à la racine du projet.
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(jsonlite)
  library(arrow)
})

Q <- 1e4  # facteur de quantification (~10 m en latitude)

geo_src <- file.path("data", "processed", "geo", "communes.geojson")
geo_out <- file.path("assets", "geo", "communes.json")
dir.create(dirname(geo_out), recursive = TRUE, showWarnings = FALSE)

#' Encode un anneau (matrice n x 2 lon/lat) en entiers à plat codés en deltas.
encode_ring <- function(m) {
  n <- nrow(m)
  if (n > 1L && all(m[1L, ] == m[n, ])) m <- m[-n, , drop = FALSE]  # retire le point de fermeture
  x <- round(m[, 1L] * Q)
  y <- round(m[, 2L] * Q)
  out <- integer(2L * length(x))
  out[c(TRUE, FALSE)] <- as.integer(c(x[1L], diff(x)))
  out[c(FALSE, TRUE)] <- as.integer(c(y[1L], diff(y)))
  out
}

build_geometry <- function() {
  if (file.exists(geo_out) &&
      file.mtime(geo_out) > file.mtime(geo_src) &&
      file.mtime(geo_out) > file.mtime("R/build_web_assets.R")) {
    message("[assets] géométrie à jour : ", geo_out)
    return(invisible())
  }
  x <- sf::read_sf(geo_src, quiet = TRUE)
  g <- sf::st_geometry(x)
  enc <- lapply(g, function(mp) {                       # mp : MULTIPOLYGON
    lapply(unclass(mp), function(poly) lapply(poly, encode_ring))
  })
  obj <- list(q = Q, c = x$code, n = x$nom, g = unname(enc))
  writeLines(jsonlite::toJSON(obj, auto_unbox = TRUE, digits = NA),
             geo_out, useBytes = TRUE)
  message("[assets] ", geo_out, " : ", format(file.size(geo_out), big.mark = " "), " o")
}

#' Libellés INSEE des classes de densité, ordonnés par densite_rang (1..6).
dens_labels <- function() {
  g <- arrow::read_parquet(file.path("data", "processed", "insee_densite", "grille_densite.parquet"),
                           col_select = c("densite_insee", "densite_rang"))
  g <- unique(g[!is.na(g$densite_rang), ])
  stopifnot(nrow(g) == 6L, identical(sort(g$densite_rang), 1:6))
  as.list(g$densite_insee[order(g$densite_rang)])
}

build_columns <- function() {
  mp <- file.path("data", "processed", "explorer", "commune_merged.parquet")
  if (!file.exists(mp)) {
    message("[assets] commune_merged.parquet absent : colonnes non générées")
    return(invisible())
  }
  m <- arrow::read_parquet(mp)
  m <- m[!is.na(m$code_commune), ]
  dir.create(file.path("assets", "data", "col"), recursive = TRUE, showWarnings = FALSE)

  # Index : code, nom, département (liste dédupliquée + indice), strate (idem)
  depts   <- unique(m[, c("code_dept", "libelle_dept")])
  depts   <- depts[!is.na(depts$code_dept), ]
  depts   <- depts[order(depts$code_dept), ]
  strates <- sort(unique(as.character(m$strate_taille)))
  idx <- list(
    c  = m$code_commune,
    n  = ifelse(is.na(m$nom_commune), m$code_commune, m$nom_commune),
    d  = match(m$code_dept, depts$code_dept) - 1L,
    s  = match(as.character(m$strate_taille), strates) - 1L,
    depts = lapply(seq_len(nrow(depts)), function(i)
      list(code = depts$code_dept[i], nom = depts$libelle_dept[i])),
    strates = as.list(strates),
    # Libellés des 6 classes de densité INSEE par rang (source unique : scripts/10_ ->
    # grille_densite.parquet), lus par assets/cmap.js (CMap.densLabels).
    dl = dens_labels()
  )
  writeLines(jsonlite::toJSON(idx, auto_unbox = TRUE, na = "null"),
             file.path("assets", "data", "communes.json"), useBytes = TRUE)

  # Colonnes numériques : un fichier par champ, valeurs arrondies à 4 chiffres
  # significatifs utiles (3 décimales max), NA -> null.
  num <- names(m)[vapply(m, is.numeric, logical(1))]
  for (f in num) {
    v <- m[[f]]
    v[!is.finite(v)] <- NA_real_
    writeLines(jsonlite::toJSON(round(v, 3), na = "null", digits = NA),
               file.path("assets", "data", "col", paste0(f, ".json")), useBytes = TRUE)
  }
  message("[assets] ", length(num), " colonnes + index : ", nrow(m), " communes")
}

#' Flux domicile-travail (top 5 entrants / sortants par commune) : fichier
#' chargé à la demande par pages/mobilite.qmd au premier clic (283 000 lignes,
#' jadis recopiées dans le HTML : ~40 Mo).
build_flows <- function() {
  fp <- file.path("data", "processed", "mobilite", "flows_per_commune.parquet")
  if (!file.exists(fp)) return(invisible())
  f <- arrow::read_parquet(fp)
  obj <- list(c = f$code_focal, d = as.integer(f$direction == "out"),
              k = f$code_counter, f = as.integer(f$flux), r = as.integer(f$rang))
  dir.create(file.path("assets", "data"), recursive = TRUE, showWarnings = FALSE)
  writeLines(jsonlite::toJSON(obj, auto_unbox = FALSE, na = "null"),
             file.path("assets", "data", "mobilite_flows.json"), useBytes = TRUE)
  message("[assets] mobilite_flows.json : ", nrow(f), " lignes")
}

#' Disposition « constellation » (pages/constellation.qmd) : une position par commune
#' dans deux dispositions partageant le même cadre (unités abstraites, x de 0 à 1000).
#'   constellation.json      positions géographiques (Web Mercator, la projection des
#'                           cartes), encarts outre-mer, positions des départements
#'   constellation-dor.json  disposition « population » (cercles de Dorling), chargée
#'                           seulement quand l'utilisateur bascule sur cette vue
#' Alignées sur l'ordre de assets/data/communes.json. Convention outre-mer : cinq encarts
#' égaux sous la métropole, chacun à SA propre échelle ; en vue population, les cercles
#' gardent la MÊME échelle partout (surface ∝ habitants). Voir docs/decisions.qmd.
build_constellation <- function() {
  mp  <- file.path("data", "processed", "explorer", "commune_merged.parquet")
  o1  <- file.path("assets", "data", "constellation.json")
  o2  <- file.path("assets", "data", "constellation-dor.json")
  deps <- c(geo_src, mp, "R/build_web_assets.R", "R/dorling.R")
  if (!file.exists(mp)) return(invisible())
  if (all(file.exists(c(o1, o2))) && all(file.mtime(o1) > file.mtime(deps)) && all(file.mtime(o2) > file.mtime(deps))) {
    message("[assets] constellation à jour"); return(invisible())
  }
  source("R/dorling.R")
  m <- arrow::read_parquet(mp, col_select = c("code_commune", "code_dept", "libelle_dept", "ctx_pop_latest"))
  m <- m[!is.na(m$code_commune), ]
  g <- sf::read_sf(geo_src, quiet = TRUE)
  pts <- sf::st_coordinates(sf::st_point_on_surface(sf::st_transform(sf::st_geometry(g), 3857)))
  pos <- data.frame(code = g$code, X = pts[, 1], Y = pts[, 2])
  # Paris, Lyon, Marseille : la source publie la ville ; la géométrie, les arrondissements.
  plm <- list("75056" = sprintf("751%02d", 1:20), "69123" = sprintf("693%02d", 81:89),
              "13055" = sprintf("132%02d", 1:16))
  for (cty in names(plm)) if (!cty %in% pos$code) {
    a <- pos[pos$code %in% plm[[cty]], ]
    if (nrow(a)) pos <- rbind(pos, data.frame(code = cty, X = mean(a$X), Y = mean(a$Y)))
  }
  i <- match(m$code_commune, pos$code)
  X <- pos$X[i]; Y <- pos$Y[i]
  placed <- !is.na(X)
  pop <- ifelse(is.na(m$ctx_pop_latest), 0, as.numeric(m$ctx_pop_latest))
  drom <- placed & grepl("^97", m$code_dept)
  drom_depts <- sort(unique(m$code_dept[drom]))
  main <- placed & !drom
  W <- 1000; mg <- 14; bh <- 150; gap <- 10
  xr <- range(X[main]); yr <- range(Y[main])
  s  <- (W - 2 * mg) / diff(xr)
  Hm <- diff(yr) * s + 2 * mg
  gx <- gy <- rep(NA_real_, nrow(m))
  gx[main] <- mg + (X[main] - xr[1]) * s
  gy[main] <- mg + (yr[2] - Y[main]) * s
  top <- Hm + 26; nb <- length(drom_depts); bw <- (W - 2 * mg - gap * (nb - 1)) / nb
  boxes <- vector("list", nb)
  for (b in seq_len(nb)) {
    d <- drom_depts[b]; k <- which(drom & m$code_dept == d)
    bx <- mg + (b - 1) * (bw + gap)
    aw <- bw - 16; ah <- bh - 30
    sc <- min(aw / max(diff(range(X[k])), 1), ah / max(diff(range(Y[k])), 1))
    gx[k] <- bx + bw / 2 + (X[k] - mean(range(X[k]))) * sc
    gy[k] <- top + 22 + ah / 2 - (Y[k] - mean(range(Y[k]))) * sc
    boxes[[b]] <- list(code = d, name = m$libelle_dept[match(d, m$code_dept)], x = bx, y = top, w = bw, h = bh)
  }
  H <- top + bh + mg
  # Échelle des cercles : la surface totale des cercles de la métropole = 22 % de son cadre.
  kk <- sqrt(0.22 * W * Hm / (pi * sum(pop[main])))
  rmin <- 0.7
  rr <- pmax(kk * sqrt(pop), rmin)
  dx <- dy <- rep(NA_real_, nrow(m))
  groups <- c(list(list(idx = which(main), rect = c(0, W, 0, Hm))),
              lapply(seq_len(nb), function(b) list(idx = which(drom & m$code_dept == drom_depts[b]),
                                                   rect = c(boxes[[b]]$x, boxes[[b]]$x + bw, boxes[[b]]$y + 20, boxes[[b]]$y + bh))))
  for (gr in groups) {
    pk <- dorling_pack(gx[gr$idx], gy[gr$idx], rr[gr$idx], gr$rect)
    dx[gr$idx] <- pk$x; dy[gr$idx] <- pk$y
  }
  # Départements (ordre de idx$depts = code_dept trié) : position = barycentre pondéré par la population
  dl <- sort(unique(m$code_dept[!is.na(m$code_dept)]))
  dpos <- lapply(dl, function(d) {
    k <- which(m$code_dept == d & placed)
    if (!length(k)) return(NULL)
    w <- if (sum(pop[k]) > 0) pop[k] else rep(1, length(k))
    list(x = sum(gx[k] * w) / sum(w), y = sum(gy[k] * w) / sum(w), pop = sum(pop[k]), drom = d %in% drom_depts)
  })
  has <- !vapply(dpos, is.null, logical(1))
  dpop <- vapply(dpos[has], function(p) p$pop, numeric(1))
  dgx <- vapply(dpos[has], function(p) p$x, numeric(1)); dgy <- vapply(dpos[has], function(p) p$y, numeric(1))
  ddrom <- vapply(dpos[has], function(p) p$drom, logical(1))
  dr <- pmax(kk * sqrt(dpop), 2.5)
  ddx <- ddy <- numeric(sum(has))
  pk <- dorling_pack(dgx[!ddrom], dgy[!ddrom], dr[!ddrom], c(0, W, 0, Hm))
  ddx[!ddrom] <- pk$x; ddy[!ddrom] <- pk$y
  for (j in which(ddrom)) {
    b <- boxes[[match(dl[has][j], drom_depts)]]
    pk <- dorling_pack(dgx[j], dgy[j], dr[j], c(b$x, b$x + b$w, b$y + 20, b$y + b$h))
    ddx[j] <- pk$x; ddy[j] <- pk$y
  }
  full <- function(v) { o <- rep(NA_real_, length(dl)); o[has] <- v; round(o, 1) }
  obj <- list(w = W, h = round(H, 1), hm = round(Hm, 1), k = round(kk, 5), rmin = rmin,
              boxes = lapply(boxes, function(b) { b[c("x", "y", "w", "h")] <- lapply(b[c("x", "y", "w", "h")], round, 1); b }),
              gx = round(gx, 1), gy = round(gy, 1),
              dep = list(c = dl, gx = full(dgx), gy = full(dgy), dx = full(ddx), dy = full(ddy), r = full(dr)))
  writeLines(jsonlite::toJSON(obj, auto_unbox = TRUE, na = "null", digits = NA), o1, useBytes = TRUE)
  writeLines(jsonlite::toJSON(list(dx = round(dx, 1), dy = round(dy, 1)), auto_unbox = FALSE, na = "null", digits = NA), o2, useBytes = TRUE)
  message("[assets] constellation : ", sum(placed), " communes placées sur ", nrow(m),
          " ; ", sum(has), " départements ; ", format(file.size(o1), big.mark = " "), " + ",
          format(file.size(o2), big.mark = " "), " o")
}

#' Fiches communales de la page Législatives (pages/elections.qmd) : fichier colonnes
#' assets/data/elections_lookup.json, téléchargé au premier clic. GÉNÉRÉ ICI (pré-rendu) et
#' non dans la page : Quarto fige la liste des `resources:` avant de rendre les pages, donc un
#' fichier écrit pendant le rendu n'arrivait jamais dans _site (404 sur Pages, audit C5).
#' Une ligne par code de la géométrie métropolitaine, communes avec famille gagnante seulement.
#' Paris, Lyon, Marseille : une ligne PAR ARRONDISSEMENT (résultat propre, fichier
#' legislatives_2024_arrondissements.parquet) + une ligne VILLE (somme, code 75056/69123/13055 : bulle
#' de la vue « Électeurs »). La série de participation 2002-2026 reste celle de la ville.
build_elections_lookup <- function() {
  ep <- file.path("data", "processed", "elections", "legislatives_2024_t2.parquet")
  pp <- file.path("data", "processed", "elections", "participation_long.parquet")
  pc <- file.path("data", "processed", "elections", "participation_commune.parquet")
  pop_p <- file.path("data", "processed", "insee", "populations.parquet")
  if (!all(file.exists(c(ep, pp, pc, pop_p, geo_src)))) {
    message("[assets] elections_lookup : sources absentes, non généré"); return(invisible())
  }
  out <- file.path("assets", "data", "elections_lookup.json")
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  elec <- arrow::read_parquet(ep)
  ap <- file.path("data", "processed", "elections", "legislatives_2024_arrondissements.parquet")
  elec_arr <- if (file.exists(ap)) arrow::read_parquet(ap) else elec[0, ]
  elec <- rbind(elec, elec_arr[, names(elec)])
  stopifnot(!anyDuplicated(elec$code_insee))
  pop  <- arrow::read_parquet(pop_p, col_select = c("code_commune", "pop_latest", "strate_pop"))
  elec <- merge(elec, data.frame(code_insee = pop$code_commune, pop_latest = pop$pop_latest,
                                 strate_pop = as.character(pop$strate_pop)), by = "code_insee", all.x = TRUE)
  # Participation : séries d'abstention 2002-2026 + vues carte
  SER <- c("2002_pres_t1", "2007_pres_t1", "2012_pres_t1", "2017_pres_t1", "2022_pres_t1",
           "2002_legi_t1", "2007_legi_t1", "2012_legi_t1", "2017_legi_t1", "2022_legi_t1", "2024_legi_t1",
           "2004_euro_t1", "2009_euro_t1", "2014_euro_t1", "2019_euro_t1", "2024_euro_t1",
           "2014_muni_t1", "2020_muni_t1", "2026_muni_t1")
  pl <- arrow::read_parquet(pp)
  pl <- pl[pl$id_election %in% SER, c("code_commune", "id_election", "part_abstention")]
  pl$part_abstention <- round(pl$part_abstention, 1)
  ser <- tidyr::pivot_wider(pl, names_from = id_election, values_from = part_abstention, names_prefix = "s_")
  part <- dplyr::left_join(
    arrow::read_parquet(pc, col_select = c("code_commune", "abst_pres22_t1", "abst_euro24",
                                           "abst_muni26_t1", "abst_delta_pres02_pres22")) |>
      dplyr::mutate(dplyr::across(-code_commune, ~ round(.x, 1))), ser, by = "code_commune")
  # Géométrie : codes métropole (hors DROM), PLM -> ville
  g <- sf::st_drop_geometry(sf::read_sf(geo_src, quiet = TRUE))
  g <- g[!substr(g$code, 1, 3) %in% c("971", "972", "973", "974", "976"), c("code", "nom")]
  plm_city <- c(setNames(rep("75056", 20), sprintf("751%02d", 1:20)),
                setNames(rep("69123", 9), sprintf("693%02d", 81:89)),
                setNames(rep("13055", 16), sprintf("132%02d", 1:16)))
  g <- rbind(g, data.frame(code = unique(plm_city), nom = NA_character_))   # lignes ville (somme)
  g$join_code <- g$code                                     # résultat : propre à chaque code
  g$part_code <- ifelse(g$code %in% names(plm_city), plm_city[g$code], g$code)   # participation : ville
  d <- merge(g, elec, by.x = "join_code", by.y = "code_insee", all.x = TRUE, sort = FALSE)
  d <- d[!is.na(d$famille_vainqueur), ]
  d <- merge(d, part, by.x = "part_code", by.y = "code_commune", all.x = TRUE, sort = FALSE)
  d <- d[order(d$code), ]
  r2 <- c("pct_vainqueur", "marge_vainqueur", "pct_abstention", paste0("pct_", c("NFP", "ENS", "RN", "LR", "Divers")),
          paste0("pct_", c("NFP", "ENS", "RN", "LR", "Divers"), "_t1"), "pct_abstention_t1")
  d[r2] <- lapply(d[r2], round, 2)
  d$couverture_inscrits <- round(d$couverture_inscrits, 3)
  d$nom_commune <- ifelse(is.na(d$nom_commune), d$nom, d$nom_commune)
  keep <- c("nom_commune", "libelle_dept", "code_dept", "tour", "repli_t1", "famille_vainqueur", "nuance_vainqueur", r2,
            "couverture_inscrits", "voix_vainqueur", "voix_second", "inscrits", "exprimes", "abstentions", "pop_latest", "strate_pop",
            setdiff(names(part), "code_commune"))
  lk <- c(list(c = d$code), as.list(d[keep]))
  writeLines(jsonlite::toJSON(lk, na = "null", digits = NA), out, useBytes = TRUE)
  message("[assets] elections_lookup.json : ", nrow(d), " codes, ", format(file.size(out), big.mark = " "), " o")
}

build_geometry()
build_columns()
build_flows()
build_constellation()
build_elections_lookup()
