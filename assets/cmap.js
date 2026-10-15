/* ============================================================
 * cmap.js : module carte partagé par toutes les pages thématiques.
 *
 * Remplace, pour les 12 pages cartographiques :
 *   - la géométrie des communes recopiée dans chaque HTML (désormais
 *     assets/geo/communes.json, téléchargée une fois puis en cache) ;
 *   - les infobulles HTML générées en R pour chaque commune (désormais
 *     construites au survol, à partir d'une courte chaîne par commune) ;
 *   - six copies du binning JS (sans garde-fou zéro, légende figée) :
 *     une seule implémentation, miroir de R/map_helpers.R::cmap_breaks().
 *
 * API publique : window.CMap
 *   mount(map, el, d)   appelé par le onRender généré en R (cmap_widget)
 *   pick(map, fn)       clic commune -> fn(code) + surbrillance
 *   recolor(opts)       recolore toutes les cartes de la page + légende
 *   makeBreaks(v, scale) / classOf(v, br) / drawLegend(el, st)
 *   data.index() / data.col(champ)   colonnes pour les pages d'analyse
 *   loading(el, texte) / done(el)    états de chargement
 * ============================================================ */
(function () {
  "use strict";

  var script = document.currentScript;
  var BASE = script && script.src ? script.src.replace(/cmap\.js(\?.*)?$/, "") : "/assets/";

  function css(name, fallback) {
    try {
      var v = getComputedStyle(document.documentElement).getPropertyValue(name).trim();
      return v || fallback;
    } catch (e) { return fallback; }
  }
  function esc(s) {
    return String(s).replace(/[&<>"]/g, function (c) {
      return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c];
    });
  }
  function fmt(v, digits) {
    if (v == null || !isFinite(v)) return "—";
    return Number(v).toLocaleString("fr-FR", {
      minimumFractionDigits: digits || 0, maximumFractionDigits: digits || 0
    });
  }

  var CMap = { maps: [], main: null, esc: esc, fmt: fmt, base: BASE };
  window.CMap = CMap;

  /* ---------- Chargement JSON mis en cache (une requête par URL) ---------- */
  var cache = {};
  function getJSON(url) {
    if (!cache[url]) {
      cache[url] = fetch(url).then(function (r) {
        if (!r.ok) throw new Error(url + " : HTTP " + r.status);
        return r.json();
      });
    }
    return cache[url];
  }
  CMap.getJSON = getJSON;

  /* ---------- Géométrie : décodage des anneaux en deltas ---------- */
  var geoP = null;
  function decodeGeo(raw) {
    var q = raw.q, n = raw.c.length, idx = {}, latlngs = new Array(n);
    for (var i = 0; i < n; i++) {
      idx[raw.c[i]] = i;
      var polys = raw.g[i], out = [];
      for (var p = 0; p < polys.length; p++) {
        var rings = [];
        for (var r = 0; r < polys[p].length; r++) {
          var f = polys[p][r], ring = [], x = 0, y = 0;
          for (var k = 0; k < f.length; k += 2) {
            x += f[k]; y += f[k + 1];
            ring.push([y / q, x / q]);              // [lat, lng]
          }
          rings.push(ring);
        }
        out.push(rings);
      }
      latlngs[i] = out.length === 1 ? out[0] : out;   // 1 polygone : [anneaux]
    }
    return { idx: idx, names: raw.n, codes: raw.c, latlngs: latlngs };
  }
  CMap.geo = function () {
    if (!geoP) geoP = getJSON(BASE + "geo/communes.json").then(function (raw) {
      var g = decodeGeo(raw);
      // Table code -> {nom, dept} pour les panneaux (remplace le JSON de 35 000
      // noms recopié dans chaque page par R::emit_commune_names_js()).
      var cn = window.communeNames = window.communeNames || {};
      for (var i = 0; i < raw.c.length; i++) {
        var c = raw.c[i];
        cn[c] = { nom: raw.n[i], dept: c.indexOf("97") === 0 ? c.slice(0, 3) : c.slice(0, 2) };
      }
      cn["75056"] = { nom: "Paris", dept: "75" };
      cn["69123"] = { nom: "Lyon", dept: "69" };
      cn["13055"] = { nom: "Marseille", dept: "13" };
      return g;
    });
    return geoP;
  };

  /* ---------- Hachures "non publié" sur le rendu canvas de Leaflet ---------- */
  var naPattern = null;
  function hatch(ctx) {
    if (!naPattern) {
      // Hachure légère : fond neutre, trait fin à faible opacité (lisible comme
      // "non publié" sans dominer la carte quand la couverture est partielle).
      var t = document.createElement("canvas");
      t.width = t.height = 12;
      var c = t.getContext("2d");
      c.fillStyle = css("--na-fill", "#E9EBED");
      c.fillRect(0, 0, 12, 12);
      c.strokeStyle = css("--na-line", "#7C8188");
      c.globalAlpha = 0.42;
      c.lineWidth = 0.9;
      c.beginPath();
      c.moveTo(-1, 13); c.lineTo(13, -1);
      c.moveTo(-1, 1); c.lineTo(1, -1);
      c.moveTo(11, 13); c.lineTo(13, 11);
      c.stroke();
      naPattern = ctx.createPattern(t, "repeat");
    }
    return naPattern;
  }
  if (window.L && L.Canvas) {
    var origFS = L.Canvas.prototype._fillStroke;
    L.Canvas.include({
      _fillStroke: function (ctx, layer) {
        var o = layer.options;
        if (o.cmapNA && o.fill) {
          ctx.globalAlpha = 1;
          ctx.fillStyle = hatch(ctx);
          ctx.fill(o.fillRule || "evenodd");
          var keep = o.fill;
          o.fill = false;                 // trait seul via la méthode d'origine
          origFS.call(this, ctx, layer);
          o.fill = keep;
          return;
        }
        return origFS.call(this, ctx, layer);
      }
    });
  }

  /* ---------- Binning : fonctions pures dans assets/cmap-stats.js ---------- */
  var ST = window.CMapStats;
  CMap.makeBreaks = ST.makeBreaks;
  CMap.classOf = ST.classOf;

  /* ---------- Index des communes (population, densité, département) ---------- */
  // Un seul chargement, partagé (légende, gradient, repères) : mis en cache HTTP.
  var ixP = null;
  CMap.index = function () {
    if (!ixP) ixP = Promise.all([CMap.data.index(), CMap.data.col("ctx_pop_latest"), CMap.data.col("densite_rang")])
      .then(function (r) {
        var ix = r[0], pos = {};
        if (ix.dl && ix.dl.length === 6) {          // libellés issus des données : écrase le repli (en place)
          ix.dl.forEach(function (l, k) { DENS_SHORT[k] = DENS_SHORT_BY_LABEL[l] || l; DENS_RANKED[k] = l; });
        }
        var metro = [];
        for (var i = 0; i < ix.c.length; i++) { pos[ix.c[i]] = i; if (!/^9[78]/.test(ix.c[i])) metro.push(i); }
        // metro : positions des communes de métropole (univers des parts de la légende)
        return { codes: ix.c, names: ix.n, dept: ix.d, depts: ix.depts, pop: r[1], dens: r[2], pos: pos, metro: metro, raw: ix };
      });
    return ixP;
  };
  // Arrondissements PLM -> commune entière (les sources thématiques publient souvent la ville seule).
  var PLM = {};
  (function () {
    var i, k;
    for (i = 1; i <= 20; i++) PLM["751" + (i < 10 ? "0" : "") + i] = "75056";
    for (i = 81; i <= 89; i++) PLM["693" + i] = "69123";
    for (i = 1; i <= 16; i++) PLM["132" + (i < 10 ? "0" : "") + i] = "13055";
  })();
  CMap.plmCity = function (code) { return PLM[code] || code; };
  var PLM_ARR = {};
  Object.keys(PLM).forEach(function (a) { (PLM_ARR[PLM[a]] = PLM_ARR[PLM[a]] || []).push(a); });

  /* ---------- Légende (dessinée en JS : toujours synchrone avec la carte) ---------- */
  // Libellés des 6 classes de densité INSEE, dans l'ordre du rang (1 rural … 6 grands centres).
  // SOURCE UNIQUE = grille_densite.parquet (scripts/10_), émise dans l'index communes.json (`dl`)
  // par R/build_web_assets.R. Les libellés courts sont indexés par le LIBELLÉ INSEE, jamais par
  // la position : un ordre divergent est impossible. Le tableau ci-dessous n'est que le repli
  // avant chargement de l'index (vérifié contre les données par tests/check_cmap_stats.js).
  var DENS_SHORT_BY_LABEL = {
    "Rural non périurbain": "Rural isolé", "Rural périurbain": "Rural périurbain",
    "Ceintures urbaines": "Ceintures urbaines", "Petites villes": "Petites villes",
    "Centres urbains intermédiaires": "Centres interm.", "Grands centres urbains": "Grands centres"
  };
  var DENS_RANKED = ["Rural non périurbain", "Rural périurbain", "Ceintures urbaines",
                     "Petites villes", "Centres urbains intermédiaires", "Grands centres urbains"];
  var DENS_SHORT = DENS_RANKED.map(function (l) { return DENS_SHORT_BY_LABEL[l]; });
  CMap.densLabels = DENS_SHORT;
  CMap.densShortByLabel = DENS_SHORT_BY_LABEL;
  CMap.densRanked = DENS_RANKED;

  function pct(x) {
    if (x == null) return "";
    return (x > 0 && x < 1 ? "<1" : fmt(x, 0)) + " %";
  }
  /** Histogramme de légende : barres colorées par classe, seuils en filets.
   *  Hauteur ∝ √effectif (les distributions saturées en zéros restent lisibles). */
  function histSVG(h, st) {
    if (!h || !st.breaks) return "";
    var W = 200, H = 34, nb = h.counts.length, bw = W / nb, mx = 0, i, out = "";
    h.counts.forEach(function (c) { mx = Math.max(mx, Math.sqrt(c)); });
    if (!(mx > 0)) return "";
    for (i = 0; i < nb; i++) {
      var xm = h.x0 + (i + 0.5) / nb * (h.x1 - h.x0), v = h.log ? Math.expm1(xm) : xm;
      var cls = Math.max(1, Math.min(st.palette.length, CMapStats.classOf(v, st.breaks)));
      var bh = Math.max(h.counts[i] > 0 ? 1 : 0, Math.sqrt(h.counts[i]) / mx * (H - 2));
      out += '<rect x="' + (i * bw + 0.3).toFixed(1) + '" y="' + (H - bh).toFixed(1) + '" width="' + (bw - 0.6).toFixed(1) +
             '" height="' + bh.toFixed(1) + '" fill="' + st.palette[cls - 1] + '"/>';
    }
    st.breaks.slice(1, -1).forEach(function (b) {
      var t = h.log ? Math.log1p(Math.max(0, b)) : b, x = (t - h.x0) / (h.x1 - h.x0) * W;
      if (x > 0 && x < W) out += '<line x1="' + x.toFixed(1) + '" x2="' + x.toFixed(1) + '" y1="0" y2="' + H + '" class="lgd-brk"/>';
    });
    return '<svg class="lgd-hist" viewBox="0 0 ' + W + " " + H + '" width="' + W + '" height="' + H +
           '" role="img" aria-label="Distribution des valeurs par commune, seuils de classes en filets (hauteur ∝ racine de l’effectif)">' +
           out + '<line x1="0" x2="' + W + '" y1="' + (H - 0.5) + '" y2="' + (H - 0.5) + '" class="lgd-axis"/></svg>';
  }
  CMap.drawLegend = function (el, st) {
    if (!el) return;
    var d = st.digits || 0, rows = "", i, lab, sh = st.stats && st.stats.sh, hasSt = !!sh;
    for (i = 1; i <= st.palette.length; i++) {
      if (st.cats) lab = st.cats[i - 1];
      else if (i === 1 && st.zero) lab = "0";
      else lab = fmt(st.breaks[i - 1], d) + " – " + fmt(st.breaks[i], d);
      rows += '<div class="lgd-row"><span class="lgd-swatch" style="background:' + st.palette[i - 1] +
              '"></span><span class="lgd-label">' + esc(lab) + (st.unit && !st.cats && !(i === 1 && st.zero)
              ? " " + esc(st.unit) : "") + "</span>" +
              (hasSt ? '<span class="lgd-pc">' + pct(sh.pc[i - 1]) + '</span><span class="lgd-pp">' + pct(sh.pp[i - 1]) + "</span>" : "") +
              "</div>";
    }
    rows += '<div class="lgd-row lgd-row-na"><span class="lgd-swatch lgd-swatch-na"></span>' +
            '<span class="lgd-label">' + esc(st.naLabel || "Non publié (secret statistique)") + "</span>" +
            (hasSt ? '<span class="lgd-pc">' + pct(sh.naC) + '</span><span class="lgd-pp">' + pct(sh.naP) + "</span>" : "") + "</div>";
    var head = hasSt ? '<div class="lgd-head"><span class="lgd-label"></span><span class="lgd-pc" title="Part des communes de la classe">% comm.</span>' +
               '<span class="lgd-pp" title="Part de la population de France métropolitaine vivant dans ces communes">% pop.</span></div>' : "";
    var open = el.querySelector("details") ? el.querySelector("details").open : true;
    el.innerHTML = '<details class="lgd-box' + (hasSt ? " lgd-wide" : "") + '"' + (open ? " open" : "") + '><summary class="lgd-title">' +
      esc(st.title) + "</summary>" + (st.sub ? '<div class="lgd-sub">' + esc(st.sub) + "</div>" : "") +
      (hasSt && st.stats.hist ? histSVG(st.stats.hist, st) : "") + head + rows +
      (hasSt ? '<div class="lgd-note">Parts sur ' + fmt(sh.n, 0) + " communes de France métropolitaine (hors DROM), somme = 100 %.</div>" : "") +
      (st.note ? '<div class="lgd-note">' + esc(st.note) + "</div>" : "") + "</details>";
    el._lgdState = st;
    bindBrush(el, st);
  };

  /* ---------- Brosse sur l'histogramme de légende (toutes les cartes thématiques) ----------
   * Glisser sur l'histogramme : les communes dont la valeur est hors plage s'estompent
   * (fillOpacity), les autres restent à pleine opacité. Un clic hors plage l'efface.
   * Réinitialisée à chaque recoloration. Générique : lit seulement CMap.cur.get(code). */
  CMap._brush = null; CMap._rc = 0;
  function histVal(h, px, W) {
    var t = px / W; if (t <= 0.01) return -Infinity; if (t >= 0.99) return Infinity;
    var x = h.x0 + t * (h.x1 - h.x0); return h.log ? Math.expm1(x) : x;
  }
  CMap.brush = function (lo, hi) {
    var cur = CMap.cur, b = lo == null ? null : [lo, hi], n = 0, tot = 0;
    CMap._brush = b;
    if (!cur || !cur.get) return;
    CMap.maps.forEach(function (m) {
      if (!m._cmapLayers) return;
      Object.keys(m._cmapLayers).forEach(function (c) {
        var l = m._cmapLayers[c];
        if (l._op0rc !== CMap._rc) { l._op0 = l.options.fillOpacity; l._op0rc = CMap._rc; }
        if (l.options.cmapNA) return;
        var v = cur.get(c), inside = !b || (v != null && isFinite(v) && v >= b[0] && v <= b[1]);
        l.setStyle({ fillOpacity: inside ? l._op0 : 0.07 });
      });
    });
  };
  function brushReadout(el, b) {
    var out = el.querySelector(".lgd-brush-read"), cur = CMap.cur;
    if (!out) return;
    if (!b || !cur || !cur.get) { out.textContent = "Glissez sur l’histogramme pour isoler une plage."; return; }
    CMap.index().then(function (ix) {
      var n = 0, N = 0, pp = 0, pt = 0, i, v;
      for (i = 0; i < ix.metro.length; i++) {
        var p = ix.metro[i]; v = cur.get(ix.codes[p]);
        if (v == null || !isFinite(v)) continue;
        var w = ix.pop[p] > 0 ? ix.pop[p] : 0; N++; pt += w;
        if (v >= b[0] && v <= b[1]) { n++; pp += w; }
      }
      var d = (cur.legend && cur.legend.digits) || 0, u = (cur.legend && cur.legend.unit) || "";
      var lo = b[0] === -Infinity ? "min." : fmt(b[0], d), hi = b[1] === Infinity ? "max." : fmt(b[1], d);
      out.innerHTML = "<strong>" + fmt(n, 0) + "</strong> communes sur " + fmt(N, 0) + " (" + fmt(100 * n / Math.max(1, N), 1) + " %), " +
        fmt(pt ? 100 * pp / pt : 0, 1) + " % de la population, de " + lo + " à " + hi + (u ? " " + esc(u) : "") + ".";
    });
  }
  function bindBrush(el, st) {
    var svg = el.querySelector(".lgd-hist"), h = st.stats && st.stats.hist;
    if (!svg || !h) return;
    var W = 200, H = 34, drag = null, b = CMap._brush, box = svg.parentNode;
    var read = document.createElement("div"); read.className = "lgd-note lgd-brush-read"; read.setAttribute("aria-live", "polite");
    svg.insertAdjacentElement("afterend", read);
    svg.style.cursor = "crosshair"; svg.style.touchAction = "none";
    function overlay(a, z) {
      var g = svg.querySelector(".lgd-bsel"); if (g) g.remove();
      if (a == null) return;
      svg.insertAdjacentHTML("beforeend", '<rect class="lgd-bsel" x="' + Math.min(a, z).toFixed(1) + '" y="0" width="' + Math.max(1, Math.abs(z - a)).toFixed(1) +
        '" height="' + H + '" fill="currentColor" fill-opacity="0.14" stroke="currentColor" stroke-width="1" pointer-events="none"/>');
    }
    function px(ev) { var r = svg.getBoundingClientRect(); return Math.max(0, Math.min(W, (ev.clientX - r.left) / r.width * W)); }
    function toX(v) { if (v === -Infinity) return 0; if (v === Infinity) return W; var t = h.log ? Math.log1p(Math.max(0, v)) : v; return Math.max(0, Math.min(W, (t - h.x0) / (h.x1 - h.x0) * W)); }
    if (b) { overlay(toX(b[0]), toX(b[1])); brushReadout(el, b); } else brushReadout(el, null);
    svg.addEventListener("pointerdown", function (ev) { drag = { p0: px(ev) }; svg.setPointerCapture(ev.pointerId); ev.preventDefault(); });
    svg.addEventListener("pointermove", function (ev) { if (drag) overlay(drag.p0, px(ev)); });
    function end(ev) {
      if (!drag) return;
      var p1 = px(ev), a = Math.min(drag.p0, p1), z = Math.max(drag.p0, p1); drag = null;
      if (z - a < 2) { overlay(null); CMap.brush(null); brushReadout(el, null); return; }
      var lo = histVal(h, a, W), hi = histVal(h, z, W);
      overlay(a, z); CMap.brush(lo, hi); brushReadout(el, [lo, hi]);
    }
    svg.addEventListener("pointerup", end); svg.addEventListener("pointercancel", end);
  }

  /** Statistiques de légende sur les communes de métropole : parts de communes et de
   *  population par classe (+ non publié), histogramme des valeurs.
   *  clsOf(code) -> classe 1..K ou 0 ; valOf(code) -> valeur (histogramme) ou undefined. */
  function legendStats(clsOf, valOf, K, log) {
    return CMap.index().then(function (ix) {
      var cls = [], pops = [], vals = valOf ? [] : null, n = ix.metro.length, i, p, c;
      for (i = 0; i < n; i++) {
        p = ix.metro[i]; c = ix.codes[p];
        cls.push(clsOf(c)); pops.push(ix.pop[p]);
        if (vals) vals.push(valOf(c));
      }
      return { sh: CMapStats.classShares(cls, pops, K), hist: vals ? CMapStats.histogram(vals, log) : null };
    });
  }
  CMap.legendStats = legendStats;

  function legendSlot(map) {
    return map.getContainer().querySelector(".cmap-lgd");
  }

  /* ---------- Légendes repliées sur mobile ---------- */
  function collapseOnMobile(root) {
    if (!window.matchMedia || !matchMedia("(max-width: 600px)").matches) return;
    (root || document).querySelectorAll("details.lgd-box[open]").forEach(function (d) {
      if (!d._cmapAuto) { d._cmapAuto = true; d.removeAttribute("open"); }
    });
  }
  CMap.collapseLegends = collapseOnMobile;

  /* ---------- États de chargement ---------- */
  CMap.loading = function (el, text) {
    if (!el) return;
    var n = el.querySelector(":scope > .cmap-loading");
    if (!n) { n = document.createElement("div"); n.className = "cmap-loading"; el.appendChild(n); }
    n.textContent = text || "Chargement…";
  };
  CMap.done = function (el) {
    var n = el && el.querySelector(":scope > .cmap-loading");
    if (n) n.remove();
  };

  /* ---------- Couches communes ---------- */
  function ensureTip(map) {
    if (!map._cmapTip) {
      map._cmapTip = L.tooltip({ sticky: true, direction: "auto", offset: [10, 0], opacity: 1,
                                 className: "cmap-tip" });
    }
    return map._cmapTip;
  }

  function buildLayer(map, d, geo) {
    var s = d.layer, el = map.getContainer();
    var border = s.border || {};
    var ink = css("--ink", "#16181B");
    var naFill = css("--na-fill", "#E9EBED");
    var fg = L.featureGroup();
    var byCode = {};
    for (var k = 0; k < s.codes.length; k++) {
      var gi = geo.idx[s.codes[k]];
      if (gi == null) continue;
      var na = s.fill[k] == null;
      var p = L.polygon(geo.latlngs[gi], {
        stroke: true, color: s.bc ? s.bc[k] : (border.color || "#FFFFFF"),
        weight: s.bw ? s.bw[k] : (border.weight == null ? 0.1 : border.weight),
        opacity: border.opacity == null ? 0.4 : border.opacity,
        fill: true, fillColor: na ? naFill : s.fill[k],
        fillOpacity: na ? 1 : (Array.isArray(s.op) ? s.op[k] : (s.op == null ? 0.9 : s.op)),
        smoothFactor: 0.4, layerId: s.codes[k], cmapNA: na, _gi: gi,
        interactive: !d.inset || d.interactive === true
      });
      p._tip = s.tip ? s.tip[k] : null;
      p._b0 = { color: p.options.color, weight: p.options.weight, opacity: p.options.opacity };
      byCode[s.codes[k]] = p;
      fg.addLayer(p);
    }
    map._cmapLayers = byCode;
    map._cmapGeo = geo;

    if (!d.inset || d.interactive) {
      var tip = ensureTip(map), hover = null;
      fg.on("mouseover", function (e) {
        var l = e.layer; hover = l;
        if (l !== map._cmapSel) l.setStyle({ weight: 1.6, color: ink, opacity: 1 });
        var h = "<strong>" + esc(geo.names[l.options._gi]) + '</strong> <span class="tt-code">(' +
                esc(l.options.layerId) + ")</span>" + (l._tip ? '<br><span class="tt-val">' + esc(l._tip) + "</span>" : "");
        tip.setContent(h); tip.setLatLng(e.latlng); map.openTooltip(tip);
      });
      fg.on("mousemove", function (e) { tip.setLatLng(e.latlng); });
      fg.on("mouseout", function (e) {
        map.closeTooltip(tip);
        if (e.layer !== map._cmapSel) e.layer.setStyle(e.layer._b0);
        hover = null;
      });
      fg.on("click", function (e) { CMap.select(map, e.layer.options.layerId, true); });
    }
    // Ordre de dessin : communes sous les contours départementaux (non interactifs).
    var ov = [];
    map.eachLayer(function (l) { if (l.options && l.options.fill === false && l.bringToFront) ov.push(l); });
    fg.addTo(map);
    ov.forEach(function (l) { l.bringToFront(); });
    map._cmapGroup = fg;
    CMap.done(el);
    collapseOnMobile(el);
    initialStats(map, s);
    map.fire("cmap:ready");
  }

  /** Parts initiales (légende fournie par R, sans valeurs) : classes déduites des couleurs. */
  var ARR1 = { "75056": "75101", "69123": "69381", "13055": "13201" };
  function initialStats(map, s) {
    var lg = map._cmapLegend, slot = lg && legendSlot(map);
    if (!map._cmapMain || !lg || !lg.palette || !slot || lg.stats) return;
    var pal = lg.palette.map(function (c) { return String(c).toUpperCase(); }), fb = {};
    for (var i = 0; i < s.codes.length; i++) fb[s.codes[i]] = s.fill[i];
    legendStats(function (c) {
      var f = fb[c] != null ? fb[c] : fb[ARR1[c]];
      return f ? pal.indexOf(String(f).toUpperCase()) + 1 : 0;
    }, null, pal.length).then(function (stats) {
      if (map._recolored || map._cmapLegend !== lg) return;
      lg.stats = stats; CMap.drawLegend(slot, lg); collapseOnMobile(slot);
    });
  }

  /** Sélection d'une commune (surbrillance + callback de la page). */
  CMap.select = function (map, code, fromClick) {
    var l = map._cmapLayers && map._cmapLayers[code];
    if (map._cmapSel && map._cmapSel !== l) {
      map._cmapSel.setStyle(map._cmapSel._b0);
    }
    if (l) {
      map._cmapSel = l;
      l.setStyle({ weight: 2, color: css("--ink", "#16181B"), opacity: 1 });
      l.bringToFront();
    }
    if (fromClick && map._cmapPick) map._cmapPick(code);
  };

  /** Enregistre le callback de clic d'une page. */
  CMap.pick = function (map, fn) { map._cmapPick = fn; };

  /** Montage d'une carte (appelé par le onRender généré en R). */
  CMap.mount = function (map, el, d) {
    if (!d) return;
    map._cmapMeta = d.layer || {};
    CMap.maps.push(map);
    if (d.main) CMap.main = map;
    map._cmapMain = !!d.main; map._cmapInset = !!d.inset;
    var b = d.bounds;
    map._dataBounds = b ? L.latLngBounds([b[0], b[1]], [b[2], b[3]]) : null;
    map._pad = d.pad || [6, 6];
    function fit() {
      if (!map._dataBounds) return;
      map._fitting = true;
      map.invalidateSize(false);
      // Légende à droite : si la carte est assez large, on réserve sa largeur à droite
      // pour que la France (et la Corse) restent entièrement visibles à côté d'elle.
      var lg = !map._cmapInset && map.getContainer().querySelector(".cmap-lgd"), extra = 0;
      if (lg && lg.firstChild) {
        var lw = lg.offsetWidth + 20;
        if (el.clientWidth - lw >= 480) extra = lw;
      }
      map.fitBounds(map._dataBounds, { paddingTopLeft: map._pad, paddingBottomRight: [map._pad[0] + extra, map._pad[1]], animate: false });
      map._fitting = false;
    }
    map._cmapFit = fit;
    map.on("dragstart", function () { if (!map._fitting) map._userMoved = true; });
    map.on("zoomstart", function () { if (!map._fitting) map._userMoved = true; });
    fit();
    var last = [el.clientWidth, el.clientHeight], timer = null;
    if (window.ResizeObserver) {
      new ResizeObserver(function () {
        clearTimeout(timer);
        timer = setTimeout(function () {
          var w = el.clientWidth, h = el.clientHeight;
          if (Math.abs(w - last[0]) < 3 && Math.abs(h - last[1]) < 3) return;
          last = [w, h];
          if (map._userMoved) map.invalidateSize(false); else fit();
        }, 120);
      }).observe(el);
    }
    // Légende initiale (R fournit l'état ; JS dessine)
    if (d.legend) { var sl = legendSlot(map); if (sl) { CMap.drawLegend(sl, d.legend); map._cmapLegend = d.legend; } }
    if (d.layer) {
      CMap.loading(el, d.inset ? "" : "Chargement des communes…");
      CMap.geo().then(function (geo) { buildLayer(map, d, geo); fit(); })
        .catch(function (e) {
          console.error(e);
          CMap.loading(el, "Carte indisponible (" + e.message + ")");
        });
    }
  };

  /* ---------- Recoloration (radios, année, type...) ---------- */
  /** opts : {get(code)->v|null, scale, palette, breaks?, zero?, levels?, tip?, opacity?,
   *          opacityOf?(code, v), legend:{title,unit,digits,sub,note,naLabel}}.
   *  Seuils issus de la carte principale ; les cartouches suivent.
   *  Effets de bord : parts communes/population + histogramme dans la légende,
   *  CMap.cur (état courant, lu par le gradient urbain-rural). */
  CMap.recolor = function (opts) {
    var main = CMap.main;
    if (!main || !main._cmapLayers) return null;
    main._recolored = true; CMap._rc++; CMap._brush = null;
    var naFill = css("--na-fill", "#E9EBED"), op = opts.opacity == null ? 0.9 : opts.opacity;
    var lg0 = opts.legend || {}, st, K = opts.palette.length;
    var getS = CMapStats.withPlmFallback(opts.get, PLM_ARR);   // villes PLM comblées (parts de légende, gradient)
    function opOf(c, v) { return opts.opacityOf ? opts.opacityOf(c, v) : op; }
    if (opts.levels) {                       // catégoriel : classe = rang du niveau
      var clsOfLevel = function (v) {
        var ix = v == null ? -1 : (typeof v === "number" ? Math.round(v) - 1 : opts.levels.indexOf(v));
        return (ix < 0 || ix >= opts.levels.length) ? -1 : ix;
      };
      CMap.maps.forEach(function (m) {
        if (!m._cmapLayers) return;
        Object.keys(m._cmapLayers).forEach(function (c) {
          var l = m._cmapLayers[c], v = opts.get(c), ix = clsOfLevel(v);
          if (!m._cmapInset) l._tip = opts.tip ? opts.tip(c, v) : (ix < 0 ? "Non classée" : opts.levels[ix]);
          if (ix < 0) { l.options.cmapNA = true; l.setStyle({ fillColor: naFill, fillOpacity: 1 }); }
          else { l.options.cmapNA = false; l.setStyle({ fillColor: opts.palette[ix], fillOpacity: opOf(c, v) }); }
        });
      });
      var slc = legendSlot(main);
      var s3 = { title: lg0.title, palette: opts.palette, cats: opts.levels, naLabel: lg0.naLabel, sub: lg0.sub, note: lg0.note };
      if (slc) { CMap.drawLegend(slc, s3); collapseOnMobile(slc); }
      CMap.cur = { levels: opts.levels, legend: lg0, get: getS, palette: opts.palette };
      var tick = main._cmapLegendTick = (main._cmapLegendTick || 0) + 1;
      legendStats(function (c) { return clsOfLevel(getS(c)) + 1; }, null, K).then(function (stats) {
        s3.stats = stats; if (slc && main._cmapLegendTick === tick) { CMap.drawLegend(slc, s3); collapseOnMobile(slc); }
      });
      refreshGradient();
      return null;
    }
    if (opts.breaks) st = { breaks: opts.breaks, zero: !!opts.zero };
    else {
      var vals = [];
      Object.keys(main._cmapLayers).forEach(function (c) { vals.push(opts.get(c)); });
      st = CMap.makeBreaks(vals, opts.scale || "quantile");
    }
    CMap.maps.forEach(function (m) {
      if (!m._cmapLayers) return;
      Object.keys(m._cmapLayers).forEach(function (c) {
        var l = m._cmapLayers[c], v = opts.get(c);
        if (opts.tip) l._tip = opts.tip(c, v);
        else if (lg0.title && !m._cmapInset) {
          l._tip = (v == null || !isFinite(v)) ? "Non publié"
            : fmt(v, lg0.digits) + (lg0.unit ? " " + lg0.unit : "");
        }
        if (v == null || !isFinite(v)) {
          l.options.cmapNA = true; l.setStyle({ fillColor: naFill, fillOpacity: 1 });
        } else {
          l.options.cmapNA = false;
          l.setStyle({ fillColor: opts.palette[CMap.classOf(v, st.breaks) - 1], fillOpacity: opOf(c, v) });
        }
      });
    });
    var slot = legendSlot(main), tick2 = main._cmapLegendTick = (main._cmapLegendTick || 0) + 1;
    var s2 = { title: lg0.title, sub: lg0.sub, note: lg0.note, unit: lg0.unit, digits: lg0.digits,
               naLabel: lg0.naLabel, cats: lg0.cats, palette: opts.palette, breaks: st.breaks, zero: st.zero };
    if (slot) { CMap.drawLegend(slot, s2); collapseOnMobile(slot); }
    var vcache = {};
    function val(c) {
      if (!(c in vcache)) { var v = getS(c); vcache[c] = (v == null || !isFinite(v)) ? null : v; }
      return vcache[c];
    }
    CMap.cur = { get: val, legend: lg0, st: st, scale: opts.scale, palette: opts.palette };
    legendStats(function (c) { var v = val(c); return v == null ? 0 : CMap.classOf(v, st.breaks); }, val, K,
                opts.scale === "log_quantile").then(function (stats) {
      s2.stats = stats;
      if (slot && main._cmapLegendTick === tick2) { CMap.drawLegend(slot, s2); collapseOnMobile(slot); }
    });
    refreshGradient();
    return st;
  };

  /** Branche un groupe de boutons radio sur une fonction (délégation, pas de délai). */
  CMap.onRadios = function (name, fn) {
    document.addEventListener("change", function (e) {
      var t = e.target;
      if (t && t.name === name && t.type === "radio") fn(t.value, t);
    });
  };

  /* ---------- Gradient urbain-rural (onglet du panneau commune) ---------- */
  // Médiane pondérée par la population (point), P25-P75 (barre), P10-P90 (trait)
  // pour chacune des 6 classes de densité INSEE ; trait pointillé = médiane nationale pondérée.
  function niceTicks(lo, hi, n) {
    var span = hi - lo; if (!(span > 0)) return [lo];
    var raw = span / n, mag = Math.pow(10, Math.floor(Math.log10(raw))), f = raw / mag;
    var step = (f < 1.5 ? 1 : f < 3 ? 2 : f < 7 ? 5 : 10) * mag, t = [], v = Math.ceil(lo / step - 1e-9) * step;
    for (; v <= hi + step * 1e-9; v += step) t.push(Math.round(v / step) * step);
    return t;
  }
  function gradientSVG(el, ix, cur) {
    var W = Math.max(260, el.clientWidth || 320), rowH = 24, padL = 112, padR = 14, padT = 8, padB = 40;
    var H = padT + padB + rowH * 6, i, k, v;
    var vals = [], groups = [[], [], [], [], [], []], gw = [[], [], [], [], [], []], allV = [], allW = [];
    for (i = 0; i < ix.metro.length; i++) {
      var p = ix.metro[i]; v = cur.get(ix.codes[p]);
      if (v == null || !isFinite(v)) continue;
      var d = ix.dens[p], w = ix.pop[p] > 0 ? ix.pop[p] : 0;
      allV.push(v); allW.push(w);
      if (d >= 1 && d <= 6) { groups[d - 1].push(v); gw[d - 1].push(w); }
    }
    if (allV.length < 20) return '<p class="js-loading">Pas assez de communes avec une valeur.</p>';
    var log = cur.scale === "log_quantile", tf = function (x) { return log ? Math.log1p(Math.max(0, x)) : x; };
    var q = groups.map(function (g, j) { return CMapStats.weightedQuantiles(g, gw[j], [0.1, 0.25, 0.5, 0.75, 0.9]); });
    var nat = CMapStats.weightedQuantiles(allV, allW, [0.5])[0];
    var lo = Infinity, hi = -Infinity;
    q.forEach(function (r) { if (r[0] != null) { lo = Math.min(lo, r[0]); hi = Math.max(hi, r[4]); } });
    if (!isFinite(lo)) return '<p class="js-loading">Pas assez de données.</p>';
    if (cur.st && cur.st.zero) lo = Math.min(lo, 0);
    var a = tf(lo), b = tf(hi); if (!(b > a)) b = a + 1;
    var px = function (x) { return padL + (tf(x) - a) / (b - a) * (W - padL - padR); };
    var dg = cur.legend.digits || 0, unit = cur.legend.unit || "";
    var out = '<svg class="grad-svg" viewBox="0 0 ' + W + " " + H + '" width="' + W + '" height="' + H +
      '" role="img" aria-label="Valeur médiane pondérée par la population, par classe de densité INSEE">';
    var ticks = log ? [0, 1, 10, 100, 1000, 10000, 100000].filter(function (t) { return t >= lo * 0.999 && t <= hi * 1.001; })
                    : niceTicks(lo, hi, 4);
    if (log && ticks.length < 2) ticks = [lo, hi];
    ticks.forEach(function (t) {
      out += '<line x1="' + px(t).toFixed(1) + '" x2="' + px(t).toFixed(1) + '" y1="' + padT + '" y2="' + (H - padB) + '" class="grad-grid"/>' +
             '<text x="' + px(t).toFixed(1) + '" y="' + (H - padB + 13) + '" text-anchor="middle" class="grad-tick">' + fmt(t, t >= 100 ? 0 : dg) + "</text>";
    });
    for (k = 0; k < 6; k++) {
      var y = padT + rowH * k + rowH / 2, r = q[k];
      out += '<text x="' + (padL - 8) + '" y="' + (y + 4) + '" text-anchor="end" class="grad-lab">' + esc(DENS_SHORT[k]) + "</text>";
      if (r[2] == null) { out += '<text x="' + padL + '" y="' + (y + 4) + '" class="grad-tick">n.d.</text>'; continue; }
      out += '<line x1="' + px(r[0]).toFixed(1) + '" x2="' + px(r[4]).toFixed(1) + '" y1="' + y + '" y2="' + y + '" class="grad-whisk"/>' +
             '<rect x="' + px(r[1]).toFixed(1) + '" y="' + (y - 5) + '" width="' + Math.max(1.5, px(r[3]) - px(r[1])).toFixed(1) + '" height="10" class="grad-iqr"><title>' +
             esc(DENS_SHORT[k]) + " : médiane " + fmt(r[2], dg) + " " + esc(unit) + ", P25-P75 " + fmt(r[1], dg) + " à " + fmt(r[3], dg) + "</title></rect>" +
             '<circle cx="' + px(r[2]).toFixed(1) + '" cy="' + y + '" r="3.6" class="grad-med"/>';
    }
    out += '<line x1="' + px(nat).toFixed(1) + '" x2="' + px(nat).toFixed(1) + '" y1="' + padT + '" y2="' + (H - padB) + '" class="grad-nat"/>' +
           '<text x="' + ((padL + W - padR) / 2) + '" y="' + (H - 6) + '" text-anchor="middle" class="grad-axis">' + esc(cur.legend.title || "") + (unit ? "" : "") + "</text></svg>";
    return out + '<p class="grad-note">Point : médiane pondérée par la population ; barre : P25 à P75 ; trait fin : P10 à P90 ; pointillé : médiane nationale (' +
      fmt(nat, dg) + (unit ? " " + esc(unit) : "") + "). Classes : typologie de densité INSEE. Pondération : population INSEE, dernier recensement.</p>";
  }
  function refreshGradient() {
    document.querySelectorAll(".cp-grad").forEach(function (el) { if (!el.hidden) drawGradient(el); });
  }
  function drawGradient(el) {
    if (!CMap.cur || CMap.cur.levels) {
      el.innerHTML = '<p class="js-loading">Gradient disponible pour les indicateurs quantitatifs (choisir un indicateur sur la carte).</p>';
      return;
    }
    el.innerHTML = '<p class="js-loading">Calcul…</p>';
    CMap.index().then(function (ix) { el.innerHTML = gradientSVG(el, ix, CMap.cur); })
      .catch(function (e) { console.error(e); el.innerHTML = '<p class="js-loading">Gradient indisponible.</p>'; });
  }

  /* ---------- Repères : rang centile d'une commune (national, département, même densité) ---------- */
  // Bande 0-100 = rang centile national. Point = la commune. Repères = médianes
  // (France, département, communes de même densité INSEE), placées à leur propre rang national.
  var ctxSorted = {}, ctxGroups = {};
  function sortedAll(f, col) {
    if (!ctxSorted[f]) ctxSorted[f] = col.filter(function (x) { return x != null && isFinite(x); }).sort(function (a, b) { return a - b; });
    return ctxSorted[f];
  }
  function groupSorted(f, col, kind, gid, n) {
    var key = f + "|" + kind;
    if (!ctxGroups[key]) {
      var m = {};
      for (var i = 0; i < n; i++) {
        var v = col[i], g = gid(i);
        if (v == null || !isFinite(v) || g == null) continue;
        (m[g] = m[g] || []).push(v);
      }
      Object.keys(m).forEach(function (g) { m[g].sort(function (a, b) { return a - b; }); });
      ctxGroups[key] = m;
    }
    return ctxGroups[key];
  }
  function med(sorted) { return sorted && sorted.length ? CMapStats.quantile7(sorted, 0.5) : null; }
  function showVal(spec, v) {
    if (v == null) return "n.d.";
    var x = spec.log10 ? Math.pow(10, v) : v;
    return fmt(x, spec.d == null ? 0 : spec.d) + (spec.unit ? " " + esc(spec.unit) : "");
  }
  /** HTML des repères pour une commune. specs : [{f, label, unit, d, log10?}] ; cols alignées sur l'index. */
  function ctxRows(ix, cols, code, specs) {
    var c = CMap.plmCity(code), p = ix.pos[c];
    if (p == null) return '<p class="ctx-na">Commune absente de la base de comparaison.</p>';
    var dp = ix.dens[p], dd = ix.dept[p], out = "";
    specs.forEach(function (sp, j) {
      var col = cols[j], v = col[p];
      if (v == null || !isFinite(v)) {
        out += '<div class="ctx-row ctx-row-na"><div class="ctx-head"><span class="ctx-lab">' + esc(sp.label) + '</span><span class="cp-num">n.d.</span></div></div>';
        return;
      }
      var nat = sortedAll(sp.f, col), pn = CMapStats.percentile(nat, v);
      var dsets = groupSorted(sp.f, col, "d", function (i) { return ix.dept[i]; }, col.length)[dd];
      var csets = dp ? groupSorted(sp.f, col, "c", function (i) { return ix.dens[i]; }, col.length)[dp] : null;
      var pd = dsets && dsets.length >= 3 ? CMapStats.percentile(dsets, v) : null;
      var pc = csets && csets.length >= 3 ? CMapStats.percentile(csets, v) : null;
      var mN = med(nat), mD = dsets && dsets.length >= 3 ? med(dsets) : null, mC = csets && csets.length >= 3 ? med(csets) : null;
      function tk(cls, m, lab) {
        return m == null ? "" : '<i class="ctx-tk ' + cls + '" style="left:' + CMapStats.percentile(nat, m).toFixed(1) + '%" title="' +
               esc(lab) + " : " + showVal(sp, m).replace(/"/g, "") + '"></i>';
      }
      out += '<div class="ctx-row"><div class="ctx-head"><span class="ctx-lab">' + esc(sp.label) + '</span><span class="cp-num">' + showVal(sp, v) + "</span></div>" +
        '<div class="ctx-strip" role="img" aria-label="' + esc(sp.label) + " : rang centile national " + Math.round(pn) + ' sur 100">' +
        tk("ctx-nat", mN, "Médiane France") + tk("ctx-dep", mD, "Médiane du département") + tk("ctx-den", mC, "Médiane des communes de même densité") +
        '<b class="ctx-dot" style="left:' + pn.toFixed(1) + '%"></b></div>' +
        '<div class="ctx-foot"><span>France <b>' + Math.round(pn) + "</b></span>" +
        (pd != null ? "<span>dépt. <b>" + Math.round(pd) + "</b></span>" : "") +
        (pc != null ? "<span>même densité <b>" + Math.round(pc) + "</b></span>" : "") + "</div></div>";
    });
    return out;
  }
  var CTX_KEY = '<div class="ctx-key" aria-hidden="true"><span><b class="ctx-dot ctx-dot-key"></b>commune</span>' +
    '<span><i class="ctx-tk ctx-nat ctx-tk-key"></i>médiane France</span><span><i class="ctx-tk ctx-dep ctx-tk-key"></i>médiane dépt.</span>' +
    '<span><i class="ctx-tk ctx-den ctx-tk-key"></i>médiane même densité</span></div>';
  CMap.ctx = {
    /** Rend les repères dans un élément ; groups : [{title, specs:[...]}]. */
    render: function (el, code, groups) {
      var all = [];
      groups.forEach(function (g) { g.specs.forEach(function (s) { all.push(s); }); });
      return Promise.all([CMap.index(), CMap.data.cols(all.map(function (s) { return s.f; }))]).then(function (r) {
        var cols = r[1], o = 0, html = "";
        groups.forEach(function (g) {
          var cs = cols.slice(o, o + g.specs.length); o += g.specs.length;
          html += '<div class="cp-section ctx-sec">' + (g.title ? "<h4>" + esc(g.title) + "</h4>" : "") + ctxRows(r[0], cs, code, g.specs) + "</div>";
        });
        el.innerHTML = html + CTX_KEY +
          '<p class="ctx-note">Rang centile parmi les communes de France (0 = valeur la plus basse, 100 = la plus haute). Descriptif, non causal.</p>';
      });
    },
    /** Ajoute une section « Repères » en bas d'un panneau commune (après sa mise à jour par la page). */
    append: function (contentId, code, specs, title) {
      var host = document.getElementById(contentId);
      if (!host || !specs || !specs.length) return;
      var box = document.createElement("div");
      box.className = "ctx-host";
      host.appendChild(box);
      CMap.ctx.render(box, code, [{ title: title || "Repères : rang centile", specs: specs }]).catch(function (e) { console.error(e); box.remove(); });
    }
  };

  /* ---------- Panneau commune : onglets Commune / Gradient urbain-rural ---------- */
  function initPanels() {
    document.querySelectorAll(".commune-panel").forEach(function (panel) {
      if (panel._tabs || !panel.querySelector(".cp-content")) return;
      panel._tabs = true;
      var content = panel.querySelector(".cp-content");
      var bar = document.createElement("div");
      bar.className = "cp-tabs"; bar.setAttribute("role", "tablist");
      bar.innerHTML = '<button type="button" role="tab" aria-selected="true" data-tab="c">Commune</button>' +
                      '<button type="button" role="tab" aria-selected="false" data-tab="g">Gradient urbain-rural</button>';
      var grad = document.createElement("div");
      grad.className = "cp-grad"; grad.hidden = true;
      panel.insertBefore(bar, content); content.after(grad);
      bar.addEventListener("click", function (e) {
        var b = e.target.closest("button"); if (!b) return;
        var g = b.dataset.tab === "g";
        bar.querySelectorAll("button").forEach(function (x) { x.setAttribute("aria-selected", x === b ? "true" : "false"); });
        content.hidden = g; grad.hidden = !g;
        if (g) drawGradient(grad);
      });
    });
  }
  CMap.initPanels = initPanels;
  if (document.readyState !== "loading") initPanels(); else document.addEventListener("DOMContentLoaded", initPanels);
  window.addEventListener("load", initPanels);

  /* ---------- Colonnes (pages d'analyse) ---------- */
  var dataAPI = {
    index: function () { return getJSON(BASE + "data/communes.json"); },
    col: function (f) { return getJSON(BASE + "data/col/" + f + ".json"); },
    cols: function (fs) { return Promise.all(fs.map(dataAPI.col)); }
  };
  CMap.data = dataAPI;

  /* ---------- Mobile : repliage des légendes ajoutées plus tard ---------- */
  window.addEventListener("load", function () {
    collapseOnMobile(document);
    setTimeout(function () { collapseOnMobile(document); }, 1200);
  });
})();
