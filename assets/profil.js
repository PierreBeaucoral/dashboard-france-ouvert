/* ============================================================
 * profil.js : radar à 8 dimensions d'une commune (et comparaison).
 * Données : colonnes téléchargées à la demande (CMap.data), rangs centiles
 * calculés ici. Configuration (axes) injectée par pages/profil.qmd :
 *   window.PROFIL_AXES = [{field, label, label_long}]
 * Couleurs : commune A = encre, trait plein ; commune B = gris, tirets
 * (aucune teinte rouge/bleue : lecture partisane en France).
 * ============================================================ */
(function () {
  "use strict";
  var AXES = window.PROFIL_AXES || [];
  var S = { idx: null, vals: {}, pct: {}, med: {}, eligible: null, primary: null, compare: null, names: [] };

  var esc = function (s) { return CMap.esc(s); };

  /* Rang centile moyen (0 à 100), ex æquo au rang moyen, NA conservés. */
  function percentRank(col) {
    var items = [], i, n = col.length, out = new Array(n);
    for (i = 0; i < n; i++) { out[i] = null; if (col[i] != null) items.push([col[i], i]); }
    items.sort(function (a, b) { return a[0] - b[0]; });
    var m = items.length, j = 0;
    while (j < m) {
      var k = j; while (k + 1 < m && items[k + 1][0] === items[j][0]) k++;
      var r = (j + k) / 2;                              // rang moyen (base 0)
      for (var t = j; t <= k; t++) out[items[t][1]] = m > 1 ? 100 * r / (m - 1) : 50;
      j = k + 1;
    }
    return out;
  }
  function median(col) {
    var v = col.filter(function (x) { return x != null; }).sort(function (a, b) { return a - b; });
    var m = v.length; if (!m) return null;
    return m % 2 ? v[(m - 1) / 2] : (v[m / 2 - 1] + v[m / 2]) / 2;
  }
  // La population est stockée en log10 : on l'affiche en habitants.
  function showRaw(field, v) {
    if (v == null) return "—";
    if (field === "ctx_log_pop") return CMap.fmt(Math.round(Math.pow(10, v)), 0);
    var d = Math.abs(v) >= 100 ? 0 : (Math.abs(v) >= 10 ? 1 : 2);
    return CMap.fmt(v, d);
  }

  function load() {
    return Promise.all([CMap.data.index(), CMap.data.cols(AXES.map(function (a) { return a.field; }))]).then(function (r) {
      S.idx = r[0];
      AXES.forEach(function (a, i) {
        S.vals[a.field] = r[1][i];
        S.pct[a.field] = percentRank(r[1][i]);
        S.med[a.field] = median(r[1][i]);
      });
      var n = S.idx.c.length;
      S.eligible = {}; S.row = {};
      for (var i = 0; i < n; i++) {
        S.row[S.idx.c[i]] = i;
        var filled = 0;
        AXES.forEach(function (a) { if (S.pct[a.field][i] != null) filled++; });
        // Au moins 5 axes sur 8 pour qu'un radar soit interprétable (le portrait, lui, s'affiche toujours).
        if (filled >= 5) S.eligible[S.idx.c[i]] = true;
        S.names.push([S.idx.n[i].toLowerCase() + " (" + S.idx.c[i].toLowerCase() + ")", S.idx.c[i]]);
      }
      AXES.forEach(buildDist);
    });
  }

  function info(code) {
    var i = S.row[code]; if (i == null) return null;
    var d = S.idx.depts[S.idx.d[i]];
    return { code: code, i: i, nom: S.idx.n[i], dept: d ? d.nom + " (" + d.code + ")" : "" };
  }

  /* ---- Radar SVG ---- */
  var CX = 220, CY = 180, R = 130, NAX = AXES.length;
  function pt(ax, pct) {
    var a = (Math.PI * 2 * ax / NAX) - Math.PI / 2, r = R * pct / 100;
    return [CX + Math.cos(a) * r, CY + Math.sin(a) * r];
  }
  function poly(i, cls) {
    var pts = [], full = true, s = "", ax, p;
    for (ax = 0; ax < NAX; ax++) {
      var v = S.pct[AXES[ax].field][i];
      pts.push(v == null ? null : pt(ax, v)); if (v == null) full = false;
    }
    if (full) {
      s += '<polygon class="rd-fill rd-' + cls + '" points="' + pts.map(function (q) { return q[0].toFixed(1) + "," + q[1].toFixed(1); }).join(" ") + '"/>';
    }
    // Lignes seulement entre axes voisins renseignés : un axe manquant casse le tracé.
    for (ax = 0; ax < NAX; ax++) {
      var nx = (ax + 1) % NAX;
      if (pts[ax] && pts[nx]) {
        s += '<line class="rd-line rd-' + cls + '" x1="' + pts[ax][0].toFixed(1) + '" y1="' + pts[ax][1].toFixed(1) +
             '" x2="' + pts[nx][0].toFixed(1) + '" y2="' + pts[nx][1].toFixed(1) + '"/>';
      }
    }
    for (ax = 0; ax < NAX; ax++) {
      p = pts[ax]; if (!p) continue;
      s += cls === "a" ? '<circle class="rd-dot rd-a" cx="' + p[0].toFixed(1) + '" cy="' + p[1].toFixed(1) + '" r="3.2"/>'
                       : '<rect class="rd-dot rd-b" x="' + (p[0] - 3).toFixed(1) + '" y="' + (p[1] - 3).toFixed(1) + '" width="6" height="6"/>';
    }
    return s;
  }
  function radar(a, b) {
    var svg = '<svg viewBox="0 0 440 360" width="100%" height="100%" preserveAspectRatio="xMidYMid meet" role="img" aria-label="Radar de rangs centiles">', k, ax;
    [0.2, 0.4, 0.6, 0.8, 1].forEach(function (f) { svg += '<circle class="rd-grid" cx="' + CX + '" cy="' + CY + '" r="' + R * f + '"/>'; });
    for (ax = 0; ax < NAX; ax++) {
      var ang = (Math.PI * 2 * ax / NAX) - Math.PI / 2, x2 = CX + Math.cos(ang) * R, y2 = CY + Math.sin(ang) * R;
      svg += '<line class="rd-grid" x1="' + CX + '" y1="' + CY + '" x2="' + x2.toFixed(1) + '" y2="' + y2.toFixed(1) + '"/>';
      var lx = CX + Math.cos(ang) * (R + 22), ly = CY + Math.sin(ang) * (R + 22);
      var anchor = Math.abs(Math.cos(ang)) < 0.2 ? "middle" : (Math.cos(ang) > 0 ? "start" : "end");
      var missing = S.pct[AXES[ax].field][a.i] == null || (b && S.pct[AXES[ax].field][b.i] == null);
      svg += '<text class="rd-lab' + (missing ? " rd-nd" : "") + '" x="' + lx.toFixed(1) + '" y="' + (ly + 4).toFixed(1) + '" text-anchor="' + anchor + '">' +
             esc(AXES[ax].label) + (missing ? " (n.d.)" : "") + "</text>";
    }
    svg += b ? poly(b.i, "b") : "";
    svg += poly(a.i, "a");
    svg += '<text class="rd-tick" x="' + CX + '" y="' + (CY - R * 0.5 - 4) + '" text-anchor="middle">50</text>';
    svg += '<text class="rd-tick" x="' + CX + '" y="' + (CY - R - 4) + '" text-anchor="middle">100</text></svg>';
    return svg;
  }

  function pctCell(p) {
    if (p == null) return '<td class="num nd">n.d.</td>';
    return '<td class="num"><span class="prof-bar"><i style="width:' + p.toFixed(0) + '%"></i></span>' + p.toFixed(0) + "e</td>";
  }

  function render(code) {
    var a = info(code), canvas = document.getElementById("radar-canvas"), box = document.getElementById("radar-info");
    if (!a) return;
    S.primary = code;
    var b = (S.compare && S.compare !== code) ? info(S.compare) : null;
    updatePortrait(a, b);
    if (!S.eligible[code]) {
      canvas.innerHTML = '<p class="js-loading">Radar indisponible : moins de 5 dimensions publiées pour cette commune.</p>';
    } else {
      canvas.innerHTML = radar(a, b);
    }
    box.innerHTML = "";
    try { history.replaceState(null, "", "#" + code + (b ? "," + b.code : "")); } catch (e) {}
  }

  /* ================= Portrait vertical ================= */
  // Axes STABLES : le domaine (P1-P99) et la densité de chaque ligne sont calculés une fois sur
  // toutes les communes publiées ; ils ne bougent jamais quand la commune change.
  function buildDist(ax) {
    S.dist = S.dist || {}; S.dist[ax.field] = PortraitKit.buildDist(ax, S.vals[ax.field]);
  }
  var axVal = PortraitKit.axVal;
  function pos(ax, v) { return PortraitKit.pos(ax, S.dist[ax.field], v); }
  function distSVG(ax) { return PortraitKit.distSVG(S.dist[ax.field]); }
  function ends(ax) { return PortraitKit.ends(ax, S.dist[ax.field]); }
  function buildPortrait() {
    var host = document.getElementById("pt-rows"); if (!host || host.firstChild) return;
    host.innerHTML = AXES.map(function (ax) {
      var e = ends(ax);
      return '<div class="pt-row" data-f="' + ax.field + '"><div class="pt-lab">' + esc(ax.label_long || ax.label) +
        '<span class="pt-src">' + esc(ax.src || "") + '</span></div>' +
        '<div class="pt-plot">' + distSVG(ax) + '<i class="pt-mk pt-a" aria-hidden="true"></i><i class="pt-mk pt-b" aria-hidden="true"></i>' +
        '<div class="pt-axis"><span>' + e[0] + '</span><span>' + e[1] + '</span></div></div>' +
        '<div class="pt-val"><b class="pt-va"></b><span class="pt-vb"></span></div></div>';
    }).join("");
  }
  function setMarker(el, x) {
    if (x == null) { el.style.opacity = "0"; return; }
    el.style.opacity = "1"; el.style.left = x.toFixed(1) + "%";
  }
  function updatePortrait(a, b) {
    buildPortrait();
    AXES.forEach(function (ax) {
      var row = document.querySelector('.pt-row[data-f="' + ax.field + '"]'); if (!row) return;
      var va = S.vals[ax.field][a.i], vb = b ? S.vals[ax.field][b.i] : null;
      setMarker(row.querySelector(".pt-a"), pos(ax, va));
      setMarker(row.querySelector(".pt-b"), b ? pos(ax, vb) : null);
      row.querySelector(".pt-va").innerHTML = axVal(ax, va);
      row.querySelector(".pt-vb").innerHTML = b ? '<span class="pt-pin" aria-hidden="true"></span>' + axVal(ax, vb) : "";
    });
    var pop = S.vals.ctx_log_pop ? S.vals.ctx_log_pop[a.i] : null, dens = S.vals.densite_rang ? S.vals.densite_rang[a.i] : null;
    var id = document.getElementById("pt-id");
    if (id) id.innerHTML = '<h3 class="pt-name">' + esc(a.nom) + '</h3><div class="pt-dept">' + esc(a.dept) + " · " + a.code + "</div>" +
      '<div class="pt-facts"><span>' + (pop != null ? CMap.fmt(Math.round(Math.pow(10, pop)), 0) + " hab." : "population n.d.") + "</span><span>" +
      (dens ? "Densité : " + esc(CMap.densLabels[Math.round(dens) - 1]) : "Densité n.d.") + "</span></div>" +
      '<div class="pt-pinned">' + (b ? '<span class="pt-pin" aria-hidden="true"></span>' + esc(b.nom) + ' <span class="cp-meta">' + b.code + '</span> <a href="#" id="prof-clear-cmp">retirer</a>' : "") + "</div>";
    var clr = document.getElementById("prof-clear-cmp");
    if (clr) clr.addEventListener("click", function (e) { e.preventDefault(); S.compare = null; render(S.primary); });
    drawLocator(a.code, b ? b.code : null);
  }

  /* ---- Carte de situation : PortraitKit.locator ---- */
  function drawLocator(code, codeB) { PortraitKit.locator(document.getElementById("pt-canvas"), { target: code, pin: codeB }); }
  window.addEventListener("themechange", function () { PortraitKit.resetLocator(); if (S.primary && S.row) { var a = info(S.primary); if (a) drawLocator(a.code, S.compare && S.row[S.compare] != null ? S.compare : null); } });

  function wire(inputId, listId, onPick) {
    var input = document.getElementById(inputId), list = document.getElementById(listId);
    input.addEventListener("input", function () {
      var q = input.value.trim().toLowerCase(), m = [], i;
      if (q.length < 2) { list.hidden = true; return; }
      for (i = 0; i < S.names.length && m.length < 8; i++) if (S.names[i][0].indexOf(q) === 0) m.push(S.names[i][1]);
      if (!m.length) { list.hidden = true; return; }
      list.innerHTML = m.map(function (c) { var o = info(c); return '<li role="option" data-code="' + c + '">' + esc(o.nom) + ' <span class="cp-meta">(' + c + ")</span></li>"; }).join("");
      list.hidden = false;
    });
    list.addEventListener("click", function (e) {
      var li = e.target.closest("li"); if (!li) return;
      input.value = ""; list.hidden = true; onPick(li.getAttribute("data-code"));
    });
    document.addEventListener("click", function (e) { if (e.target !== input) list.hidden = true; });
  }

  function boot() {
    var cv = document.getElementById("radar-canvas");
    if (!cv) return;
    var inputs = document.querySelectorAll("#prof-search, #prof-cmp-search");
    CMap.loading(cv, "Chargement des communes…");
    inputs.forEach(function (i) { i.disabled = true; });
    load().then(function () {
      CMap.done(cv);
      inputs.forEach(function (i) { i.disabled = false; });
      wire("prof-search", "prof-suggest", function (c) { render(c); });
      wire("prof-cmp-search", "prof-cmp-suggest", function (c) { S.compare = c; render(S.primary || c); });
      var h = (location.hash || "").replace("#", "").split(","), a = (h[0] || "").toUpperCase(), b = (h[1] || "").toUpperCase();
      if (b && S.row[b] != null) S.compare = b;
      render(a && S.row[a] != null ? a : "75056");
    }).catch(function (e) { console.error(e); cv.innerHTML = '<p class="js-loading">Données indisponibles.</p>'; });
  }
  if (document.readyState === "complete") boot(); else window.addEventListener("load", boot);
})();
