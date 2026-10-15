/* ============================================================
 * explorer.js : nuage de points interactif de la page Explorer.
 * Données : window.SCAT (colonnes d'un échantillon de communes, injecté
 * par pages/explorer.qmd) et window.scatVars = [{field, label, group}].
 * - axes bornés au 99e centile (le nombre de points hors cadre est affiché) ;
 * - la courbe n'est PAS un LOESS : c'est la moyenne de y par tranche de x
 *   (30 tranches d'effectif égal) ;
 * - droite OLS et rho de Spearman calculés sur tous les points valides.
 * ============================================================ */
(function () {
  "use strict";
  var D, V = [], esc = function (s) { return String(s).replace(/[&<>"]/g, function (c) { return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]; }); };
  function css(n) { return getComputedStyle(document.documentElement).getPropertyValue(n).trim(); }
  function fmt(v, d) { return Number(v).toLocaleString("fr-FR", { minimumFractionDigits: d, maximumFractionDigits: d }); }
  function digitsFor(span) { return span >= 50 ? 0 : (span >= 5 ? 1 : (span >= 0.5 ? 2 : 3)); }

  function niceTicks(lo, hi, n) {
    var span = hi - lo; if (!(span > 0)) return [lo];
    var raw = span / n, mag = Math.pow(10, Math.floor(Math.log10(raw))), f = raw / mag;
    var step = (f < 1.5 ? 1 : f < 3 ? 2 : f < 7 ? 5 : 10) * mag, t = [], v = Math.ceil(lo / step) * step;
    for (; v <= hi + step * 1e-9; v += step) t.push(Math.round(v / step) * step);
    return t;
  }
  function quantile(sorted, p) { return sorted[Math.min(sorted.length - 1, Math.floor(p * (sorted.length - 1)))]; }
  function avgRanks(arr) {
    var o = arr.map(function (v, i) { return [v, i]; }).sort(function (a, b) { return a[0] - b[0]; }), r = new Array(arr.length), j = 0;
    while (j < o.length) { var k = j; while (k + 1 < o.length && o[k + 1][0] === o[j][0]) k++; for (var t = j; t <= k; t++) r[o[t][1]] = (j + k) / 2 + 1; j = k + 1; }
    return r;
  }

  function buildSelectors() {
    var groups = {};
    V.forEach(function (v) { (groups[v.group] = groups[v.group] || []).push(v); });
    function build(id, def) {
      var sel = document.getElementById(id); if (!sel || sel._built) return;
      sel.innerHTML = "";
      Object.keys(groups).forEach(function (g) {
        var og = document.createElement("optgroup"); og.label = g;
        groups[g].forEach(function (v) { var o = document.createElement("option"); o.value = v.field; o.textContent = v.label; if (v.field === def) o.selected = true; og.appendChild(o); });
        sel.appendChild(og);
      });
      sel.addEventListener("change", render); sel._built = true;
    }
    build("scat-x", "elec_pct_rn"); build("scat-y", "dvf_prix_app");
    var st = document.getElementById("scat-strate"); if (st) st.addEventListener("change", render);
  }

  function render() {
    var xs = document.getElementById("scat-x"), ys = document.getElementById("scat-y"), st = document.getElementById("scat-strate"),
        cv = document.getElementById("scat-canvas"), rhoEl = document.getElementById("scat-rho");
    if (!xs || !ys || !cv) return;
    var X = D[xs.value], Y = D[ys.value], strate = st ? st.value : "Toutes", I = [], i;
    for (i = 0; i < D.strate.length; i++) {
      if (strate !== "Toutes" && D.strate[i] !== strate) continue;
      if (X[i] != null && Y[i] != null) I.push(i);
    }
    if (I.length < 10) { cv.innerHTML = '<p class="js-loading">Pas assez d’observations (' + I.length + ").</p>"; rhoEl.textContent = ""; return; }
    var vx = I.map(function (k) { return X[k]; }), vy = I.map(function (k) { return Y[k]; }), n = I.length;
    // Spearman
    var rx = avgRanks(vx), ry = avgRanks(vy), mx = (n + 1) / 2, num = 0, dx = 0, dy = 0;
    for (i = 0; i < n; i++) { num += (rx[i] - mx) * (ry[i] - mx); dx += (rx[i] - mx) * (rx[i] - mx); dy += (ry[i] - mx) * (ry[i] - mx); }
    var rho = num / Math.sqrt(dx * dy);
    // Cadre : du minimum au 99e centile
    var sx = vx.slice().sort(function (a, b) { return a - b; }), sy = vy.slice().sort(function (a, b) { return a - b; });
    var xMin = sx[0], xMax = quantile(sx, 0.99), yMin = sy[0], yMax = quantile(sy, 0.99);
    if (!(xMax > xMin)) xMax = sx[n - 1]; if (!(yMax > yMin)) yMax = sy[n - 1];
    var xt = niceTicks(xMin, xMax, 5), yt = niceTicks(yMin, yMax, 5);
    xMin = Math.min(xMin, xt[0]); xMax = Math.max(xMax, xt[xt.length - 1]); yMin = Math.min(yMin, yt[0]); yMax = Math.max(yMax, yt[yt.length - 1]);
    var w = Math.max(280, cv.clientWidth), h = Math.max(200, Math.min(480, cv.clientHeight || 300)), pad = { l: 58, r: 16, t: 12, b: 38 };
    var px = function (v) { return pad.l + (v - xMin) / (xMax - xMin) * (w - pad.l - pad.r); };
    var py = function (v) { return h - pad.b - (v - yMin) / (yMax - yMin) * (h - pad.t - pad.b); };
    var ink = css("--ink"), ink2 = css("--ink-2"), ink3 = css("--ink-3"), rule = css("--rule");
    var dX = digitsFor(xMax - xMin), dY = digitsFor(yMax - yMin), out = 0;
    var svg = '<svg width="' + w + '" height="' + h + '" role="img" aria-label="Nuage de points" xmlns="http://www.w3.org/2000/svg" style="font-family:var(--font-sans)">';
    xt.forEach(function (t) { svg += '<line x1="' + px(t).toFixed(1) + '" x2="' + px(t).toFixed(1) + '" y1="' + pad.t + '" y2="' + (h - pad.b) + '" stroke="' + rule + '" stroke-width="0.6"/><text x="' + px(t).toFixed(1) + '" y="' + (h - pad.b + 15) + '" text-anchor="middle" font-size="11" fill="' + ink2 + '">' + fmt(t, dX) + "</text>"; });
    yt.forEach(function (t) { svg += '<line y1="' + py(t).toFixed(1) + '" y2="' + py(t).toFixed(1) + '" x1="' + pad.l + '" x2="' + (w - pad.r) + '" stroke="' + rule + '" stroke-width="0.6"/><text y="' + (py(t) + 4).toFixed(1) + '" x="' + (pad.l - 6) + '" text-anchor="end" font-size="11" fill="' + ink2 + '">' + fmt(t, dY) + "</text>"; });
    var xl = V.find(function (v) { return v.field === xs.value; }).label, yl = V.find(function (v) { return v.field === ys.value; }).label;
    svg += '<text x="' + ((pad.l + w - pad.r) / 2) + '" y="' + (h - 6) + '" text-anchor="middle" font-size="12" fill="' + ink + '">' + esc(xl) + "</text>";
    svg += '<text x="13" y="' + (h / 2) + '" text-anchor="middle" font-size="12" fill="' + ink + '" transform="rotate(-90 13 ' + (h / 2) + ')">' + esc(yl) + "</text>";
    for (i = 0; i < n; i++) {
      if (vx[i] > xMax || vy[i] > yMax) { out++; continue; }
      svg += '<circle cx="' + px(vx[i]).toFixed(1) + '" cy="' + py(vy[i]).toFixed(1) + '" r="1.6" fill="' + ink2 + '" fill-opacity="0.22"/>';
    }
    // Moyenne de y par tranche de x (30 tranches d'effectif égal)
    var ord = I.map(function (k, j) { return j; }).sort(function (a, b) { return vx[a] - vx[b]; }), bins = 30, size = Math.max(5, Math.floor(n / bins)), path = "", started = false, b;
    for (b = 0; b < bins; b++) {
      var sl = ord.slice(b * size, (b + 1) * size); if (sl.length < 3) continue;
      var bx = 0, by = 0; sl.forEach(function (j) { bx += vx[j]; by += vy[j]; }); bx /= sl.length; by /= sl.length;
      if (bx > xMax || by > yMax || by < yMin) continue;
      path += (started ? " L " : "M ") + px(bx).toFixed(1) + "," + py(by).toFixed(1); started = true;
    }
    if (path) svg += '<path d="' + path + '" stroke="' + ink + '" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/>';
    // OLS
    var sxv = 0, syv = 0; for (i = 0; i < n; i++) { sxv += vx[i]; syv += vy[i]; }
    var mxv = sxv / n, myv = syv / n, sxy = 0, sxx = 0, syy = 0;
    for (i = 0; i < n; i++) { var ax = vx[i] - mxv, ay = vy[i] - myv; sxy += ax * ay; sxx += ax * ax; syy += ay * ay; }
    var r2 = null;
    if (sxx > 0 && syy > 0) {
      var slope = sxy / sxx, icpt = myv - slope * mxv; r2 = (sxy * sxy) / (sxx * syy);
      var y1 = icpt + slope * xMin, y2 = icpt + slope * xMax;
      svg += '<line x1="' + px(xMin).toFixed(1) + '" y1="' + py(Math.max(yMin, Math.min(yMax, y1))).toFixed(1) + '" x2="' + px(xMax).toFixed(1) + '" y2="' + py(Math.max(yMin, Math.min(yMax, y2))).toFixed(1) + '" stroke="' + ink3 + '" stroke-width="1.6" stroke-dasharray="5 4"/>';
    }
    var lx = w - pad.r - 190;
    svg += '<g font-size="11" fill="' + ink2 + '"><rect x="' + (lx - 6) + '" y="' + (pad.t - 2) + '" width="196" height="38" fill="' + css("--surface") + '" fill-opacity="0.85"/>' +
      '<line x1="' + lx + '" x2="' + (lx + 18) + '" y1="' + (pad.t + 8) + '" y2="' + (pad.t + 8) + '" stroke="' + ink + '" stroke-width="2"/><text x="' + (lx + 24) + '" y="' + (pad.t + 12) + '">moyenne par tranche (30 quantiles)</text>' +
      '<line x1="' + lx + '" x2="' + (lx + 18) + '" y1="' + (pad.t + 24) + '" y2="' + (pad.t + 24) + '" stroke="' + ink3 + '" stroke-width="1.6" stroke-dasharray="5 4"/><text x="' + (lx + 24) + '" y="' + (pad.t + 28) + '">droite des moindres carrés</text></g></svg>';
    cv.innerHTML = svg;
    rhoEl.textContent = "ρ de Spearman = " + fmt(rho, 2) + (r2 != null ? " · R² = " + fmt(r2, 2) : "") + " · n = " + fmt(n, 0) + (out ? " · " + fmt(out, 0) + " points au-dessus du 99e centile hors cadre" : "");
  }

  /** Appelé par un clic sur la matrice de corrélation (ggiraph onclick). */
  window.scatSet = function (x, y) {
    var xs = document.getElementById("scat-x"), ys = document.getElementById("scat-y");
    if (!xs || !ys || !D) return;
    function has(sel, v) { return Array.prototype.some.call(sel.options, function (o) { return o.value === v; }); }
    if (has(xs, x)) xs.value = x;
    if (has(ys, y)) ys.value = y;
    render();
    var card = document.getElementById("scat-canvas");
    if (card && card.scrollIntoView) card.scrollIntoView({ behavior: "smooth", block: "nearest" });
  };

  function boot() { D = window.SCAT; V = window.scatVars || []; if (!D || !document.getElementById("scat-canvas")) return; buildSelectors(); render(); }
  if (document.readyState === "complete") setTimeout(boot, 50); else window.addEventListener("load", function () { setTimeout(boot, 50); });
  window.addEventListener("themechange", function () { if (D) render(); });
  var t; window.addEventListener("resize", function () { clearTimeout(t); t = setTimeout(render, 150); });
})();
