/* ============================================================
 * portrait-kit.js : briques partagées par Profil et « Communes semblables ».
 *   PortraitKit.tf(ax, v)            transformation de l'axe (lin | log1p | cat)
 *   PortraitKit.buildDist(ax, col)   domaine P1-P99 + densité lissée (axe STABLE)
 *   PortraitKit.distSVG(d)           silhouette de la distribution nationale
 *   PortraitKit.ends(ax, d)          libellés des bornes de l'axe
 *   PortraitKit.pos(ax, d, v)        position 0-100 % d'une valeur sur l'axe
 *   PortraitKit.axVal(ax, v)         valeur exacte + unité (HTML échappé)
 *   PortraitKit.locator(el, o)       carte de situation (canvas) : o = {target, pin, peers[]}
 * ax = {tf: "lin"|"log"|"cat", log10, d, unit}. Dépend de CMap et CMapStats.
 * ============================================================ */
(function () {
  "use strict";
  var esc = function (s) { return CMap.esc(s); };
  var K = { BINS: 40 };
  window.PortraitKit = K;

  K.tf = function (ax, v) { return ax.tf === "log" ? Math.log1p(Math.max(0, v)) : v; };

  K.buildDist = function (ax, col) {
    var t = [], i, BINS = K.BINS;
    for (i = 0; i < col.length; i++) if (col[i] != null && isFinite(col[i])) t.push(K.tf(ax, col[i]));
    t.sort(function (a, b) { return a - b; });
    var d = { n: t.length, cat: ax.tf === "cat" };
    if (d.cat) { d.lo = 0.5; d.hi = 6.5; d.bins = [0, 0, 0, 0, 0, 0]; t.forEach(function (v) { d.bins[Math.max(0, Math.min(5, Math.round(v) - 1))]++; }); }
    else if (t.length > 20) {
      d.lo = CMapStats.quantile7(t, 0.01); d.hi = CMapStats.quantile7(t, 0.99);
      if (!(d.hi > d.lo)) { d.lo = t[0]; d.hi = t[t.length - 1] + 1e-9; }
      var raw = new Array(BINS).fill(0);
      t.forEach(function (v) { raw[Math.max(0, Math.min(BINS - 1, Math.floor((v - d.lo) / (d.hi - d.lo) * BINS)))]++; });
      var k = [1, 2, 3, 2, 1];
      d.bins = raw.map(function (_, j) {
        var acc = 0, w = 0; k.forEach(function (kw, o) { var q = j + o - 2; if (q >= 0 && q < BINS) { acc += kw * raw[q]; w += kw; } }); return acc / w; });
    }
    return d;
  };

  K.axVal = function (ax, v) {
    if (v == null) return "n.d.";
    if (ax.tf === "cat") return esc(CMap.densLabels[Math.round(v) - 1] || "n.d.");
    var x = ax.log10 ? Math.pow(10, v) : v;
    return CMap.fmt(x, ax.d == null ? 0 : ax.d) + (ax.unit ? " " + esc(ax.unit) : "");
  };

  K.pos = function (ax, d, v) {
    if (v == null || !d || d.lo == null) return null;
    return Math.max(0, Math.min(100, (K.tf(ax, v) - d.lo) / (d.hi - d.lo) * 100));
  };

  K.distSVG = function (d) {
    if (!d || !d.bins) return '<svg class="pt-svg" viewBox="0 0 100 30" preserveAspectRatio="none"></svg>';
    var mx = Math.max.apply(null, d.bins) || 1, out = "", n = d.bins.length, i;
    if (d.cat) {
      for (i = 0; i < n; i++) { var h = d.bins[i] / mx * 26, x0 = (i + 0.12) / n * 100; out += '<rect class="pt-bar" x="' + x0.toFixed(2) + '" y="' + (29 - h).toFixed(1) + '" width="' + (100 / n * 0.76).toFixed(2) + '" height="' + h.toFixed(1) + '"/>'; }
    } else {
      var path = "M0,29";
      for (i = 0; i < n; i++) path += " L" + ((i + 0.5) / n * 100).toFixed(2) + "," + (29 - d.bins[i] / mx * 26).toFixed(2);
      out = '<path class="pt-area" d="' + path + ' L100,29 Z"/>';
    }
    return '<svg class="pt-svg" viewBox="0 0 100 30" preserveAspectRatio="none" aria-hidden="true">' + out + '<line class="pt-base" x1="0" x2="100" y1="29.5" y2="29.5"/></svg>';
  };

  K.ends = function (ax, d) {
    if (!d || d.lo == null || d.cat) return ["", ""];
    var inv = function (t) { return ax.tf === "log" ? Math.expm1(t) : t; };
    var f = function (t) { var x = inv(t); if (ax.log10) x = Math.pow(10, x); return CMap.fmt(x, Math.abs(x) >= 100 ? 0 : (ax.d == null ? 0 : Math.min(ax.d, 1))); };
    return [f(d.lo), f(d.hi) + (ax.tf === "log" ? " (éch. log)" : "")];
  };

  /* ---- Carte de situation (canvas, géométrie partagée) ---- */
  var LOC = { base: {}, W: 176 };
  var ARRS = { "75056": 1, "69123": 1, "13055": 1 };
  function codesOf(code) {
    if (!ARRS[code]) return [code];
    var out = [], i;
    if (code === "75056") for (i = 1; i <= 20; i++) out.push("751" + (i < 10 ? "0" : "") + i);
    if (code === "69123") for (i = 81; i <= 89; i++) out.push("693" + i);
    if (code === "13055") for (i = 1; i <= 16; i++) out.push("132" + (i < 10 ? "0" : "") + i);
    return out;
  }
  function terr(code) { var t = code.slice(0, 3); return /^97[1-6]$/.test(t) ? t : "FR"; }
  function polysOf(g, gi) {
    var x = g.latlngs[gi]; if (!x || !x.length) return [];
    return (typeof x[0][0][0] === "number") ? [x] : x;
  }
  function locProject(bb, W, H) {
    var k = Math.cos((bb[0] + bb[2]) / 2 * Math.PI / 180), w = (bb[3] - bb[1]) * k, h = bb[2] - bb[0], sc = Math.min((W - 8) / w, (H - 8) / h);
    var ox = (W - w * sc) / 2, oy = (H - h * sc) / 2;
    return function (lat, lng) { return [ox + (lng - bb[1]) * k * sc, H - oy - (lat - bb[0]) * sc]; };
  }
  function locBase(g, t) {
    if (LOC.base[t]) return LOC.base[t];
    var W = LOC.W, H = LOC.W, dpr = window.devicePixelRatio || 1, cv = document.createElement("canvas");
    cv.width = W * dpr; cv.height = H * dpr;
    var ctx = cv.getContext("2d"); ctx.scale(dpr, dpr);
    var sel = [], bb = [90, 180, -90, -180], i, c;
    for (i = 0; i < g.codes.length; i++) {
      c = g.codes[i];
      if (t === "FR" ? /^9[78]/.test(c) : c.slice(0, 3) !== t) continue;
      sel.push(i);
      if (t !== "FR") polysOf(g, i).forEach(function (rings) { rings[0].forEach(function (q) { if (q[0] < bb[0]) bb[0] = q[0]; if (q[0] > bb[2]) bb[2] = q[0]; if (q[1] < bb[1]) bb[1] = q[1]; if (q[1] > bb[3]) bb[3] = q[1]; }); });
    }
    if (t === "FR") bb = [41.2, -5.4, 51.2, 9.8];
    var P = locProject(bb, W, H);
    ctx.fillStyle = "#CDCDC6"; ctx.strokeStyle = "#CDCDC6"; ctx.lineWidth = 0.3;   // plaque toujours claire (cf. --plate)
    sel.forEach(function (gi) {
      polysOf(g, gi).forEach(function (rings) {
        ctx.beginPath();
        rings.forEach(function (ring) { ring.forEach(function (q, j) { var p = P(q[0], q[1]); if (j) ctx.lineTo(p[0], p[1]); else ctx.moveTo(p[0], p[1]); }); ctx.closePath(); });
        ctx.fill("evenodd"); ctx.stroke();
      });
    });
    return (LOC.base[t] = { cv: cv, P: P, dpr: dpr });
  }
  /** o = {target, pin, peers[]} (codes INSEE). Les semblables sont des points ; cible = disque cerclé ; épinglée = carré en tirets. */
  K.locator = function (el, o) {
    if (!el) return Promise.resolve();
    return CMap.geo().then(function (g) {
      var t = terr(o.target), base = locBase(g, t), W = LOC.W, dpr = base.dpr;
      el.width = W * dpr; el.height = W * dpr; el.style.width = W + "px"; el.style.height = W + "px";
      var ctx = el.getContext("2d"); ctx.scale(dpr, dpr);
      ctx.drawImage(base.cv, 0, 0, W, W);
      function mark(cd, kind) {
        var cs = codesOf(cd), cx = 0, cy = 0, n = 0;
        cs.forEach(function (c) {
          var gi = g.idx[c]; if (gi == null) return;
          polysOf(g, gi).forEach(function (rings) {
            ctx.beginPath();
            rings.forEach(function (ring) { ring.forEach(function (q, j) { var p = base.P(q[0], q[1]); if (j) ctx.lineTo(p[0], p[1]); else ctx.moveTo(p[0], p[1]); cx += p[0]; cy += p[1]; n++; }); ctx.closePath(); });
            ctx.fillStyle = "#16181B"; if (kind === "a") ctx.fill("evenodd");
          });
        });
        if (!n) return;
        cx /= n; cy /= n;
        ctx.lineWidth = 1.6; ctx.strokeStyle = "#16181B";
        if (kind === "a") { ctx.beginPath(); ctx.arc(cx, cy, 8, 0, 6.2832); ctx.stroke(); }
        else if (kind === "p") { ctx.beginPath(); ctx.arc(cx, cy, 2.6, 0, 6.2832); ctx.fillStyle = "#45494F"; ctx.fill(); }
        else { ctx.setLineDash([3, 2]); ctx.strokeStyle = "#45494F"; ctx.strokeRect(cx - 7, cy - 7, 14, 14); ctx.setLineDash([]); }
      }
      (o.peers || []).forEach(function (c) { if (terr(c) === t) mark(c, "p"); });
      if (o.pin && terr(o.pin) === t) mark(o.pin, "b");
      mark(o.target, "a");
    }).catch(function (e) { console.error(e); });
  };
  K.resetLocator = function () { LOC.base = {}; };
})();
