/* ============================================================
 * constellation.js : moteur « points de communes » de pages/constellation.qmd.
 *
 * Un seul ensemble de points (35 k communes + 101 départements) et quatre
 * dispositions, avec transitions de ≤ 450 ms (instantané si prefers-reduced-motion) :
 *   geo      position géographique (Web Mercator, encarts outre-mer)
 *   pop      cercles de Dorling, surface ∝ population
 *   scatter  nuage de deux indicateurs (axes fixes P1-P99, Spearman, n)
 *   rank     classement : rang centile en abscisse, valeur en ordonnée
 * La couleur = un indicateur, rampe et seuils de cmap-stats.js, identiques partout.
 * Brosse (histogramme sous la carte) : les points hors plage s'estompent.
 * État dans l'URL : #CODE&a=scatter&c=...&x=...&y=...&l=com&d=69&n=3&b=lo,hi&v=...
 * Configuration injectée par pages/constellation.qmd : window.CONSTELL
 *   {ind:[{var,label,group,unit,dig,src}], pal:{green,red,blue,ochre}, groupPal:{groupe:rampe}}
 * Pas de d3 : échelles, interpolation et brosse tiennent en ~100 lignes (voir docs/decisions.qmd).
 * ============================================================ */
// cmap.js est inclus après ce script (after-body) : le moteur démarre à DOMContentLoaded.
(function () {
  "use strict";
  if (window.CMap) start(); else document.addEventListener("DOMContentLoaded", start);
  function start() {
  var CFG = window.CONSTELL, Core = window.ConstCore, Stats = window.CMapStats;
  if (!CFG || !Core || !Stats || !window.CMap) return;
  var $ = function (id) { return document.getElementById(id); };
  var esc = CMap.esc, fmt = CMap.fmt;
  var MET = {}; CFG.ind.forEach(function (m) { MET[m.var] = m; });
  var REDUCED = window.matchMedia && matchMedia("(prefers-reduced-motion: reduce)").matches;
  var DUR = 450;
  var DEF = { a: "geo", c: "revenu_median", x: "revenu_median", y: "apl_medecins", v: "apl_medecins", l: "dept", d: "", n: 0 };
  var ARR_LABEL = { geo: "carte", pop: "population", scatter: "nuage", rank: "classement" };

  /* ---------- État ---------- */
  var S = { a: DEF.a, c: DEF.c, x: DEF.x, y: DEF.y, v: DEF.v, l: DEF.l, d: DEF.d, n: DEF.n, code: null, brush: null };
  var D = { idx: null, geo: null, dor: null, pop: null, dens: null, nd: 0, deptRow: null };
  var COLS = {}, DOMS = {};
  var cv, ctx, W = 0, H = 0, dpr = 1, theme = {};
  var CL = null, DL = null;            // couches communes / départements
  var Q = { breaks: null, pal: null, scale: "quantile", agg: null, cvals: null, ranks: null };
  var anim = { on: false, t0: 0, dur: 0 }, TW = { com: [0, 0, 0], dep: [1, 1, 1], frame: [0, 0, 0], axes: [0, 0, 0] };
  var hover = -1, hoverKind = "";
  var selDot = { x: 0, y: 0, tx: 0, ty: 0, fx: 0, fy: 0, on: false };

  /* ---------- Utilitaires ---------- */
  function ease(t) { return t < 0.5 ? 4 * t * t * t : 1 - Math.pow(-2 * t + 2, 3) / 2; }
  function fold(s) { return String(s).normalize("NFD").replace(/[̀-ͯ]/g, "").toLowerCase(); }
  function css(el, name, fb) { var v = getComputedStyle(el).getPropertyValue(name).trim(); return v || fb; }
  function pow10(v) { return /^ctx_log_/.test(v); }
  function col(v) {
    if (COLS[v]) return Promise.resolve(COLS[v]);
    return CMap.data.col(v).then(function (a) {
      var f = new Float64Array(a.length);
      for (var i = 0; i < a.length; i++) f[i] = a[i] == null ? NaN : a[i];
      return (COLS[v] = f);
    });
  }
  function getJSON(u) { return fetch(CMap.base + u).then(function (r) { if (!r.ok) throw new Error(u + " : " + r.status); return r.json(); }); }
  function dom(v) {
    if (!DOMS[v]) DOMS[v] = Core.domain(COLS[v], pow10(v) ? false : null);
    return DOMS[v];
  }
  function unitOf(v) { return pow10(v) ? "hab." : (MET[v].unit || ""); }
  function labelOf(v) { return pow10(v) ? "Population" : MET[v].label; }
  /** Valeur affichée (population : 10^v) avec unité. */
  function show(v, val, noUnit) {
    if (val == null || !isFinite(val)) return "—";
    var m = MET[v], x = pow10(v) ? Math.round(Math.pow(10, val)) : val, d = pow10(v) ? 0 : m.dig;
    return fmt(x, d) + (noUnit || !unitOf(v) ? "" : " " + unitOf(v));
  }
  function niceTicks(lo, hi, log) {
    var out = [], k;
    if (log) {
      for (k = Math.floor(Math.log10(lo)); k <= Math.ceil(Math.log10(hi)); k++)
        [1, 2, 5].forEach(function (m) { var t = m * Math.pow(10, k); if (t >= lo && t <= hi) out.push(t); });
      return out;
    }
    var span = hi - lo, step = Math.pow(10, Math.floor(Math.log10(span / 5))), r = span / 5 / step;
    step *= r < 1.5 ? 1 : r < 3 ? 2 : r < 7 ? 5 : 10;
    for (var t = Math.ceil(lo / step) * step; t <= hi + 1e-9; t += step) out.push(+t.toFixed(10));
    return out;
  }
  function tickLabel(v, t, step) {
    if (pow10(v)) { var p = Math.pow(10, t); return p >= 1e6 ? fmt(p / 1e6, 0) + " M" : p >= 1e3 ? fmt(p / 1e3, 0) + " k" : fmt(p, 0); }
    var a = Math.abs(t);
    return fmt(t, a >= 100 ? 0 : a >= 10 ? 1 : a >= 1 ? 1 : 2).replace(/,0+$/, "");
  }
  function scaleOf(v) {                // valeur -> 0..1 (borné)
    var d = dom(v), la = Math.log(d.lo), lb = Math.log(d.hi);
    return function (val) {
      var t = d.log ? (Math.log(Math.max(val, d.lo * 1e-6)) - la) / (lb - la) : (val - d.lo) / (d.hi - d.lo);
      return t < 0 ? 0 : t > 1 ? 1 : t;
    };
  }

  /* ---------- Couches de points ---------- */
  function mkLayer(n) {
    return { n: n, x: new Float32Array(n), y: new Float32Array(n), r: new Float32Array(n),
             fx: new Float32Array(n), fy: new Float32Array(n), fr: new Float32Array(n),
             tx: new Float32Array(n), ty: new Float32Array(n), tr: new Float32Array(n),
             cls: new Uint8Array(n), hid: new Uint8Array(n), nan: new Uint8Array(n), dim: new Uint8Array(n), ok: new Uint8Array(n),
             list: null, bounds: null };
  }
  function rebuildBuckets(L) {          // tri par seau (classe x estompé) des points à dessiner
    var cnt = new Int32Array(13), i, b;
    for (i = 0; i < L.n; i++) if (L.ok[i] && !L.hid[i] && !L.nan[i]) cnt[(L.cls[i] + 6 * L.dim[i]) + 1]++;
    for (b = 1; b <= 12; b++) cnt[b] += cnt[b - 1];
    L.bounds = cnt.slice(); var pos = cnt.slice(0, 12), out = new Int32Array(cnt[12]);
    for (i = 0; i < L.n; i++) if (L.ok[i] && !L.hid[i] && !L.nan[i]) out[pos[L.cls[i] + 6 * L.dim[i]]++] = i;
    L.list = out;
  }

  /* ---------- Couleurs, seuils ---------- */
  function palette() {
    var g = MET[S.c].group, key = CFG.groupPal[g] || "green";
    return CFG.pal[key];
  }
  function recolorData() {
    var cvals = COLS[S.c], d = dom(S.c);
    var br = Stats.makeBreaks(Array.prototype.slice.call(cvals), d.log ? "log_quantile" : "quantile");
    Q.breaks = br; Q.pal = palette(); Q.cvals = cvals;
    Q.agg = Core.deptAggregate(cvals, D.pop, D.idx.d, D.nd);
    Q.ranks = Core.ranks(cvals);
    if (S.a === "scatter") { Q.xAgg = Core.deptAggregate(COLS[S.x], D.pop, D.idx.d, D.nd); Q.yAgg = Core.deptAggregate(COLS[S.y], D.pop, D.idx.d, D.nd); }
    var dr = Core.ranks(Q.agg.v);
    Q.dRanks = dr;
    var i;
    for (i = 0; i < CL.n; i++) CL.cls[i] = isFinite(cvals[i]) ? Stats.classOf(cvals[i], br.breaks) : 0;
    for (i = 0; i < DL.n; i++) DL.cls[i] = isFinite(Q.agg.v[i]) ? Stats.classOf(Q.agg.v[i], br.breaks) : 0;
  }

  /* ---------- Filtres et brosse ---------- */
  function applyFilter() {
    var i, d = S.d === "" ? -1 : D.deptRow[S.d], dn = +S.n;
    for (i = 0; i < CL.n; i++) {
      var okk = CL.ok[i] ? 1 : 0;
      if (okk && d >= 0 && D.idx.d[i] !== d) okk = 0;
      if (okk && dn > 0 && D.dens && D.dens[i] !== dn) okk = 0;
      CL.hid[i] = okk ? 0 : 1;
    }
    applyBrush();
  }
  var matchCom = null;
  function applyBrush() {
    var i;
    if (S.brush && COLS[S.c]) {
      matchCom = Core.inRange(COLS[S.c], S.brush[0], S.brush[1]);
      for (i = 0; i < CL.n; i++) CL.dim[i] = matchCom[i] ? 0 : 1;
      for (i = 0; i < DL.n; i++) { var v = Q.agg && Q.agg.v[i]; DL.dim[i] = v >= S.brush[0] && v <= S.brush[1] ? 0 : 1; }
    } else {
      matchCom = null; CL.dim.fill(0); DL.dim.fill(0);
    }
    rebuildBuckets(CL); rebuildBuckets(DL);
  }
  function nVisible() { var n = 0, i; for (i = 0; i < CL.n; i++) if (CL.ok[i] && !CL.hid[i]) n++; return n; }
  function baseR(nv) { return nv > 20000 ? 0.95 : nv > 3000 ? 1.4 : nv > 600 ? 2.2 : 3.4; }

  /* ---------- Cibles de disposition (px) ---------- */
  function fit() {
    var pad = 6, f = Math.min((W - 2 * pad) / D.geo.w, (H - 2 * pad) / D.geo.h);
    return { f: f, ox: (W - D.geo.w * f) / 2, oy: (H - D.geo.h * f) / 2 };
  }
  var P = { l: 58, r: 16, t: 14, b: 40 };
  function plot() { return { l: P.l, t: P.t, w: Math.max(40, W - P.l - P.r), h: Math.max(40, H - P.t - P.b) }; }

  function targets(arr) {
    var Fm = fit(), Pl = plot(), i, n = CL.n, nv = nVisible(), rc = baseR(nv), G = D.geo, popMax = 0;
    for (i = 0; i < DL.n; i++) popMax = Math.max(popMax, Q.agg.pop[i]);
    var sx, sy, xv = COLS[S.x], yv = COLS[S.y], cv_ = COLS[S.c];
    if (arr === "scatter") { sx = scaleOf(S.x); sy = scaleOf(S.y); }
    if (arr === "rank") sy = scaleOf(S.c);
    for (i = 0; i < n; i++) {
      var hid = 0, x = 0, y = 0, r = rc;
      if (!CL.ok[i]) { CL.tx[i] = CL.x[i]; CL.ty[i] = CL.y[i]; CL.tr[i] = rc; continue; }
      if (arr === "geo") { x = Fm.ox + G.gx[i] * Fm.f; y = Fm.oy + G.gy[i] * Fm.f; }
      else if (arr === "pop") {
        x = Fm.ox + D.dor.dx[i] * Fm.f; y = Fm.oy + D.dor.dy[i] * Fm.f;
        r = Math.max(Math.max(G.k * Math.sqrt(D.pop[i] > 0 ? D.pop[i] : 0), G.rmin) * Fm.f, 0.8);
      } else if (arr === "scatter") {
        if (!isFinite(xv[i]) || !isFinite(yv[i])) hid = 1;
        else { x = Pl.l + sx(xv[i]) * Pl.w; y = Pl.t + (1 - sy(yv[i])) * Pl.h; }
      } else {
        if (!isFinite(cv_[i])) hid = 1;
        else { x = Pl.l + Q.ranks[i] * Pl.w; y = Pl.t + (1 - sy(cv_[i])) * Pl.h; }
      }
      CL.tx[i] = hid ? CL.x[i] : x; CL.ty[i] = hid ? CL.y[i] : y; CL.tr[i] = r;
      CL.nan[i] = hid;
    }
    // départements
    for (i = 0; i < DL.n; i++) {
      var dh = 0, dxx = 0, dyy = 0, dr = 3 + 7 * Math.sqrt(Q.agg.pop[i] / (popMax || 1));
      if (!DL.ok[i]) { DL.tx[i] = DL.x[i]; DL.ty[i] = DL.y[i]; DL.tr[i] = dr; continue; }
      if (arr === "geo") { dxx = Fm.ox + G.dep.gx[i] * Fm.f; dyy = Fm.oy + G.dep.gy[i] * Fm.f; }
      else if (arr === "pop") { dxx = Fm.ox + G.dep.dx[i] * Fm.f; dyy = Fm.oy + G.dep.dy[i] * Fm.f; dr = Math.max(G.dep.r[i] * Fm.f, 2.5); }
      else if (arr === "scatter") {
        var ax = Q.xAgg.v[i], ay = Q.yAgg.v[i];
        if (!isFinite(ax) || !isFinite(ay)) dh = 1; else { dxx = Pl.l + sx(ax) * Pl.w; dyy = Pl.t + (1 - sy(ay)) * Pl.h; }
      } else {
        var av = Q.agg.v[i];
        if (!isFinite(av)) dh = 1; else { dxx = Pl.l + Q.dRanks[i] * Pl.w; dyy = Pl.t + (1 - sy(av)) * Pl.h; }
      }
      DL.tx[i] = dh ? DL.x[i] : dxx; DL.ty[i] = dh ? DL.y[i] : dyy; DL.tr[i] = dr; DL.nan[i] = dh;
    }
    selTarget(arr, Fm, Pl, sx, sy);
  }
  function rowOf(code) { return D.idx.c.indexOf(code); }
  function selTarget(arr, Fm, Pl, sx, sy) {
    var i = S.code ? rowOf(S.code) : -1;
    selDot.on = i >= 0 && CL.ok[i] === 1;
    if (!selDot.on) return;
    var x, y;
    if (arr === "geo") { x = Fm.ox + D.geo.gx[i] * Fm.f; y = Fm.oy + D.geo.gy[i] * Fm.f; }
    else if (arr === "pop") { x = Fm.ox + D.dor.dx[i] * Fm.f; y = Fm.oy + D.dor.dy[i] * Fm.f; }
    else if (arr === "scatter") {
      if (!isFinite(COLS[S.x][i]) || !isFinite(COLS[S.y][i])) { selDot.on = false; return; }
      x = Pl.l + sx(COLS[S.x][i]) * Pl.w; y = Pl.t + (1 - sy(COLS[S.y][i])) * Pl.h;
    } else {
      if (!isFinite(COLS[S.c][i])) { selDot.on = false; return; }
      x = Pl.l + Q.ranks[i] * Pl.w; y = Pl.t + (1 - sy(COLS[S.c][i])) * Pl.h;
    }
    selDot.tx = x; selDot.ty = y;
    if (!selDot.seen) { selDot.x = x; selDot.y = y; }
    selDot.seen = true;
  }

  /* ---------- Animation ---------- */
  var raf = 0;
  function snapshot() {
    [CL, DL].forEach(function (L) { L.fx.set(L.x); L.fy.set(L.y); L.fr.set(L.r); });
    selDot.fx = selDot.x; selDot.fy = selDot.y;
    for (var k in TW) TW[k][1] = TW[k][0];       // [courant, départ, cible] : départ = courant
  }
  function setTW(k, to) { TW[k][2] = to; }
  function run(dur) {
    // TW[k] = [valeur courante, valeur de départ, valeur cible]
    anim.t0 = performance.now(); anim.dur = REDUCED ? 0 : dur; anim.on = true;
    cancelAnimationFrame(raf);
    if (anim.dur === 0) { step(anim.t0 + 1); return; }
    raf = requestAnimationFrame(step);
  }
  function step(now) {
    var p = anim.dur ? Math.min(1, (now - anim.t0) / anim.dur) : 1, e = ease(p), i, L, a, b;
    [CL, DL].forEach(function (L) {
      var n = L.n;
      for (i = 0; i < n; i++) {
        L.x[i] = L.fx[i] + (L.tx[i] - L.fx[i]) * e; L.y[i] = L.fy[i] + (L.ty[i] - L.fy[i]) * e; L.r[i] = L.fr[i] + (L.tr[i] - L.fr[i]) * e;
      }
    });
    selDot.x = selDot.fx + (selDot.tx - selDot.fx) * e; selDot.y = selDot.fy + (selDot.ty - selDot.fy) * e;
    for (var k in TW) TW[k][0] = TW[k][1] + (TW[k][2] - TW[k][1]) * p;
    draw();
    if (p < 1) raf = requestAnimationFrame(step); else anim.on = false;
  }

  /* ---------- Dessin ---------- */
  function readTheme() {
    var el = cv.parentNode;
    theme = { ink: css(el, "--cs-ink", "#16181B"), ink3: css(el, "--cs-ink-3", "#62666D"), rule: css(el, "--cs-rule", "#DEDED8"),
              na: css(el, "--na-line", "#7C8188"), plate: css(el, "--plate", "#FFFFFF"), font: css(document.body, "font-family", "sans-serif") };
  }
  function drawLayer(L, alpha, a0) {
    if (!L.list || alpha <= 0.01) return;
    var b, j, i, K = Q.pal.length, isCom = L === CL;
    for (b = 0; b < 12; b++) {
      var s = L.bounds[b], e = L.bounds[b + 1]; if (s === e) continue;
      var c = b % 6, dim = b >= 6;
      ctx.globalAlpha = alpha * (dim ? 0.1 : (a0 || 1));
      ctx.fillStyle = c === 0 ? theme.na : Q.pal[c - 1];
      ctx.strokeStyle = theme.ink; ctx.lineWidth = isCom ? 0.35 : 0.8;
      ctx.beginPath();
      for (j = s; j < e; j++) {
        i = L.list[j];
        var r = L.r[i], x = L.x[i], y = L.y[i];
        if (r < 1.25 && isCom) ctx.rect(x - r, y - r, 2 * r, 2 * r);
        else { ctx.moveTo(x + r, y); ctx.arc(x, y, r, 0, 6.2832); }
      }
      ctx.fill();
      if (!dim && (!isCom || c <= 2) && e - s < 9000) { ctx.globalAlpha = alpha * 0.28; ctx.stroke(); }
    }
    ctx.globalAlpha = 1;
  }
  function text(t, x, y, o) {
    o = o || {};
    ctx.font = (o.bold ? "600 " : "") + (o.size || 11) + "px " + theme.font;
    ctx.textAlign = o.align || "left"; ctx.textBaseline = o.base || "alphabetic";
    if (o.halo) { ctx.lineWidth = 3.5; ctx.strokeStyle = theme.plate; ctx.lineJoin = "round"; ctx.strokeText(t, x, y); }
    ctx.fillStyle = o.color || theme.ink; ctx.fillText(t, x, y);
  }
  function drawBoxes(alpha) {
    if (alpha <= 0.01) return;
    var Fm = fit(), G = D.geo;
    ctx.globalAlpha = alpha; ctx.lineWidth = 1; ctx.strokeStyle = theme.rule;
    G.boxes.forEach(function (b) {
      var x = Fm.ox + b.x * Fm.f, y = Fm.oy + b.y * Fm.f, w = b.w * Fm.f, h = b.h * Fm.f;
      ctx.strokeRect(x + 0.5, y + 0.5, w, h);
      var fs = 10.5; ctx.font = "600 " + fs + "px " + theme.font;
      while (fs > 7 && ctx.measureText(b.name).width > w - 8) { fs -= 0.5; ctx.font = "600 " + fs + "px " + theme.font; }
      text(b.name, x + 4, y + 11, { size: fs, color: theme.ink3, bold: true });
    });
    text("Outre-mer : encarts à échelle propre à chacun", Fm.ox + 14 * Fm.f, Fm.oy + (G.boxes[0].y - 8) * Fm.f, { size: 10, color: theme.ink3 });
    ctx.globalAlpha = 1;
  }
  function axisInfo() {
    var xv = S.a === "scatter" ? S.x : null, yv = S.a === "scatter" ? S.y : S.c;
    return { xv: xv, yv: yv };
  }
  function drawAxes(alpha) {
    if (alpha <= 0.01) return;
    var Pl = plot(), ai = axisInfo(), t;
    ctx.globalAlpha = alpha; ctx.lineWidth = 1; ctx.strokeStyle = theme.rule;
    function yAxis(v) {
      var d = dom(v), sc = scaleOf(v);
      niceTicks(d.lo, d.hi, d.log).forEach(function (tk) {
        var y = Pl.t + (1 - sc(tk)) * Pl.h;
        ctx.beginPath(); ctx.moveTo(Pl.l, y + 0.5); ctx.lineTo(Pl.l + Pl.w, y + 0.5); ctx.stroke();
        text(tickLabel(v, tk), Pl.l - 6, y + 3.5, { align: "right", color: theme.ink3, size: 10.5 });
      });
      text(labelOf(v) + (unitOf(v) ? " (" + unitOf(v) + ")" : ""), 4, 11, { size: 10.5, color: theme.ink, bold: true });
    }
    function xAxis(v) {
      var d = dom(v), sc = scaleOf(v);
      niceTicks(d.lo, d.hi, d.log).forEach(function (tk) {
        var x = Pl.l + sc(tk) * Pl.w;
        ctx.beginPath(); ctx.moveTo(x + 0.5, Pl.t); ctx.lineTo(x + 0.5, Pl.t + Pl.h); ctx.stroke();
        text(tickLabel(v, tk), x, Pl.t + Pl.h + 14, { align: "center", color: theme.ink3, size: 10.5 });
      });
      text(labelOf(v) + (unitOf(v) ? " (" + unitOf(v) + ")" : "") + (d.log ? " · échelle logarithmique" : ""), Pl.l + Pl.w, Pl.t + Pl.h + 31, { align: "right", size: 10.5, bold: true });
    }
    yAxis(ai.yv);
    if (S.a === "scatter") xAxis(ai.xv);
    else {
      [0, 25, 50, 75, 100].forEach(function (p) {
        var x = Pl.l + p / 100 * Pl.w;
        ctx.beginPath(); ctx.moveTo(x + 0.5, Pl.t); ctx.lineTo(x + 0.5, Pl.t + Pl.h); ctx.stroke();
        text(p + "", x, Pl.t + Pl.h + 14, { align: "center", color: theme.ink3, size: 10.5 });
      });
      text("Rang centile de " + (S.l === "dept" ? "la médiane départementale" : "la commune") + " (0 = plus bas)", Pl.l + Pl.w, Pl.t + Pl.h + 31, { align: "right", size: 10.5, bold: true });
    }
    ctx.globalAlpha = 1;
  }
  function ring(x, y, r, w, col_) { ctx.beginPath(); ctx.arc(x, y, r, 0, 6.2832); ctx.lineWidth = w; ctx.strokeStyle = col_; ctx.stroke(); }
  function draw() {
    if (!ctx || !CL) return;
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.clearRect(0, 0, W, H);
    drawBoxes(TW.frame[0]); drawAxes(TW.axes[0]);
    var al = S.a === "scatter" ? 0.55 : 0.9;
    drawLayer(CL, TW.com[0], al);
    drawLayer(DL, TW.dep[0], 0.92);
    if (selDot.on && selDot.seen) {
      var i = rowOf(S.code), nm = D.idx.n[i];
      ring(selDot.x, selDot.y, 7, 3.5, theme.plate); ring(selDot.x, selDot.y, 7, 1.8, theme.ink);
      var right = selDot.x < W * 0.62;
      text(nm, selDot.x + (right ? 11 : -11), selDot.y - 9, { bold: true, size: 12, halo: true, align: right ? "left" : "right" });
    }
    if (hover >= 0) {
      var L = hoverKind === "d" ? DL : CL;
      ring(L.x[hover], L.y[hover], L.r[hover] + 3, 1.6, theme.ink);
    }
  }

  /* ---------- Application d'un changement d'état ---------- */
  function ensureCols() {
    var need = [S.c, "ctx_pop_latest"];
    if (S.a === "scatter") need.push(S.x, S.y);
    if (S.v) need.push(S.v);
    if (+S.n > 0) need.push("densite_rang");
    var jobs = need.filter(function (v, i) { return need.indexOf(v) === i; }).map(col);
    if (S.a === "pop" && !D.dor) jobs.push(getJSON("data/constellation-dor.json").then(function (j) { D.dor = j; }));
    return Promise.all(jobs).then(function () { D.pop = COLS.ctx_pop_latest; if (+S.n > 0) D.dens = COLS.densite_rang; });
  }
  var token = 0;
  /** Applique S à l'écran. what : "all" (recalcul complet) ou {geometry} ; dur : durée de la transition. */
  function apply(opts) {
    opts = opts || {};
    var tk = ++token;
    return ensureCols().then(function () {
      if (tk !== token) return;
      recolorData();
      snapshot();
      var revealFrom = opts.reveal;
      applyFilter();
      targets(S.a); rebuildBuckets(CL); rebuildBuckets(DL);
      // révélation : les communes naissent sur le point de leur département
      if (revealFrom) {
        for (var i = 0; i < CL.n; i++) if (CL.ok[i]) { var d = D.idx.d[i]; if (DL.ok[d]) { CL.fx[i] = DL.x[d]; CL.fy[i] = DL.y[d]; CL.fr[i] = 1; CL.x[i] = CL.fx[i]; CL.y[i] = CL.fy[i]; } }
      }
      setTW("com", S.l === "com" ? 1 : 0); setTW("dep", S.l === "dept" ? 1 : 0);
      setTW("frame", S.a === "geo" || S.a === "pop" ? 1 : 0); setTW("axes", S.a === "scatter" || S.a === "rank" ? 1 : 0);
      readTheme();
      run(opts.dur == null ? DUR : opts.dur);
      refreshUI();
    }).catch(function (e) { console.error(e); setStatus("Erreur de chargement : " + e.message); });
  }

  /* ---------- Hachage d'URL ---------- */
  function readHash() {
    var h = (location.hash || "").replace(/^#/, ""), parts = h.split("&");
    if (parts[0] && /^((?:2[ab]|\d{2})\d{3})$/i.test(parts[0])) S.code = parts[0].toUpperCase();
    parts.forEach(function (p) {
      var kv = p.split("="); if (kv.length !== 2) return;
      var k = kv[0], v = decodeURIComponent(kv[1]);
      if (k === "a" && ARR_LABEL[v]) S.a = v;
      else if ((k === "c" || k === "x" || k === "y" || k === "v") && MET[v]) S[k] = v;
      else if (k === "l" && (v === "dept" || v === "com")) S.l = v;
      else if (k === "d") S.d = v;
      else if (k === "n" && /^[0-6]$/.test(v)) S.n = +v;
      else if (k === "b") { var b = v.split(",").map(function (t) { return t === "-inf" ? -Infinity : t === "inf" ? Infinity : +t; });
        if (b.length === 2 && !isNaN(b[0]) && !isNaN(b[1])) S.brush = b; }
    });
    if (S.d !== "" || +S.n > 0) S.l = "com";
  }
  var hashT = 0;
  function writeHash() {
    clearTimeout(hashT);
    hashT = setTimeout(function () {
      var q = [];
      if (S.a !== DEF.a) q.push("a=" + S.a);
      if (S.c !== DEF.c) q.push("c=" + S.c);
      if (S.x !== DEF.x) q.push("x=" + S.x);
      if (S.y !== DEF.y) q.push("y=" + S.y);
      if (S.v !== DEF.v) q.push("v=" + S.v);
      if (S.l !== DEF.l) q.push("l=" + S.l);
      if (S.d) q.push("d=" + S.d);
      if (+S.n) q.push("n=" + S.n);
      if (S.brush) q.push("b=" + S.brush.map(function (t) { return t === -Infinity ? "-inf" : t === Infinity ? "inf" : +t.toPrecision(6); }).join(","));
      var h = [S.code || ""].concat(q).filter(function (t, i) { return i === 0 ? !!t : true; }).join("&");
      try { history.replaceState(null, "", location.pathname + location.search + (h ? "#" + h : "")); } catch (e) { /* ignore */ }
    }, 150);
  }

  /* ---------- Interface ---------- */
  function setStatus(t) { var el = $("cs-status"); if (el) el.textContent = t; }
  function fillSelect(el, withEmpty) {
    var groups = {}, order = [];
    CFG.ind.forEach(function (m) { if (!groups[m.group]) { groups[m.group] = []; order.push(m.group); } groups[m.group].push(m); });
    el.innerHTML = order.map(function (g) {
      return '<optgroup label="' + esc(g) + '">' + groups[g].map(function (m) {
        return '<option value="' + m.var + '">' + esc(m.label) + "</option>";
      }).join("") + "</optgroup>";
    }).join("");
  }
  function syncControls() {
    ["c", "x", "y", "v"].forEach(function (k) { var el = $("cs-" + k); if (el) el.value = S[k]; });
    document.querySelectorAll("input[name=cs-arr]").forEach(function (r) { r.checked = r.value === S.a; });
    document.querySelectorAll("input[name=cs-lvl]").forEach(function (r) { r.checked = r.value === S.l; });
    $("cs-dept").value = S.d; $("cs-dens").value = String(S.n);
    document.querySelectorAll(".cs-xy").forEach(function (e) { e.hidden = S.a !== "scatter"; });
  }

  /* ---------- Histogramme brossable ---------- */
  var HB = { nb: 44, w: 560, h: 92, ml: 8, mr: 8, mt: 4, mb: 20 };
  function binColour(v, t) {                 // couleur de classe du centre de la classe
    var d = dom(v), x = d.log ? Math.exp(Math.log(d.lo) + t * (Math.log(d.hi) - Math.log(d.lo))) : d.lo + t * (d.hi - d.lo);
    return isFinite(x) ? Q.pal[Stats.classOf(x, Q.breaks.breaks) - 1] : theme.na;
  }
  function bx(t) { return HB.ml + t * (HB.w - HB.ml - HB.mr); }
  function valAt(px) {                       // abscisse SVG -> valeur ; bords = ouvert
    var t = (px - HB.ml) / (HB.w - HB.ml - HB.mr), d = dom(S.c);
    if (t <= 0.004) return -Infinity; if (t >= 0.996) return Infinity;
    return d.log ? Math.exp(Math.log(d.lo) + t * (Math.log(d.hi) - Math.log(d.lo))) : d.lo + t * (d.hi - d.lo);
  }
  function posOf(val) {
    var d = dom(S.c);
    if (val === -Infinity) return HB.ml; if (val === Infinity) return HB.w - HB.mr;
    var t = d.log ? (Math.log(Math.max(val, d.lo * 1e-6)) - Math.log(d.lo)) / (Math.log(d.hi) - Math.log(d.lo)) : (val - d.lo) / (d.hi - d.lo);
    return bx(Math.max(0, Math.min(1, t)));
  }
  function drawHist() {
    var svg = $("cs-hist"); HB.w = Math.max(240, svg.clientWidth || 560); svg.setAttribute("viewBox", "0 0 " + HB.w + " " + HB.h);
    var d = dom(S.c), c = Core.hist(COLS[S.c], d.lo, d.hi, HB.nb, null, d.log), mx = 0, i, out = "";
    c.forEach(function (n) { mx = Math.max(mx, Math.sqrt(n)); });
    var bw = (HB.w - HB.ml - HB.mr) / HB.nb, base = HB.h - HB.mb;
    for (i = 0; i < HB.nb; i++) {
      var bh = mx ? Math.sqrt(c[i]) / mx * (base - HB.mt) : 0;
      out += '<rect x="' + (bx(i / HB.nb) + 0.4).toFixed(1) + '" y="' + (base - bh).toFixed(1) + '" width="' + (bw - 0.8).toFixed(1) + '" height="' + bh.toFixed(1) + '" fill="' + binColour(S.c, (i + 0.5) / HB.nb) + '"/>';
    }
    Q.breaks.breaks.slice(1, -1).forEach(function (b) {
      var x = posOf(b); if (x > HB.ml && x < HB.w - HB.mr) out += '<line x1="' + x.toFixed(1) + '" x2="' + x.toFixed(1) + '" y1="0" y2="' + base + '" class="cs-brk"/>';
    });
    niceTicks(d.lo, d.hi, d.log).filter(function (t, k, a) { return a.length <= 7 || k % 2 === 0; }).forEach(function (t) {
      out += '<text x="' + posOf(t).toFixed(1) + '" y="' + (HB.h - 6) + '" text-anchor="middle" class="cs-tick">' + esc(tickLabel(S.c, t)) + "</text>";
    });
    out += '<line x1="' + HB.ml + '" x2="' + (HB.w - HB.mr) + '" y1="' + (base + 0.5) + '" y2="' + (base + 0.5) + '" class="cs-axis"/>';
    out += '<g id="cs-bg"></g>';
    svg.innerHTML = out;
    svg.setAttribute("aria-label", "Histogramme de " + labelOf(S.c) + " sur toutes les communes, hauteur proportionnelle à la racine de l’effectif. Faites glisser pour sélectionner une plage, ou utilisez les champs De et À.");
    drawBrushRect();
  }
  function drawBrushRect() {
    var g = $("cs-bg"); if (!g) return;
    var base = HB.h - HB.mb;
    if (!S.brush) { g.innerHTML = ""; return; }
    var x0 = posOf(S.brush[0]), x1 = posOf(S.brush[1]);
    g.innerHTML = '<rect x="' + x0.toFixed(1) + '" y="0" width="' + Math.max(1, x1 - x0).toFixed(1) + '" height="' + base + '" class="cs-brush"/>' +
      '<line x1="' + x0.toFixed(1) + '" x2="' + x0.toFixed(1) + '" y1="0" y2="' + base + '" class="cs-handle"/><line x1="' + x1.toFixed(1) + '" x2="' + x1.toFixed(1) + '" y1="0" y2="' + base + '" class="cs-handle"/>';
  }
  function bindBrush() {
    var svg = $("cs-hist"), drag = null;
    function px(ev) { var r = svg.getBoundingClientRect(); return (ev.clientX - r.left) / r.width * HB.w; }
    svg.addEventListener("pointerdown", function (ev) {
      var p = px(ev);
      if (S.brush && p > posOf(S.brush[0]) + 3 && p < posOf(S.brush[1]) - 3) drag = { mode: "move", p0: p, b0: S.brush.slice() };
      else drag = { mode: "new", p0: p };
      svg.setPointerCapture(ev.pointerId); ev.preventDefault();
    });
    svg.addEventListener("pointermove", function (ev) {
      if (!drag) return;
      var p = px(ev), d = dom(S.c), tspan = HB.w - HB.ml - HB.mr;
      if (drag.mode === "new") { var a = Math.min(drag.p0, p), b = Math.max(drag.p0, p); if (b - a < 2) return; S.brush = [valAt(a), valAt(b)]; }
      else {
        var shift = (p - drag.p0) / tspan, pa = (posOf(drag.b0[0]) - HB.ml) / tspan + shift, pb = (posOf(drag.b0[1]) - HB.ml) / tspan + shift;
        if (pa < 0) { pb -= pa; pa = 0; } if (pb > 1) { pa -= pb - 1; pb = 1; }
        S.brush = [valAt(HB.ml + pa * tspan), valAt(HB.ml + pb * tspan)];
        if (drag.b0[0] === -Infinity && pa <= 0.004) S.brush[0] = -Infinity;
      }
      brushChanged(true);
    });
    function end(ev) {
      if (!drag) return;
      if (drag.mode === "new" && Math.abs(px(ev) - drag.p0) < 3) { S.brush = null; }
      drag = null; brushChanged(false);
    }
    svg.addEventListener("pointerup", end); svg.addEventListener("pointercancel", end);
    function fromInputs() {
      var a = $("cs-b0").value.trim(), b = $("cs-b1").value.trim();
      var lo = a === "" ? -Infinity : +a.replace(",", "."), hi = b === "" ? Infinity : +b.replace(",", ".");
      if (isNaN(lo) || isNaN(hi)) return;
      if (pow10(S.c)) { lo = isFinite(lo) ? Math.log10(Math.max(lo, 1)) : lo; hi = isFinite(hi) ? Math.log10(Math.max(hi, 1)) : hi; }
      S.brush = a === "" && b === "" ? null : [Math.min(lo, hi), Math.max(lo, hi)];
      brushChanged(false);
    }
    $("cs-b0").addEventListener("change", fromInputs); $("cs-b1").addEventListener("change", fromInputs);
    $("cs-bclear").addEventListener("click", function () { S.brush = null; brushChanged(false); });
  }
  var brushT = 0;
  function brushChanged(live) {
    applyBrush(); drawBrushRect(); draw();
    clearTimeout(brushT);
    brushT = setTimeout(function () { updatePanel(); writeHash(); syncBrushInputs(); }, live ? 60 : 0);
    if (!live) syncBrushInputs();
  }
  function rawVal(v) { return v === -Infinity || v === Infinity ? "" : (pow10(S.c) ? Math.round(Math.pow(10, v)) : +v.toPrecision(5)); }
  function syncBrushInputs() {
    var b = S.brush;
    if (document.activeElement !== $("cs-b0")) $("cs-b0").value = b ? rawVal(b[0]) : "";
    if (document.activeElement !== $("cs-b1")) $("cs-b1").value = b ? rawVal(b[1]) : "";
  }

  /* ---------- Panneau (sélection, brosse, 2e indicateur, tableau) ---------- */
  function selection() {                   // communes retenues : filtre (département, densité) puis plage brossée
    var rows = [], i, d = S.d === "" ? -1 : D.deptRow[S.d], dn = +S.n;
    for (i = 0; i < CL.n; i++) {
      if (!CL.ok[i]) continue;
      if (d >= 0 && D.idx.d[i] !== d) continue;
      if (dn > 0 && D.dens && D.dens[i] !== dn) continue;
      rows.push(i);
    }
    return rows;
  }
  function updatePanel() {
    var rows = selection(), i, nSel = 0, popSel = 0, popAll = 0, cvals = COLS[S.c], pops = D.pop, inB = [];
    var brush = S.brush;
    rows.forEach(function (r) {
      var p = pops[r] > 0 ? pops[r] : 0; popAll += p;
      if (!brush || (isFinite(cvals[r]) && cvals[r] >= brush[0] && cvals[r] <= brush[1])) { nSel++; popSel += p; inB.push(r); }
    });
    var srcC = MET[S.c].src;
    var read = $("cs-bread");
    if (brush) {
      var lo = brush[0] === -Infinity ? "min." : show(S.c, brush[0], true), hi = brush[1] === Infinity ? "max." : show(S.c, brush[1], true);
      read.innerHTML = "<strong>" + fmt(nSel, 0) + "</strong> communes sur " + fmt(rows.length, 0) + " (" + fmt(100 * nSel / Math.max(1, rows.length), 1) + " %), " +
        "<strong>" + fmt(popAll ? 100 * popSel / popAll : 0, 1) + " %</strong> de la population, entre <strong>" + esc(lo) + "</strong> et <strong>" + esc(hi) + "</strong> " +
        esc(unitOf(S.c)) + " <span class=\"cs-src\">(" + esc(labelOf(S.c)) + ", " + esc(srcC) + ")</span>";
    } else {
      read.innerHTML = "Aucune plage choisie : toutes les " + fmt(rows.length, 0) + " communes du filtre sont affichées. Faites glisser sur l’histogramme pour isoler une plage de <strong>" +
        esc(labelOf(S.c)) + "</strong> <span class=\"cs-src\">(" + esc(srcC) + ")</span>.";
    }
    // second indicateur : distribution dans la sélection contre toutes les communes
    var v2 = S.v, c2 = COLS[v2], d2 = dom(v2), nb = 30;
    var mAll = new Uint8Array(CL.n), mSel = new Uint8Array(CL.n);
    rows.forEach(function (r) { mAll[r] = 1; });
    inB.forEach(function (r) { mSel[r] = 1; });
    var hAll = Core.hist(c2, d2.lo, d2.hi, nb, mAll, d2.log), hSel = Core.hist(c2, d2.lo, d2.hi, nb, mSel, d2.log);
    var tA = hAll.reduce(function (a, b) { return a + b; }, 0) || 1, tS = hSel.reduce(function (a, b) { return a + b; }, 0) || 1;
    var mxv = 0; hAll.forEach(function (n, k) { mxv = Math.max(mxv, n / tA, hSel[k] / tS); });
    var svg = $("cs-h2"), w = Math.max(200, svg.clientWidth || 320), h = 70; svg.setAttribute("viewBox", "0 0 " + w + " " + h);
    var base = h - 18, bw = (w - 12) / nb, out = "", k;
    for (k = 0; k < nb; k++) {
      var ha = mxv ? hAll[k] / tA / mxv * (base - 4) : 0, hs = mxv ? hSel[k] / tS / mxv * (base - 4) : 0, x = 6 + k * bw;
      out += '<rect x="' + (x + 0.3).toFixed(1) + '" y="' + (base - ha).toFixed(1) + '" width="' + (bw - 0.6).toFixed(1) + '" height="' + ha.toFixed(1) + '" class="cs-h2-all"/>';
      out += '<rect x="' + (x + 1.3).toFixed(1) + '" y="' + (base - hs).toFixed(1) + '" width="' + Math.max(0, bw - 2.6).toFixed(1) + '" height="' + hs.toFixed(1) + '" class="cs-h2-sel"/>';
    }
    niceTicks(d2.lo, d2.hi, d2.log).filter(function (t, kk, a) { return a.length <= 6 || kk % 2 === 0; }).forEach(function (t) {
      var tt = d2.log ? (Math.log(t) - Math.log(d2.lo)) / (Math.log(d2.hi) - Math.log(d2.lo)) : (t - d2.lo) / (d2.hi - d2.lo);
      out += '<text x="' + (6 + tt * (w - 12)).toFixed(1) + '" y="' + (h - 5) + '" text-anchor="middle" class="cs-tick">' + esc(tickLabel(v2, t)) + "</text>";
    });
    out += '<line x1="6" x2="' + (w - 6) + '" y1="' + (base + 0.5) + '" y2="' + (base + 0.5) + '" class="cs-axis"/>';
    svg.innerHTML = out;
    var med = function (rowsX) { return Core.weightedMedian(rowsX.map(function (r) { return c2[r]; }), rowsX.map(function (r) { return pops[r]; })); };
    var mA = med(rows), mS = med(inB);
    $("cs-h2read").innerHTML = "Médiane (pondérée par la population) de <strong>" + esc(labelOf(v2)) + "</strong> : " +
      (brush ? "sélection <strong>" + esc(show(v2, mS)) + "</strong> contre " : "") + "filtre <strong>" + esc(show(v2, mA)) + "</strong> " +
      '<span class="cs-src">(' + esc(MET[v2].src) + "). " + fmt(Core.hist(c2, d2.lo, d2.hi, 1, mSel, false)[0], 0) + " communes avec une valeur publiée dans la sélection.</span>";
    // tableau accessible : 20 premières communes de la sélection
    var vals = inB.filter(function (r) { return isFinite(cvals[r]); }).sort(function (a, b) { return cvals[b] - cvals[a]; }).slice(0, 20);
    $("cs-top").innerHTML = "<caption>20 communes de la sélection ayant la valeur la plus élevée de " + esc(labelOf(S.c)) + " (" + esc(srcC) + ")</caption>" +
      "<thead><tr><th scope=\"col\">Commune</th><th scope=\"col\" class=\"num\">" + esc(labelOf(S.c)) + "</th></tr></thead><tbody>" +
      vals.map(function (r) {
        return '<tr><th scope="row"><button type="button" class="cs-link" data-code="' + D.idx.c[r] + '">' + esc(D.idx.n[r]) + "</button> <span class=\"cs-src\">" +
          esc(D.idx.depts[D.idx.d[r]] ? D.idx.depts[D.idx.d[r]].code : "") + '</span></th><td class="num">' + esc(show(S.c, cvals[r])) + "</td></tr>";
      }).join("") + "</tbody>";
    updateStats();
    updateDetail();
  }
  function updateStats() {
    var el = $("cs-stats"), cap = $("cs-caption");
    var lvl = S.l === "dept" ? "Chaque point est la médiane départementale (pondérée par la population) ; couleur : " : "Chaque point est une commune ; couleur : ";
    cap.innerHTML = lvl + "<strong>" + esc(labelOf(S.c)) + "</strong> <span class=\"cs-src\">(" + esc(MET[S.c].src) + ")</span>. " +
      "Seuils en quintiles des communes publiées" + (Q.breaks.zero ? " (classe 1 : valeur 0)" : "") + ".";
    if (S.a !== "scatter") { el.hidden = true; return; }
    var rows = selection(), xv = COLS[S.x], yv = COLS[S.y], a = [], b = [];
    rows.forEach(function (r) { a.push(xv[r]); b.push(yv[r]); });
    var sp = Core.spearman(a, b), mx = MET[S.x], my = MET[S.y];
    el.hidden = false;
    el.innerHTML = "<strong>Spearman ρ = " + (isFinite(sp.rho) ? fmt(sp.rho, 2) : "—") + "</strong> sur <strong>" + fmt(sp.n, 0) + "</strong> communes ayant les deux valeurs " +
      "(abscisse : " + esc(labelOf(S.x)) + ", " + esc(mx.src) + " ; ordonnée : " + esc(labelOf(S.y)) + ", " + esc(my.src) + "). " +
      "<em>Association, pas causalité</em> : deux indicateurs qui vont ensemble peuvent partager une cause commune (densité, taille, âge), et une corrélation entre communes ne dit rien des individus " +
      "(biais écologique). Les dates des deux sources diffèrent." + (S.l === "dept" ? " Les points départementaux sont des médianes : ρ est calculé sur les communes, pas sur ces médianes." : "");
  }
  function updateDetail() {
    var el = $("cs-detail");
    if (!S.code) { el.innerHTML = '<p class="cs-hint">Aucune commune sélectionnée. Cliquez un point, ou cherchez un nom ci-dessus : elle reste surlignée dans toutes les dispositions.</p>'; return; }
    var i = rowOf(S.code); if (i < 0) { el.innerHTML = ""; return; }
    var dp = D.idx.depts[D.idx.d[i]], rows = [S.c];
    if (S.a === "scatter") { if (rows.indexOf(S.x) < 0) rows.push(S.x); if (rows.indexOf(S.y) < 0) rows.push(S.y); }
    if (S.v && rows.indexOf(S.v) < 0) rows.push(S.v);
    var pc = COLS[S.c] && isFinite(COLS[S.c][i]) ? Math.round(Q.ranks[i] * 100) : null;
    el.innerHTML = '<div class="cs-dh"><h3>' + esc(D.idx.n[i]) + '</h3><div class="cs-dm">' + esc(dp ? dp.nom + " (" + dp.code + ")" : "") + " · " + S.code +
      " · " + fmt(D.pop[i], 0) + " hab. <span class=\"cs-src\">(INSEE, population 2021)</span></div></div>" +
      '<div class="cs-kvs">' + rows.map(function (v) {
        var val = COLS[v][i];
        return '<div class="cs-kv"><span>' + esc(labelOf(v)) + ' <span class="cs-src">' + esc(MET[v].src) + '</span></span><span class="cs-kvn">' + esc(show(v, val)) + "</span></div>";
      }).join("") + "</div>" +
      (pc != null ? '<p class="cs-hint">Rang centile national de ' + esc(labelOf(S.c)) + " : <strong>" + pc + "</strong> sur 100 (0 = valeur la plus basse).</p>" : "") +
      '<p class="cs-hint"><a href="profil.html#' + S.code + '">Ouvrir le profil complet</a></p>';
  }
  function refreshUI() {
    syncControls(); drawHist(); syncBrushInputs();
    updatePanel(); writeHash();
    var nv = nVisible();
    setStatus((S.l === "dept" ? D.nd_placed + " départements (médiane pondérée par la population)" : fmt(nv, 0) + " communes") + " · disposition : " + ARR_LABEL[S.a] + ".");
    cv.setAttribute("aria-label", "Graphique de points. Disposition : " + ARR_LABEL[S.a] + ". Couleur : " + labelOf(S.c) + ". " +
      (S.code ? "Commune sélectionnée : " + D.idx.n[rowOf(S.code)] + ". " : "") + "Les données exactes sont dans le tableau des 20 premières communes de la sélection.");
  }

  /* ---------- Survol, clic ---------- */
  function pointer(ev) {
    var r = cv.getBoundingClientRect(), mx = ev.clientX - r.left, my = ev.clientY - r.top;
    var L = S.l === "dept" ? DL : CL, best = -1, bd = 1e9, i, lim;
    for (i = 0; i < L.n; i++) {
      if (!L.ok[i] || L.hid[i] || L.nan[i] || (L.dim[i] && matchCom)) continue;
      var dx = L.x[i] - mx, dy = L.y[i] - my, d2 = dx * dx + dy * dy, rr = Math.max(L.r[i], 3) + 4;
      if (d2 < rr * rr && d2 < bd) { bd = d2; best = i; }
    }
    return { i: best, kind: L === DL ? "d" : "c", x: mx, y: my };
  }
  var tip;
  function bindPointer() {
    tip = $("cs-tip");
    var pend = null;
    cv.addEventListener("pointermove", function (ev) {
      if (pend) return; pend = ev;
      requestAnimationFrame(function () {
        var e = pend; pend = null; if (anim.on) return;
        var h = pointer(e);
        if (h.i !== hover || h.kind !== hoverKind) { hover = h.i; hoverKind = h.kind; draw(); }
        if (h.i < 0) { tip.hidden = true; cv.style.cursor = "default"; return; }
        cv.style.cursor = "pointer";
        var html;
        if (h.kind === "d") {
          var dp = D.idx.depts[h.i];
          html = "<strong>" + esc(dp.nom) + " (" + esc(dp.code) + ")</strong><br>" + esc(labelOf(S.c)) + " : " + esc(show(S.c, Q.agg.v[h.i])) +
            ' <span class="cs-src">médiane pondérée · ' + esc(MET[S.c].src) + "</span><br>" + fmt(Q.agg.pop[h.i], 0) + " hab. · clic : voir les communes";
        } else {
          html = "<strong>" + esc(D.idx.n[h.i]) + "</strong> (" + esc(D.idx.depts[D.idx.d[h.i]].code) + ")<br>" + esc(labelOf(S.c)) + " : " + esc(show(S.c, COLS[S.c][h.i])) +
            ' <span class="cs-src">' + esc(MET[S.c].src) + "</span>" +
            (S.a === "scatter" ? "<br>" + esc(labelOf(S.x)) + " : " + esc(show(S.x, COLS[S.x][h.i])) + "<br>" + esc(labelOf(S.y)) + " : " + esc(show(S.y, COLS[S.y][h.i])) : "") +
            "<br>" + fmt(D.pop[h.i], 0) + " hab.";
        }
        tip.innerHTML = html; tip.hidden = false;
        var bx_ = Math.min(h.x + 14, cv.clientWidth - tip.offsetWidth - 4); tip.style.left = Math.max(4, bx_) + "px";
        tip.style.top = Math.min(h.y + 14, cv.clientHeight - tip.offsetHeight - 4) + "px";
      });
    });
    cv.addEventListener("pointerleave", function () { hover = -1; tip.hidden = true; if (!anim.on) draw(); });
    cv.addEventListener("click", function (ev) {
      var h = pointer(ev); if (h.i < 0) return;
      if (h.kind === "d") { setDept(D.idx.depts[h.i].code); }
      else selectCommune(D.idx.c[h.i]);
    });
  }

  /* ---------- Actions ---------- */
  function selectCommune(code) {
    S.code = code; var i = rowOf(code);
    selDot.seen = false;
    if (i >= 0 && CL.ok[i] && S.l === "dept") { /* le point reste visible via l'anneau de sélection */ }
    selTarget(S.a, fit(), plot(), S.a === "scatter" ? scaleOf(S.x) : null, S.a === "scatter" ? scaleOf(S.y) : (S.a === "rank" ? scaleOf(S.c) : null));
    selDot.fx = selDot.x = selDot.tx; selDot.fy = selDot.y = selDot.ty;
    draw(); updateDetail(); writeHash();
    setStatus("Commune sélectionnée : " + D.idx.n[i] + ".");
  }
  function setDept(code) { S.d = code; S.l = "com"; apply({ reveal: true }); }
  function set(patch, o) {
    var prevL = S.l;
    Object.keys(patch).forEach(function (k) { S[k] = patch[k]; });
    if (patch.c) { S.brush = patch.brush || null; }
    if ((patch.d != null && patch.d !== "") || (patch.n != null && +patch.n > 0)) S.l = "com";
    if (patch.l === "dept") { S.d = ""; S.n = 0; }
    return apply(Object.assign({ reveal: prevL === "dept" && S.l === "com" }, o || {}));
  }

  function bindControls() {
    document.querySelectorAll("input[name=cs-arr]").forEach(function (r) { r.addEventListener("change", function () { set({ a: r.value }); }); });
    document.querySelectorAll("input[name=cs-lvl]").forEach(function (r) { r.addEventListener("change", function () { set({ l: r.value }); }); });
    $("cs-c").addEventListener("change", function () { set({ c: this.value }); });
    $("cs-x").addEventListener("change", function () { set({ x: this.value }); });
    $("cs-y").addEventListener("change", function () { set({ y: this.value }); });
    $("cs-v").addEventListener("change", function () { set({ v: this.value }, { dur: 0 }); });
    $("cs-dept").addEventListener("change", function () { set({ d: this.value }); });
    $("cs-dens").addEventListener("change", function () { set({ n: +this.value }); });
    $("cs-top").addEventListener("click", function (e) { var b = e.target.closest(".cs-link"); if (b) selectCommune(b.dataset.code); });
    $("cs-reset").addEventListener("click", function () { S.code = null; S.brush = null; set({ a: DEF.a, c: DEF.c, x: DEF.x, y: DEF.y, v: DEF.v, l: "dept", d: "", n: 0 }); });
    document.querySelectorAll("[data-preset]").forEach(function (b) {
      b.addEventListener("click", function () { preset(b.dataset.preset); });
    });
    bindBrush(); bindPointer(); bindSearch();
  }
  function quantileOf(v, p) { return Core.quantile(Core.finiteSorted(COLS[v]), p); }
  function preset(name) {
    if (name === "scatter") return set({ a: "scatter", c: "revenu_median", x: "revenu_median", y: "apl_medecins", v: "apl_medecins", l: "com", d: "", n: 0 });
    if (name === "pop") return set({ a: "pop", l: "dept", d: "", n: 0 });
    if (name === "affordable") {
      return col("dvf_prix_mai").then(function () {
        return set({ a: "geo", c: "dvf_prix_mai", v: "apl_medecins", l: "com", d: "", n: 0, brush: [-Infinity, quantileOf("dvf_prix_mai", 0.25)] });
      });
    }
  }

  /* ---------- Recherche ---------- */
  function bindSearch() {
    var inp = $("cs-search"), ul = $("cs-suggest"), names = null, act = -1, res = [];
    function build() {
      if (names) return;
      names = D.idx.n.map(function (n, i) { return fold(n); });
    }
    function show_() {
      ul.innerHTML = res.map(function (i, k) {
        return '<li role="option" id="cs-sg' + k + '" data-i="' + i + '"' + (k === act ? ' aria-selected="true"' : "") + ">" + esc(D.idx.n[i]) + ' <span class="cs-src">' + esc(D.idx.depts[D.idx.d[i]].code) + "</span></li>";
      }).join("");
      ul.hidden = !res.length; inp.setAttribute("aria-expanded", res.length ? "true" : "false");
    }
    function pick(i) {
      ul.hidden = true; inp.value = D.idx.n[i];
      if (!CL.ok[i]) { setStatus(D.idx.n[i] + " n’a pas de position sur la carte."); return; }
      if (S.d !== "" && D.idx.depts[D.idx.d[i]].code !== S.d) { S.d = ""; }
      S.code = D.idx.c[i];
      if (S.l === "dept") { S.d = D.idx.depts[D.idx.d[i]].code; S.l = "com"; apply({ reveal: true }); }
      else { selectCommune(S.code); }
    }
    inp.addEventListener("input", function () {
      build(); var q = fold(inp.value.trim()); act = -1;
      if (q.length < 2) { res = []; show_(); return; }
      var a = [], b = [], i;
      for (i = 0; i < names.length; i++) { if (!CL.ok[i]) continue; if (names[i].indexOf(q) === 0) a.push(i); }
      for (i = 0; i < names.length && b.length < 200; i++) { if (!CL.ok[i]) continue; if (names[i].indexOf(q) > 0) b.push(i); }
      var byPop = function (x, y) { return (D.pop[y] || 0) - (D.pop[x] || 0); };
      res = a.sort(byPop).concat(b.sort(byPop)).slice(0, 8); show_();
    });
    inp.addEventListener("keydown", function (e) {
      if (e.key === "ArrowDown") { act = Math.min(res.length - 1, act + 1); show_(); e.preventDefault(); }
      else if (e.key === "ArrowUp") { act = Math.max(0, act - 1); show_(); e.preventDefault(); }
      else if (e.key === "Enter" && res.length) { pick(res[Math.max(0, act)]); e.preventDefault(); }
      else if (e.key === "Escape") { ul.hidden = true; }
    });
    ul.addEventListener("mousedown", function (e) { var li = e.target.closest("li"); if (li) { pick(+li.dataset.i); e.preventDefault(); } });
    inp.addEventListener("blur", function () { setTimeout(function () { ul.hidden = true; }, 120); });
  }

  /* ---------- Taille du canevas ---------- */
  function resize() {
    var box = cv.parentNode, w = box.clientWidth, h = box.clientHeight;
    if (w < 10 || h < 10) return;
    dpr = Math.min(2, window.devicePixelRatio || 1);
    W = w; H = h;
    cv.width = Math.round(w * dpr); cv.height = Math.round(h * dpr);
    cv.style.width = w + "px"; cv.style.height = h + "px";
    ctx = cv.getContext("2d");
    if (CL && D.geo && D.ready) { readTheme(); targets(S.a); [CL, DL].forEach(function (L) { L.x.set(L.tx); L.y.set(L.ty); L.r.set(L.tr); }); selDot.x = selDot.tx; selDot.y = selDot.ty; draw(); }
  }

  /* ---------- Démarrage ---------- */
  function boot() {
    cv = $("cs-canvas"); if (!cv) return;
    setStatus("Chargement des communes…");
    readHash();
    Promise.all([CMap.data.index(), getJSON("data/constellation.json"), col("ctx_pop_latest")]).then(function (r) {
      D.idx = r[0]; D.geo = r[1]; D.pop = r[2]; D.nd = D.idx.depts.length;
      D.deptRow = {}; D.idx.depts.forEach(function (d, i) { D.deptRow[d.code] = i; });
      var n = D.idx.c.length;
      CL = mkLayer(n); DL = mkLayer(D.nd);
      for (var i = 0; i < n; i++) CL.ok[i] = D.geo.gx[i] != null ? 1 : 0;
      D.nd_placed = 0;
      for (i = 0; i < D.nd; i++) { DL.ok[i] = D.geo.dep.gx[i] != null ? 1 : 0; D.nd_placed += DL.ok[i]; }
      // listes déroulantes
      [$("cs-c"), $("cs-x"), $("cs-y"), $("cs-v")].forEach(fillSelect);
      $("cs-dept").innerHTML = '<option value="">Toutes (France entière)</option>' + D.idx.depts.map(function (d) { return '<option value="' + esc(d.code) + '">' + esc(d.code + " · " + d.nom) + "</option>"; }).join("");
      $("cs-dens").innerHTML = '<option value="0">Toutes densités</option>' + CMap.densLabels.map(function (l, k) { return '<option value="' + (k + 1) + '">' + (k + 1) + " · " + esc(l) + "</option>"; }).join("");
      document.querySelector(".cs-wrap").classList.add("cs-ready");
      resize();
      // position initiale : tout au centre de son point cible, apparition par fondu
      return ensureCols().then(function () {
        recolorData(); applyFilter(); targets(S.a); rebuildBuckets(CL); rebuildBuckets(DL); D.ready = true;
        [CL, DL].forEach(function (L) { L.x.set(L.tx); L.y.set(L.ty); L.r.set(L.tr); });
        selDot.x = selDot.tx; selDot.y = selDot.ty;
        TW.com = [S.l === "com" ? 1 : 0, S.l === "com" ? 1 : 0, S.l === "com" ? 1 : 0]; TW.dep = [S.l === "dept" ? 1 : 0, S.l === "dept" ? 1 : 0, S.l === "dept" ? 1 : 0];
        var fr = S.a === "geo" || S.a === "pop" ? 1 : 0; TW.frame = [fr, fr, fr]; TW.axes = [1 - fr, 1 - fr, 1 - fr];
        readTheme(); bindControls(); refreshUI(); draw();
        setStatus("Prêt. " + D.nd_placed + " départements.");
      });
    }).catch(function (e) { console.error(e); setStatus("Données indisponibles : " + e.message); });
    if (window.ResizeObserver) {
      var t; new ResizeObserver(function () { clearTimeout(t); t = setTimeout(resize, 80); }).observe(cv.parentNode);
    }
    window.addEventListener("hashchange", function () { /* lien partagé collé dans la même page */
      var old = JSON.stringify(S); readHash(); if (JSON.stringify(S) !== old && CL) apply({ dur: 0 });
    });
    new MutationObserver(function () { readTheme(); draw(); }).observe(document.documentElement, { attributes: true, attributeFilter: ["data-theme"] });
    if (window.matchMedia) matchMedia("(prefers-color-scheme: dark)").addEventListener("change", function () { readTheme(); draw(); });
  }
  window.Constellation = { state: S, set: set, preset: preset, select: function (c) { return selectCommune(c); }, layers: function () { return { CL: CL, DL: DL }; } };
  if (document.readyState !== "loading") boot(); else document.addEventListener("DOMContentLoaded", boot);
  }
})();
