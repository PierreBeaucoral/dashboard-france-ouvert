/* ============================================================
 * constellation-core.js : fonctions pures du moteur « constellation » (sans DOM),
 * testées sous Node (tests/check_constellation.js).
 *
 *   weightedMedian(v, w)          médiane pondérée (poids nul partout : médiane simple)
 *   deptAggregate(v, pop, d, nd)  valeur par département = médiane pondérée par la population
 *   inRange(v, lo, hi)            masque Uint8 : communes dont la valeur est dans [lo, hi]
 *   spearman(x, y)                {rho, n} sur les paires complètes (rangs moyens)
 *   domain(v, log)                bornes d'axe fixes P1-P99 (+ choix d'échelle log)
 *   hist(v, lo, hi, nb, mask)     effectifs par classe sur un domaine fixe
 *   ranks(v)                      rang centile 0..1 (rangs moyens), NaN conservé
 * ============================================================ */
(function (root) {
  "use strict";
  var ok = function (x) { return x != null && isFinite(x); };

  function quantile(sorted, p) {
    var h = (sorted.length - 1) * p, lo = Math.floor(h), hi = Math.ceil(h);
    return sorted[lo] + (h - lo) * (sorted[hi] - sorted[lo]);
  }
  function finiteSorted(v) {
    var o = [], i;
    for (i = 0; i < v.length; i++) if (ok(v[i])) o.push(v[i]);
    return o.sort(function (a, b) { return a - b; });
  }

  /** Plus petite valeur dont le poids cumulé atteint la moitié du poids total. */
  function weightedMedian(v, w) {
    var items = [], tot = 0, i;
    for (i = 0; i < v.length; i++) if (ok(v[i])) { var wi = w && w[i] > 0 ? w[i] : 0; items.push([v[i], wi]); tot += wi; }
    if (!items.length) return NaN;
    if (!(tot > 0)) { items.forEach(function (it) { it[1] = 1; }); tot = items.length; }
    items.sort(function (a, b) { return a[0] - b[0]; });
    var acc = 0;
    for (i = 0; i < items.length; i++) { acc += items[i][1]; if (acc >= tot / 2) return items[i][0]; }
    return items[items.length - 1][0];
  }

  /** v, pop : colonnes communes ; d : indice de département par commune (null = hors départements). */
  function deptAggregate(v, pop, d, nd) {
    var gv = [], gw = [], i, out = { v: new Float64Array(nd), pop: new Float64Array(nd), n: new Int32Array(nd) };
    for (i = 0; i < nd; i++) { gv.push([]); gw.push([]); }
    for (i = 0; i < v.length; i++) {
      if (d[i] == null || d[i] < 0) continue;
      var p = pop[i] > 0 ? pop[i] : 0;
      out.pop[d[i]] += p; out.n[d[i]]++;
      if (ok(v[i])) { gv[d[i]].push(v[i]); gw[d[i]].push(p); }
    }
    for (i = 0; i < nd; i++) out.v[i] = gv[i].length ? weightedMedian(gv[i], gw[i]) : NaN;
    return out;
  }

  function inRange(v, lo, hi) {
    var m = new Uint8Array(v.length), i;
    for (i = 0; i < v.length; i++) m[i] = ok(v[i]) && v[i] >= lo && v[i] <= hi ? 1 : 0;
    return m;
  }

  /** Rang centile (0..1), rangs moyens pour les ex æquo ; NaN si valeur absente. */
  function ranks(v) {
    var idx = [], i, out = new Float64Array(v.length).fill(NaN);
    for (i = 0; i < v.length; i++) if (ok(v[i])) idx.push(i);
    idx.sort(function (a, b) { return v[a] - v[b]; });
    var n = idx.length, j = 0;
    while (j < n) {
      var k = j; while (k + 1 < n && v[idx[k + 1]] === v[idx[j]]) k++;
      var r = n > 1 ? (j + k) / 2 / (n - 1) : 0.5;
      for (var t = j; t <= k; t++) out[idx[t]] = r;
      j = k + 1;
    }
    return out;
  }

  function spearman(x, y) {
    var a = [], b = [], i;
    for (i = 0; i < x.length; i++) if (ok(x[i]) && ok(y[i])) { a.push(x[i]); b.push(y[i]); }
    var n = a.length; if (n < 3) return { rho: NaN, n: n };
    var ra = ranks(a), rb = ranks(b), ma = 0, mb = 0;
    for (i = 0; i < n; i++) { ma += ra[i]; mb += rb[i]; }
    ma /= n; mb /= n;
    var sab = 0, saa = 0, sbb = 0;
    for (i = 0; i < n; i++) { var da = ra[i] - ma, db = rb[i] - mb; sab += da * db; saa += da * da; sbb += db * db; }
    return { rho: saa > 0 && sbb > 0 ? sab / Math.sqrt(saa * sbb) : NaN, n: n };
  }

  /** Bornes P1-P99 ; log=true si toutes les valeurs sont > 0 et la queue haute très étirée. */
  function domain(v, forceLog) {
    var s = finiteSorted(v);
    if (s.length < 2) return { lo: 0, hi: 1, log: false };
    var lo = quantile(s, 0.01), hi = quantile(s, 0.99), med = quantile(s, 0.5);
    if (!(hi > lo)) { lo = s[0]; hi = s[s.length - 1]; }
    var log = forceLog != null ? forceLog : (s[0] > 0 && lo > 0 && hi / med > 6);
    return { lo: lo, hi: hi > lo ? hi : lo + 1, log: !!log };
  }

  /** Effectifs sur nb classes égales de [lo, hi] (hors bornes : classes extrêmes). mask facultatif. */
  function hist(v, lo, hi, nb, mask, log) {
    var c = new Array(nb).fill(0), i, a = log ? Math.log(lo) : lo, b = log ? Math.log(hi) : hi;
    for (i = 0; i < v.length; i++) {
      if (!ok(v[i]) || (mask && !mask[i]) || (log && v[i] <= 0)) continue;
      var t = ((log ? Math.log(v[i]) : v[i]) - a) / (b - a);
      c[Math.max(0, Math.min(nb - 1, Math.floor(t * nb)))]++;
    }
    return c;
  }

  var api = { weightedMedian: weightedMedian, deptAggregate: deptAggregate, inRange: inRange, ranks: ranks,
              spearman: spearman, domain: domain, hist: hist, quantile: quantile, finiteSorted: finiteSorted };
  if (typeof module !== "undefined" && module.exports) module.exports = api;
  else root.ConstCore = api;
})(typeof window !== "undefined" ? window : this);
