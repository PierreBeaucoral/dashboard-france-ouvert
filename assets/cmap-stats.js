/* ============================================================
 * cmap-stats.js : fonctions pures du module carte (sans DOM), chargées
 * avant cmap.js et testables sous Node (tests/check_cmap_stats.js).
 *
 *   quantile7(sorted, p)           quantile de type 7 (défaut de R)
 *   makeBreaks(values, scale)      seuils de classes, jumeau de R/map_helpers.R::cmap_breaks()
 *   classOf(v, breaks)             classe 1..5
 *   classShares(cls, pops, K)      parts (%) de communes et de population par classe,
 *                                  + part "non publié" : la somme fait 100 %
 *   histogram(values, breaks, log) histogramme (22 classes) pour la légende
 *   weightedQuantiles(v, w, ps)    quantiles pondérés (gradient urbain-rural)
 *   percentile(sorted, v)          rang centile (0-100), ex æquo au rang moyen
 * ============================================================ */
(function (root) {
  "use strict";

  function quantile7(sorted, p) {
    var h = (sorted.length - 1) * p, lo = Math.floor(h), hi = Math.ceil(h);
    return sorted[lo] + (h - lo) * (sorted[hi] - sorted[lo]);
  }
  function uniq(a) { return a.filter(function (v, i) { return a.indexOf(v) === i; }); }
  function num(a, b) { return a - b; }

  /** {breaks:[6 valeurs], zero:bool}. zero=true : la classe 1 est le singleton {0}. */
  function makeBreaks(values, scale) {
    var v = values.filter(function (x) { return x != null && isFinite(x); });
    if (v.length < 2) return { breaks: [0, 1, 2, 3, 4, 5], zero: false };
    var pos = v.filter(function (x) { return x > 0; }).sort(num);
    var all = v.slice().sort(num);
    var nZero = v.filter(function (x) { return x === 0; }).length;
    var br, zero = false, i;
    if (scale === "log_quantile" && pos.length >= 5) {
      var lg = pos.map(Math.log1p);
      br = [0]; zero = true;
      for (i = 0; i <= 4; i++) br.push(Math.expm1(quantile7(lg, i / 4)));
    } else if (scale === "linear") {
      br = []; for (i = 0; i <= 5; i++) br.push(all[0] + (all[all.length - 1] - all[0]) * i / 5);
    } else if (scale !== "log_quantile" && pos.length >= 5 && nZero >= v.length * 0.3) {
      br = [0]; zero = true;
      for (i = 0; i <= 4; i++) br.push(quantile7(pos, i / 4));
    } else {
      br = []; for (i = 0; i <= 5; i++) br.push(quantile7(all, i / 5));
    }
    if (uniq(br).length < 6) {                          // dégénéré -> linéaire
      zero = false; br = [];
      for (i = 0; i <= 5; i++) br.push(all[0] + (all[all.length - 1] - all[0]) * i / 5);
    }
    return { breaks: br, zero: zero };
  }

  /** Classe 1..5 : 1 + nombre de seuils breaks[1..5] <= v (bornée à 5). */
  function classOf(v, br) {
    var c = 1;
    for (var i = 1; i <= 5; i++) if (v >= br[i]) c++;
    return Math.min(5, c);
  }

  /** cls[i] : classe 1..K, ou 0/null (non publié). pops[i] : population (>= 0, null -> 0).
   *  Renvoie {pc[K], pp[K], naC, naP} en %, avec sum(pc)+naC = 100 = sum(pp)+naP. */
  function classShares(cls, pops, K) {
    var nC = new Array(K).fill(0), nP = new Array(K).fill(0), naC = 0, naP = 0, totC = 0, totP = 0, i;
    for (i = 0; i < cls.length; i++) {
      var p = pops[i] > 0 ? pops[i] : 0;
      totC += 1; totP += p;
      if (cls[i] >= 1 && cls[i] <= K) { nC[cls[i] - 1] += 1; nP[cls[i] - 1] += p; }
      else { naC += 1; naP += p; }
    }
    var f = function (x, t) { return t > 0 ? 100 * x / t : 0; };
    return { pc: nC.map(function (x) { return f(x, totC); }), pp: nP.map(function (x) { return f(x, totP); }),
             naC: f(naC, totC), naP: f(naP, totP), n: totC };
  }

  /** Histogramme sur [P1, P99] (valeurs hors bornes dans les classes extrêmes).
   *  log=true : abscisse log1p. Renvoie {x0, x1, log, counts[nb]} (x en échelle d'affichage). */
  function histogram(values, log, nb) {
    nb = nb || 22;
    var v = values.filter(function (x) { return x != null && isFinite(x) && (!log || x >= 0); });
    if (v.length < 2) return null;
    var t = v.map(function (x) { return log ? Math.log1p(x) : x; }).sort(num);
    var x0 = quantile7(t, 0.01), x1 = quantile7(t, 0.99);
    if (!(x1 > x0)) { x0 = t[0]; x1 = t[t.length - 1]; }
    if (!(x1 > x0)) return null;
    var counts = new Array(nb).fill(0);
    for (var i = 0; i < t.length; i++) {
      var k = Math.floor((t[i] - x0) / (x1 - x0) * nb);
      counts[Math.max(0, Math.min(nb - 1, k))]++;
    }
    return { x0: x0, x1: x1, log: !!log, counts: counts };
  }

  /** Quantiles pondérés (définition : plus petite valeur dont le poids cumulé >= p). */
  function weightedQuantiles(vals, w, ps) {
    var o = [], tot = 0, i;
    for (i = 0; i < vals.length; i++) if (vals[i] != null && isFinite(vals[i]) && w[i] > 0) { o.push([vals[i], w[i]]); tot += w[i]; }
    if (!o.length) return ps.map(function () { return null; });
    o.sort(function (a, b) { return a[0] - b[0]; });
    var out = [], acc = 0, j = 0;
    ps.forEach(function (p) {
      while (j < o.length - 1 && (acc + o[j][1]) / tot < p) { acc += o[j][1]; j++; }
      out.push(o[j][0]);
    });
    return out;
  }

  /** Rang centile 0-100 d'une valeur dans un tableau trié (ex æquo : rang moyen). */
  function percentile(sorted, v) {
    var n = sorted.length; if (!n) return null;
    var lo = 0, hi = n;
    while (lo < hi) { var m = (lo + hi) >> 1; if (sorted[m] < v) lo = m + 1; else hi = m; }
    var less = lo; hi = n;
    while (lo < hi) { var m2 = (lo + hi) >> 1; if (sorted[m2] <= v) lo = m2 + 1; else hi = m2; }
    return 100 * (less + 0.5 * (lo - less)) / n;
  }

  /** Lecteur de valeurs qui comble les villes PLM (audit M2). Les cartes sont dessinées à
   *  l'arrondissement : la ville (75056...) n'a alors pas de valeur propre et comptait « non
   *  publié » dans les parts de légende et le gradient. Repli : moyenne simple des
   *  arrondissements publiés (numérique) ou valeur du premier arrondissement (catégoriel).
   *  Approximation limitée aux 3 villes, qui ne sert qu'à classer la ville (pas affichée).
   *  arrOf : {codeVille: [codes arrondissements]}. */
  function withPlmFallback(get, arrOf) {
    return function (c) {
      var v = get(c);
      if (v != null && (typeof v !== "number" || isFinite(v))) return v;
      var a = arrOf[c]; if (!a) return v;
      var s = 0, n = 0, first = null, i, x;
      for (i = 0; i < a.length; i++) {
        x = get(a[i]);
        if (x == null || (typeof x === "number" && !isFinite(x))) continue;
        if (typeof x !== "number") { if (first == null) first = x; continue; }
        s += x; n++;
      }
      return n ? s / n : (first != null ? first : v);
    };
  }

  var api = { withPlmFallback: withPlmFallback, quantile7: quantile7, makeBreaks: makeBreaks, classOf: classOf, classShares: classShares,
              histogram: histogram, weightedQuantiles: weightedQuantiles, percentile: percentile };
  if (typeof module !== "undefined" && module.exports) module.exports = api;
  else root.CMapStats = api;
})(typeof window !== "undefined" ? window : this);
