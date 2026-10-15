/* ============================================================
 * peers-core.js : sélection des communes « semblables » (fonctions pures, sans DOM).
 * Testé sous Node : tests/check_peers.js.
 *
 * Définition (docs/decisions.qmd, 2026-10-15) : k plus proches voisins sur les
 * critères actifs. Chaque critère est transformé (ax.tf : log1p pour les montants
 * asymétriques), puis centré-réduit sur toutes les communes qui l'ont. Distance =
 * racine de la moyenne des carrés des écarts réduits (comparable quel que soit le
 * nombre de critères). Filtres durs facultatifs : même département, même classe de
 * densité (si trop peu de communes dans la classe, complété hors classe : relaxed).
 * Déterministe : tri par (distance, position dans l'index).
 * ============================================================ */
(function (root) {
  "use strict";
  var tf = function (tfk, v) { return tfk === "log" ? Math.log1p(Math.max(0, v)) : v; };

  /** cols: {champ: tableau}, tfs: {champ: "lin"|"log"|"cat"} -> {champ: {m, sd}} */
  function stats(cols, tfs) {
    var out = {};
    Object.keys(cols).forEach(function (f) {
      var c = cols[f], n = 0, s = 0, s2 = 0, i, t;
      for (i = 0; i < c.length; i++) if (c[i] != null && isFinite(c[i])) { t = tf(tfs[f], c[i]); n++; s += t; s2 += t * t; }
      var m = n ? s / n : 0, v = n > 1 ? (s2 - n * m * m) / (n - 1) : 1;
      out[f] = { m: m, sd: Math.sqrt(Math.max(v, 1e-12)) };
    });
    return out;
  }

  /**
   * o = {cols, tfs, st, target (indice), active [champs], k, sameDept, sameClass, dept[], dens[]}
   * -> {peers:[{i, d}], nCand, relaxed, short, wide, crossClass}
   */
  function select(o) {
    var act = o.active.filter(function (f) { var v = o.cols[f][o.target]; return v != null && isFinite(v); });
    var n = o.cols[act[0] || Object.keys(o.cols)[0]].length;
    var zt = act.map(function (f) { return (tf(o.tfs[f], o.cols[f][o.target]) - o.st[f].m) / o.st[f].sd; });
    var inC = [], outC = [], i, j, s, v;
    for (i = 0; i < n; i++) {
      if (i === o.target) continue;
      if (o.sameDept && o.dept[i] !== o.dept[o.target]) continue;
      s = 0;
      for (j = 0; j < act.length; j++) {
        v = o.cols[act[j]][i];
        if (v == null || !isFinite(v)) { s = -1; break; }
        v = (tf(o.tfs[act[j]], v) - o.st[act[j]].m) / o.st[act[j]].sd - zt[j]; s += v * v;
      }
      if (s < 0) continue;
      var e = { i: i, d: act.length ? Math.sqrt(s / act.length) : 0 };
      if (o.sameClass && o.dens[i] !== o.dens[o.target]) outC.push(e); else inC.push(e);
    }
    var cmp = function (a, b) { return a.d - b.d || a.i - b.i; };
    inC.sort(cmp);
    var peers = inC.slice(0, o.k), relaxed = false;
    if (peers.length < o.k && outC.length) {              // repli hors classe : annoncé à l'utilisateur
      outC.sort(cmp); peers = peers.concat(outC.slice(0, o.k - peers.length)); relaxed = true;
    }
    var popF = "ctx_log_pop", wide = false, crossClass = false;
    peers.forEach(function (p) {
      if (o.cols[popF] && o.cols[popF][o.target] != null && o.cols[popF][p.i] != null &&
          Math.abs(o.cols[popF][p.i] - o.cols[popF][o.target]) > Math.log10(2)) wide = true;   // plus de 2x d'écart de taille
      if (o.dens && o.dens[p.i] !== o.dens[o.target]) crossClass = true;
    });
    return { peers: peers, nCand: inC.length + outC.length, relaxed: relaxed, short: peers.length < o.k, wide: wide && act.indexOf(popF) >= 0, crossClass: crossClass, used: act };
  }
  var API = { stats: stats, select: select };
  if (typeof module !== "undefined" && module.exports) module.exports = API; else root.PeersCore = API;
})(typeof window !== "undefined" ? window : globalThis);
