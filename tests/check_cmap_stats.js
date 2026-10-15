// ============================================================
// tests/check_cmap_stats.js : vérifie les fonctions pures de assets/cmap-stats.js.
// Lancer depuis la racine : node tests/check_cmap_stats.js
//  - parts de communes / de population par classe : somme = 100 % (non publié inclus) ;
//  - parité de makeBreaks avec R/map_helpers.R::cmap_breaks (valeurs de référence calculées sous R) ;
//  - quantiles pondérés, rang centile.
// ============================================================
const assert = require("assert");
const S = require("../assets/cmap-stats.js");
const near = (a, b, t) => assert.ok(Math.abs(a - b) <= (t || 1e-9), a + " vs " + b);

// 1. Parts : somme = 100 avec et sans non publiés
const cls = [1, 2, 3, 0, 5, 5, 4, 0, 2, 1], pops = [10, 20, 30, 5, 100, 50, null, 7, 3, 2];
const sh = S.classShares(cls, pops, 5);
near(sh.pc.reduce((a, b) => a + b, 0) + sh.naC, 100);
near(sh.pp.reduce((a, b) => a + b, 0) + sh.naP, 100);
near(sh.naC, 20);                               // 2 communes sur 10 non publiées
near(sh.naP, 100 * 12 / 227);                   // population 5 + 7 sur 227

// 2. makeBreaks : parité avec R (set.seed-free : vecteur déterministe)
const v = Array.from({ length: 1000 }, (_, i) => (i < 700 ? 0 : (i - 699) * 0.37));
const b = S.makeBreaks(v, "quantile");
assert.ok(b.zero && b.breaks[0] === 0 && b.breaks.length === 6);
// Référence : Rscript -e 'source("R/theme.R"); source("R/map_helpers.R"); print(cmap_breaks(c(rep(0,700),(1:300)*0.37),"quantile")$breaks)'
const ref = [0, 0.37, 28.0275, 55.685, 83.3425, 111];   // valeurs R (cmap_breaks, type 7 sur les positifs)
b.breaks.forEach((x, i) => near(x, ref[i], 1e-6));
const cl2 = v.map(x => S.classOf(x, b.breaks));
assert.ok(cl2.every((c, i) => (v[i] === 0) === (c === 1)));
const pos = cl2.filter((c, i) => v[i] > 0).reduce((m, c) => (m[c] = (m[c] || 0) + 1, m), {});
[2, 3, 4, 5].forEach(c => assert.ok(Math.abs(pos[c] - 75) <= 1, "classe " + c + " : " + pos[c]));

// 3. Quantiles pondérés et rang centile
const wq = S.weightedQuantiles([1, 2, 3, 4], [1, 1, 1, 97], [0.5]);
assert.strictEqual(wq[0], 4);                   // 97 % de la masse est sur la valeur 4
near(S.percentile([1, 2, 2, 3], 2), 50);        // ex æquo : rang moyen
near(S.percentile([1, 2, 3, 4], 0), 0);

// 4. Villes PLM comblées (audit M2) : la ville sans valeur propre prend la moyenne de ses arrondissements
const g = S.withPlmFallback(c => ({ "75101": 10, "75102": 20, "75103": null, "X": 5 })[c], { "75056": ["75101", "75102", "75103"] });
near(g("75056"), 15);                           // arrondissement sans valeur ignoré
assert.strictEqual(g("X"), 5);
assert.ok(g("99999") == null);                  // commune sans arrondissement : inchangée
const gc = S.withPlmFallback(c => ({ "75101": "Grands centres urbains" })[c], { "75056": ["75101"] });
assert.strictEqual(gc("75056"), "Grands centres urbains");

// 5. Libellés de densité (audit C1) : le repli de cmap.js suit EXACTEMENT l'ordre des rangs
// de scripts/10_ (1 rural non périurbain ... 6 grands centres) et, si l'index généré existe,
// l'ordre des données (assets/data/communes.json::dl, lui-même issu de grille_densite.parquet).
const fs = require("fs");
const src = fs.readFileSync("assets/cmap.js", "utf8");
const ranked = JSON.parse(src.match(/var DENS_RANKED = (\[[^\]]*\])/)[1].replace(/\s+/g, " "));
assert.deepStrictEqual(ranked, ["Rural non périurbain", "Rural périurbain", "Ceintures urbaines",
  "Petites villes", "Centres urbains intermédiaires", "Grands centres urbains"]);
if (fs.existsSync("assets/data/communes.json")) {
  const dl = JSON.parse(fs.readFileSync("assets/data/communes.json")).dl;
  assert.deepStrictEqual(dl, ranked, "ordre des libellés de densité : données != cmap.js");
}
console.log("check_cmap_stats.js : OK");
