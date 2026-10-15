// ============================================================
// tests/check_constellation.js : contrôles de la page Constellation.
// Lancer depuis la racine : node tests/check_constellation.js
//  1. Dorling : toutes les communes à géométrie sont placées, aucun chevauchement
//     (tolérance 1e-6 u), aucun cercle hors du cadre, encarts respectés ;
//  2. brosse : inRange renvoie exactement les communes dans [lo, hi] (bornes incluses) ;
//  3. départements : médiane pondérée par la population (cas à la main) ;
//  4. Spearman : valeurs de référence, invariance monotone.
// ============================================================
const assert = require("assert");
const fs = require("fs");
const C = require("../assets/constellation-core.js");
const near = (a, b, t) => assert.ok(Math.abs(a - b) <= (t || 1e-9), a + " vs " + b);

// 1. Dorling (fichiers produits par R/build_web_assets.R::build_constellation)
const geo = JSON.parse(fs.readFileSync("assets/data/constellation.json"));
const dor = JSON.parse(fs.readFileSync("assets/data/constellation-dor.json"));
const pop = JSON.parse(fs.readFileSync("assets/data/col/ctx_pop_latest.json"));
const n = geo.gx.length;
assert.strictEqual(dor.dx.length, n);
let placed = 0, unplacedWithGeo = 0;
for (let i = 0; i < n; i++) {
  const g = geo.gx[i] != null, d = dor.dx[i] != null;
  assert.strictEqual(g, d, "geo et Dorling placent les mêmes communes (indice " + i + ")");
  if (g) placed++;
}
assert.ok(placed > 34800, "au moins 34 800 communes placées : " + placed);
const R = (i) => Math.max(geo.k * Math.sqrt(pop[i] > 0 ? pop[i] : 0), geo.rmin);
// grille spatiale pour chercher les chevauchements
const cs = 12, grid = new Map();
const key = (a, b) => a * 100003 + b;
let maxr = 0, bad = 0;
for (let i = 0; i < n; i++) if (dor.dx[i] != null) maxr = Math.max(maxr, R(i));
const ids = [];
for (let i = 0; i < n; i++) if (dor.dx[i] != null) {
  ids.push(i);
  const k = key(Math.floor(dor.dx[i] / cs), Math.floor(dor.dy[i] / cs));
  (grid.get(k) || grid.set(k, []).get(k)).push(i);
  assert.ok(dor.dx[i] - R(i) >= -0.2 && dor.dx[i] + R(i) <= geo.w + 0.2 && dor.dy[i] - R(i) >= -0.2 && dor.dy[i] + R(i) <= geo.h + 0.2, "cercle hors du cadre : " + i);
}
const reach = Math.ceil((maxr * 2) / cs);
for (const i of ids) {
  const cx = Math.floor(dor.dx[i] / cs), cy = Math.floor(dor.dy[i] / cs), ri = R(i);
  for (let a = cx - reach; a <= cx + reach; a++) for (let b = cy - reach; b <= cy + reach; b++) {
    for (const j of grid.get(key(a, b)) || []) {
      if (j <= i) continue;
      const dd = Math.hypot(dor.dx[i] - dor.dx[j], dor.dy[i] - dor.dy[j]);
      // coordonnées arrondies à 0,1 u : tolérance 0,15 u cumulée sur les deux centres
      if (dd < ri + R(j) - 0.15) bad++;
    }
  }
}
assert.strictEqual(bad, 0, bad + " paires de cercles se chevauchent (> 0,15 u)");
// les communes d'outre-mer restent dans leur encart (en vue géographique comme en vue population)
const idx = JSON.parse(fs.readFileSync("assets/data/communes.json"));
const inBox = (x, y, b) => x >= b.x - 0.2 && x <= b.x + b.w + 0.2 && y >= b.y - 0.2 && y <= b.y + b.h + 0.2;
let drom = 0;
for (let i = 0; i < n; i++) if (/^97/.test(idx.c[i]) && geo.gx[i] != null) {
  const b = geo.boxes.find((q) => q.code === idx.depts[idx.d[i]].code);
  assert.ok(b, "encart manquant pour " + idx.c[i]);
  assert.ok(inBox(geo.gx[i], geo.gy[i], b) && inBox(dor.dx[i], dor.dy[i], b), "hors encart : " + idx.c[i]);
  drom++;
}
assert.ok(drom > 100, "communes d'outre-mer placées : " + drom);
console.log("Dorling : " + placed + " communes placées sur " + n + ", 0 chevauchement, " + drom + " en encarts OK");

// 2. Brosse : exactement les communes de [lo, hi]
{
  const v = [1, 2, 3, null, NaN, 5, 8, 8.5, 10, -1];
  const m = C.inRange(v, 2, 8);
  assert.deepStrictEqual(Array.from(m), [0, 1, 1, 0, 0, 1, 1, 0, 0, 0]);
  const col = JSON.parse(fs.readFileSync("assets/data/col/revenu_median.json"));
  const lo = 20000, hi = 22000, mm = C.inRange(col, lo, hi);
  let exp = 0; col.forEach((x) => { if (x != null && x >= lo && x <= hi) exp++; });
  assert.strictEqual(Array.from(mm).reduce((a, b) => a + b, 0), exp);
  console.log("Brosse : inRange exact (" + exp + " communes entre " + lo + " et " + hi + " EUR) OK");
}

// 3. Départements : médiane pondérée par la population
{
  const v = [1, 2, 3, 100, 200], p = [1, 1, 10, 5, 1], d = [0, 0, 0, 1, 1];
  const a = C.deptAggregate(v, p, d, 3);
  near(a.v[0], 3);                       // poids 1,1,10 : la médiane pondérée est 3 (médiane simple : 2)
  near(a.v[1], 100);                     // poids 5,1 : 100
  assert.ok(isNaN(a.v[2]));              // département vide
  near(a.pop[0], 12); near(a.pop[1], 6);
  assert.strictEqual(C.weightedMedian([1, 2, 3], [0, 0, 0]), 2);   // poids nuls : médiane simple
  console.log("Départements : médiane pondérée par la population OK");
}

// 4. Spearman
{
  const x = [1, 2, 3, 4, 5, 6, 7, 8], y = x.map((t) => Math.exp(t));
  near(C.spearman(x, y).rho, 1);
  near(C.spearman(x, y.map((t) => -t)).rho, -1);
  const s = C.spearman([1, 2, 3, 4, 5], [2, 1, 4, 3, 5]);
  near(s.rho, 0.8); assert.strictEqual(s.n, 5);
  assert.strictEqual(C.spearman([1, null, 3, 4], [2, 5, null, 8]).n, 2 + 0);
  console.log("Spearman OK");
}
console.log("check_constellation : tout est OK");
