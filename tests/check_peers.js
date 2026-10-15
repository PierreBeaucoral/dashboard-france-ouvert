// tests/check_peers.js : vérifie la sélection des semblables (assets/peers-core.js) sur les vraies colonnes.
// Lancer depuis la racine : node tests/check_peers.js   (après Rscript R/build_web_assets.R)
const assert = require("assert"), P = require("../assets/peers-core.js");
const ix = require("../assets/data/communes.json"), col = f => require("../assets/data/col/" + f + ".json");
const F = ["ctx_log_pop", "densite_rang", "revenu_median", "dvf_prix_mai"];
const cols = {}; F.forEach(f => cols[f] = col(f));
const tfs = { ctx_log_pop: "lin", densite_rang: "cat", revenu_median: "lin", dvf_prix_mai: "log" };
const st = P.stats(cols, tfs), at = c => ix.c.indexOf(c);
const base = { cols, tfs, st, active: ["ctx_log_pop", "densite_rang"], k: 15, sameDept: false, sameClass: false, dept: ix.d, dens: cols.densite_rang };
const run = (code, o) => P.select(Object.assign({}, base, { target: at(code) }, o || {}));

// 1. cible exclue, k respecté, déterministe
const a = run("63113"), b = run("63113");
assert.ok(a.peers.length === 15 && !a.peers.some(p => p.i === at("63113")), "cible exclue, k=15");
assert.deepStrictEqual(a.peers, b.peers, "déterministe");
assert.ok(a.peers.every((p, j) => j === 0 || a.peers[j - 1].d <= p.d), "trié par distance");
console.log("Clermont-Ferrand :", a.peers.map(p => ix.n[p.i]).join(", "));
// 2. k variable
assert.strictEqual(run("63113", { k: 5 }).peers.length, 5);
// 3. filtres durs
const d = run("63113", { sameDept: true, k: 10 });
assert.ok(d.peers.every(p => ix.d[p.i] === ix.d[at("63113")]), "même département");
const c = run("63113", { sameClass: true });
assert.ok(c.peers.every(p => cols.densite_rang[p.i] === 6) && !c.relaxed, "même classe");
// 4. critère actif : les valeurs manquantes sont exclues
const r = run("63113", { active: ["ctx_log_pop", "revenu_median", "dvf_prix_mai"] });
assert.ok(r.peers.every(p => cols.revenu_median[p.i] != null && cols.dvf_prix_mai[p.i] != null), "valeurs présentes");
// 5. repli : département + classe, trop peu de communes de classe 6 dans le 63 -> complété hors classe, annoncé
const f = run("63113", { sameDept: true, sameClass: true, k: 10 });
assert.ok(f.relaxed && f.peers.length === 10 && f.peers.every(p => ix.d[p.i] === ix.d[at("63113")]), "repli hors classe, même département");
// 6. Paris : k voisins, groupe élargi annoncé ; seul de son département -> 0 candidat
const p = run("75056");
assert.ok(p.peers.length === 15 && p.wide, "Paris : élargi"); console.log("Paris :", p.peers.map(q => ix.n[q.i]).join(", "));
const ps = run("75056", { sameDept: true });
assert.ok(ps.peers.length === 0 && ps.short, "Paris + même dépt : aucun");
console.log("check_peers : OK");
