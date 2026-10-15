/* ============================================================
 * elections-views.js : vues alternatives de la carte Législatives 2024.
 *   famille (défaut)  carte d'origine, restaurée telle quelle
 *   voters            cercles proportionnels aux inscrits (couleur = famille gagnante)
 *   abstention...     choroplèthes de participation (prés. 2022, européennes 2024,
 *                     municipales 2026, évolution 2002 -> 2022)
 * Les valeurs viennent de window.communeLookup (fichier colonnes chargé au premier clic).
 * Dépend de CMap (cmap.js), Leaflet et de window.elecLoad / fammilleColors (page).
 * ============================================================ */
(function () {
  "use strict";
  var V = window.ElecViews = {};
  var saved = false, circles = null, cur = "famille";
  var FAM = ["NFP", "ENS", "RN", "LR", "Divers"];
  var FAMLAB = ["NFP", "Ensemble", "RN", "LR", "Divers"];

  function eachLayer(fn) {
    CMap.maps.forEach(function (m) {
      if (!m._cmapLayers) return;
      Object.keys(m._cmapLayers).forEach(function (c) { fn(m, c, m._cmapLayers[c]); });
    });
  }
  function save() {
    if (saved) return;
    saved = true;
    eachLayer(function (m, c, l) {
      l._o = { fc: l.options.fillColor, fo: l.options.fillOpacity, na: l.options.cmapNA, pt: l.options.cmapPartial, tip: l._tip };
    });
    CMap.main._lgd0 = CMap.main._cmapLegend;
  }
  function restore() {
    if (circles) { CMap.main.removeLayer(circles); circles = null; }
    if (!saved) return;
    eachLayer(function (m, c, l) {
      l.options.cmapNA = l._o.na; l.options.cmapPartial = l._o.pt; l._tip = l._o.tip;
      l.setStyle({ fillColor: l._o.fc, fillOpacity: l._o.fo });
    });
    var slot = CMap.main.getContainer().querySelector(".cmap-lgd");
    if (slot && CMap.main._lgd0) CMap.drawLegend(slot, CMap.main._lgd0);
  }

  function voters(lk) {
    var map = CMap.main, pal = window.fammilleColors, groups = {}, L_ = window.L;
    eachLayer(function (m, c, l) {                       // polygones en filigrane
      l.options.cmapNA = false;
      l.setStyle({ fillColor: "#C9CCCF", fillOpacity: 0.28 });
      if (!m._cmapInset) l._tip = (lk[c] ? lk[c].nom_commune + " : " + CMap.fmt(lk[c].inscrits, 0) + " inscrits" : "");
    });
    var items = [];
    Object.keys(map._cmapLayers).forEach(function (c) {
      var d = lk[c]; if (!d || !d.inscrits) return;
      var city = /^751\d\d$/.test(c) ? "75056" : /^693\d\d$/.test(c) ? "69123" : /^132\d\d$/.test(c) ? "13055" : null;
      var ctr = map._cmapLayers[c].getBounds().getCenter();
      if (city) {                                        // une seule bulle pour la ville PLM
        var g = groups[city] || (groups[city] = { d: lk[city] || d, la: 0, ln: 0, n: 0 });
        g.la += ctr.lat; g.ln += ctr.lng; g.n++;
      } else items.push({ d: d, ll: ctr });
    });
    Object.keys(groups).forEach(function (k) {
      var g = groups[k]; items.push({ d: g.d, ll: L_.latLng(g.la / g.n, g.ln / g.n) });
    });
    items.sort(function (a, b) { return b.d.inscrits - a.d.inscrits; });   // grands dessous, petits dessus
    circles = L_.layerGroup();
    items.forEach(function (it) {
      var r = 0.018 * Math.sqrt(it.d.inscrits);
      if (r < 0.7) return;
      var mg = it.d.marge_vainqueur == null ? 20 : it.d.marge_vainqueur;
      L_.circleMarker(it.ll, { radius: r, stroke: true, weight: 0.4, color: "#fff", opacity: 0.8,
        fillColor: pal[it.d.famille_vainqueur] || "#999", fillOpacity: Math.min(0.95, 0.5 + 0.012 * mg),
        interactive: false }).addTo(circles);
    });
    circles.addTo(map);
    var slot = map.getContainer().querySelector(".cmap-lgd");
    if (slot) CMap.drawLegend(slot, { title: "Un cercle par commune : surface ∝ inscrits",
      sub: "Couleur : famille gagnante (tour décisif de chaque circonscription)", palette: FAM.map(function (f) { return pal[f]; }),
      cats: FAMLAB, naLabel: "Pas de résultat" });
  }

  V.set = function (view) {
    cur = view;
    window.elecLoad().then(function (lk) {
      save(); restore();
      if (view === "famille") return;
      eachLayer(function (m, c, l) { l.options.cmapPartial = false; });   // hachure "partiel" : vue famille seulement
      if (view === "voters") return voters(lk);
      var f = V.fields[view];
      CMap.recolor({ get: function (c) { var d = lk[c]; return d ? d[f.col] : null; }, palette: V.pals[f.pal],
        breaks: f.breaks, scale: f.breaks ? null : "quantile",
        legend: { title: f.title, sub: f.sub, note: f.note, unit: f.unit, digits: f.digits, cats: f.cats,
                  naLabel: "Non publié" } });
    });
  };
  V.init = function (fields, pals) {
    V.fields = fields; V.pals = pals;
    CMap.onRadios("elec-view", function (v) { V.set(v); });
  };

  /* Synthèse nationale : part des communes vs part des inscrits, par famille gagnante. */
  V.summary = function (el, nat, pal) {
    var rows = FAM.map(function (f, i) {
      var s = nat[f]; if (!s) return "";
      return '<tr><td><span class="lgd-swatch cp-natl-sw" style="background:' + pal[f] + '"></span>' +
        FAMLAB[i] + '</td><td class="cp-num">' + CMap.fmt(s.pc, 1) + ' %</td><td class="cp-num">' + CMap.fmt(s.pi, 1) + " %</td></tr>";
    }).join("");
    el.innerHTML = '<div class="cp-section"><h4>Communes ou électeurs ?</h4><table class="cp-natl"><thead><tr><th></th>' +
      '<th title="Part des communes gagnées">communes</th><th title="Part des inscrits vivant dans ces communes">inscrits</th></tr></thead><tbody>' +
      rows + '</tbody></table><p class="cp-def">Famille gagnante (tour décisif de chaque circonscription, agrégé par bureau de vote), ' + CMap.fmt(nat.n, 0) +
      " communes de France entière ; " + CMap.fmt(nat.nmixte || 0, 0) + " d'entre elles, coupées entre des circonscriptions élues à des tours différents, mélangent des voix du 1er et du 2nd tour" +
      ((nat.npart || 0) > 0 ? " et " + CMap.fmt(nat.npart, 0) + " ne portent que sur une partie des inscrits (hachurées)" : "") + ". Une carte à l'aire donne le même poids à une commune de 50 habitants et à Marseille : " +
      "la vue « Électeurs » corrige ce biais.</p></div>";
  };
})();
