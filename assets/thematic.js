/* ============================================================
 * thematic.js : page thématique pilotée par une configuration (JSON).
 *
 * Une page = une carte (CMap) + des boutons radio (un par indicateur) + un
 * panneau commune. Ce module remplace les ~100 lignes de JS recopiées dans
 * chaque page : la page ne fournit que des DONNÉES (champs, sections du panneau).
 *
 *   Thematic.setup({
 *     prefix, panelFn, data: "nomVarGlobale", pals: {seq: [...], div: [...]},
 *     fields: { <id>: {key, title, unit, digits, pal, scale|breaks, cats, sub, note,
 *                      na, badge, levels, offset} },
 *     panel:  { empty, sections: [{h, def, show?, rows: [{l, k, d, u}]}] } })
 *   Thematic.attach(map, prefix)   appelé par la page dans cmap_on_render()
 *
 * Dépend de window.CMap (assets/cmap.js). Écrit #<code> dans l'URL (lien partageable)
 * et rouvre la commune au chargement.
 * ============================================================ */
(function () {
  "use strict";
  var T = window.Thematic = { cfg: {} };
  var idx = null;

  function index() {
    return CMap.data.index().then(function (d) {
      if (!idx) {
        idx = {};
        d.c.forEach(function (c, i) {
          var dp = d.depts[d.d[i]];
          idx[c] = { n: d.n[i], dept: dp ? dp.nom + " (" + dp.code + ")" : "" };
        });
      }
      return idx;
    });
  }
  function num(x, d, u) {
    if (x == null) return null;
    if (d === -3) return CMap.esc(x);                 // texte
    if (d === -2) return x ? "Oui" : "Non";           // indicateur 0/1
    if (!isFinite(x)) return null;
    if (d === -1) return String(x);                   // brut (année)
    return Number(x).toLocaleString("fr-FR", { minimumFractionDigits: d || 0, maximumFractionDigits: d || 0 }) +
      (u ? " " + u : "");
  }

  T.setup = function (cfg) {
    T.cfg[cfg.prefix] = cfg;
    var data = function () { return window[cfg.data] || {}; };

    function recolor(id) {
      var f = cfg.fields[id], off = f.offset || 0;
      var o = { palette: cfg.pals[f.pal],
        get: function (c) { var r = data()[c], v = r ? r[f.key] : null; return v == null ? null : v + off; },
        legend: { title: f.title, sub: f.sub, note: f.note, unit: f.unit || "", digits: f.digits || 0,
                  cats: f.cats, naLabel: f.na || "Non publié" } };
      if (f.levels) { o.levels = f.levels; o.tip = function (c, v) { return v == null ? "Non publié" : f.levels[Math.round(v) - 1]; }; }
      else if (f.breaks) o.breaks = f.breaks; else o.scale = f.scale || "quantile";
      CMap.recolor(o);
      var b = document.getElementById(cfg.prefix + "-badge");
      if (b) b.innerHTML = f.badge ? '<span class="cp-badge cp-badge-imp">' + f.badge + "</span>" : "";
    }
    T.cfg[cfg.prefix].recolor = recolor;

    function render(code) {
      var el = document.getElementById(cfg.prefix + "-content");
      if (!el) return;
      index().then(function (ix) {
        var m = ix[code] || { n: code, dept: "" }, d = data()[code];
        var h = '<div class="cp-header"><h3>' + CMap.esc(m.n) + '</h3><div class="cp-meta">' +
          CMap.esc(m.dept) + " · " + code + "</div></div>";
        if (!d) { el.innerHTML = h + '<div class="dvf-empty">' + (cfg.panel.empty || "Pas de donnée publiée.") + "</div>"; return; }
        cfg.panel.sections.forEach(function (s) {
          if (s.show && d[s.show] == null) return;
          var rows = (s.rows || []).map(function (r) {
            var v = num(d[r.k], r.d, r.u);
            return '<div class="cp-kv"><span>' + r.l + '</span><span class="cp-num">' + (v == null ? (r.na || "—") : v) + "</span></div>";
          }).join("");
          h += '<div class="cp-section"><h4>' + s.h + "</h4>" + (s.def ? '<p class="cp-def">' + s.def + "</p>" : "") +
            (rows ? '<div class="cp-kvs">' + rows + "</div>" : "") + "</div>";
        });
        el.innerHTML = h;
      });
    }
    window[cfg.panelFn] = function (code) {
      if (code) { try { history.replaceState(null, "", "#" + code); } catch (e) {} }
      render(code);
    };
  };

  T.attach = function (map, prefix) {
    var cfg = T.cfg[prefix], first = cfg.first;
    window[prefix + "Map"] = map;
    CMap.pick(map, function (code) { window[cfg.panelFn](code); });
    CMap.onRadios(prefix + "-ind", function (v) { cfg.recolor(v); });
    var f = cfg.fields[first], lg = map.getContainer().querySelector(".cmap-lgd");
    if (f.cats && lg) CMap.drawLegend(lg, { title: f.title, sub: f.sub, note: f.note,
      palette: cfg.pals[f.pal], cats: f.cats, naLabel: f.na || "Non publié" });
    var b = document.getElementById(prefix + "-badge");
    if (b && f.badge) b.innerHTML = '<span class="cp-badge cp-badge-imp">' + f.badge + "</span>";
    // Ouverture depuis #<code>
    var m = (location.hash || "").match(/^#((?:2[ab]|\d{2})\d{3})$/i);
    if (m) map.once("cmap:ready", function () {
      var c = m[1].toUpperCase(), l = map._cmapLayers && map._cmapLayers[c];
      render(c);
      if (l) { CMap.select(map, c, false); map._userMoved = true; map.fitBounds(l.getBounds().pad(4), { maxZoom: 10, animate: false }); }
    });
  };
})();
