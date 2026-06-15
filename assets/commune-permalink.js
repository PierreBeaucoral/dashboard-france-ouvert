/* ============================================================
 * commune-permalink.js — partage d'URL par commune (global, zéro édition page)
 *
 * Inclus en fin de body sur toutes les pages via _quarto.yml.
 *
 * Deux comportements :
 *   1. LECTURE  : au chargement, si l'URL contient #<codeINSEE> (ex. #69123),
 *      ouvre le panneau de détail de cette commune sur la page courante.
 *   2. ÉCRITURE : enrobe la fonction de mise à jour du panneau de la page
 *      pour qu'un clic sur une commune écrive #<code> dans l'URL → l'utilisateur
 *      peut copier le lien et le partager (il rouvrira la même commune).
 *
 * Marche sans connaître la page : on détecte dynamiquement la fonction
 * `update*Panel` exposée par la page (chaque thématique a la sienne).
 * ============================================================ */
(function () {
  "use strict";

  // Fonctions panel connues, par page thématique.
  var PANEL_FNS = [
    "updateSecPanel", "updateRevPanel", "updateFinPanel", "updateSantePanel",
    "updateDemoPanel", "updateCarbPanel", "updateMobPanel", "updateBioPanel",
    "updateDvfPanel", "updateAirPanel", "updateMunPanel", "updateCommunePanel"
  ];

  // Code INSEE : 5 caractères, dépt 01-95 / 2A-2B / 97x, ex. 69123, 2A004, 97411.
  function codeFromHash() {
    var m = (location.hash || "").match(/^#((?:2[ab]|\d{2})\d{3})$/i);
    return m ? m[1].toUpperCase() : null;
  }

  function panelFnName() {
    for (var i = 0; i < PANEL_FNS.length; i++) {
      if (typeof window[PANEL_FNS[i]] === "function") return PANEL_FNS[i];
    }
    return null;
  }

  // ÉCRITURE : enrobe la fn panel pour qu'elle écrive le hash à chaque appel.
  function wrapPanelFn() {
    var name = panelFnName();
    if (!name || window["__permalinkWrapped_" + name]) return;
    var orig = window[name];
    window[name] = function (code) {
      if (code) {
        try { history.replaceState(null, "", "#" + code); } catch (e) {}
      }
      return orig.apply(this, arguments);
    };
    window["__permalinkWrapped_" + name] = true;
  }

  // LECTURE : ouvre la commune du hash (si présente).
  function openFromHash() {
    var code = codeFromHash();
    if (!code) return;
    var name = panelFnName();
    if (!name) return;
    try { window[name](code); } catch (e) {}
  }

  function boot() {
    // Laisse le temps aux scripts inline de la page + au widget leaflet de
    // définir les fonctions panel et de charger les données.
    setTimeout(function () { wrapPanelFn(); openFromHash(); }, 1200);
    // Réessaie une fois (cartes lourdes : layers ajoutés tardivement).
    setTimeout(function () { wrapPanelFn(); openFromHash(); }, 3000);
  }

  if (document.readyState === "complete") boot();
  else window.addEventListener("load", boot);

  // Navigation manuelle du hash (coller un lien dans la barre puis Entrée).
  window.addEventListener("hashchange", openFromHash);
})();
