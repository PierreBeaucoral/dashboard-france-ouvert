/* ============================================================
 * theme.js : bascule clair / sombre.
 * Ordre de priorité : choix de l'utilisateur (localStorage) > préférence système
 * (prefers-color-scheme, gérée en CSS). Le choix est porté par l'attribut
 * data-theme de <html> ; styles/custom.scss définit les deux jeux de variables.
 * Émet l'évènement "themechange" (les graphiques SVG dessinés en JS se redessinent).
 * ============================================================ */
(function () {
  "use strict";
  var KEY = "fo-theme", root = document.documentElement;
  function stored() { try { return localStorage.getItem(KEY); } catch (e) { return null; } }
  function save(v) { try { localStorage.setItem(KEY, v); } catch (e) { /* mode privé : ignoré */ } }
  function systemDark() { return !!(window.matchMedia && matchMedia("(prefers-color-scheme: dark)").matches); }
  function current() { return root.getAttribute("data-theme") || (systemDark() ? "dark" : "light"); }

  var s = stored();
  if (s === "dark" || s === "light") root.setAttribute("data-theme", s);

  function label(btn) {
    var dark = current() === "dark";
    btn.setAttribute("aria-pressed", dark ? "true" : "false");
    btn.textContent = dark ? "Mode clair" : "Mode sombre";
    btn.title = dark ? "Passer au thème clair" : "Passer au thème sombre";
  }
  function mount() {
    if (document.querySelector(".theme-toggle")) return;
    var host = document.querySelector(".navbar .navbar-collapse") || document.querySelector(".navbar .container-fluid") || document.querySelector(".navbar");
    if (!host) return;
    var b = document.createElement("button");
    b.type = "button"; b.className = "theme-toggle";
    label(b);
    b.addEventListener("click", function () {
      var next = current() === "dark" ? "light" : "dark";
      root.setAttribute("data-theme", next); save(next); label(b);
      window.dispatchEvent(new Event("themechange"));
    });
    host.appendChild(b);
    if (window.matchMedia) {
      var mq = matchMedia("(prefers-color-scheme: dark)"), on = function () { label(b); window.dispatchEvent(new Event("themechange")); };
      if (mq.addEventListener) mq.addEventListener("change", on);
    }
  }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", mount); else mount();
})();
