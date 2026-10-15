/* ============================================================
 * classements.js : top 20 / bottom 20 des communes pour un indicateur.
 * Seule la colonne de l'indicateur affiché est téléchargée (CMap.data.col),
 * puis mise en cache. Configuration injectée par pages/classements.qmd :
 *   window.RK_IND = [{field, label, unit, dig}]
 * ============================================================ */
(function () {
  "use strict";
  var IND = window.RK_IND || [], idx = null, esc = function (s) { return CMap.esc(s); };

  function fmtVal(v, m) {
    if (v == null) return "—";
    return CMap.fmt(v, m.dig) + (m.unit ? " " + m.unit : "");
  }

  function render() {
    var k = +document.getElementById("rk-ind").value, m = IND[k];
    var dept = document.getElementById("rk-dept").value, strate = document.getElementById("rk-strate").value;
    var order = document.querySelector("input[name=rk-order]:checked").value;
    var box = document.getElementById("rk-table");
    box.innerHTML = '<div class="js-loading">Chargement de l’indicateur…</div>';
    CMap.data.col(m.field).then(function (col) {
      var rows = [], i, n = idx.c.length;
      for (i = 0; i < n; i++) {
        if (col[i] == null) continue;
        if (dept !== "*" && String(idx.d[i]) !== dept) continue;
        if (strate !== "*" && String(idx.s[i]) !== strate) continue;
        rows.push(i);
      }
      rows.sort(function (a, b) { return order === "top" ? col[b] - col[a] : col[a] - col[b]; });
      var top = rows.slice(0, 20), maxV = top.length ? Math.max.apply(null, top.map(function (r) { return Math.abs(col[r]); })) : 1;
      var html = '<p class="rk-count">' + CMap.fmt(rows.length, 0) + " communes correspondent au filtre. " +
                 (order === "top" ? "Valeurs les plus élevées" : "Valeurs les plus faibles") + " pour <strong>" + esc(m.label) + "</strong>.</p>";
      if (!top.length) {
        box.innerHTML = html + '<p class="js-loading">Aucune commune ne correspond à ce filtre (donnée non publiée pour cette taille de commune ou ce département).</p>';
        return;
      }
      html += '<table class="rk-table"><tbody>';
      top.forEach(function (r, j) {
        var w = maxV > 0 ? Math.max(2, Math.abs(col[r]) / maxV * 100) : 0, d = idx.depts[idx.d[r]];
        html += "<tr><td class=\"rk-rank\">" + (j + 1) + '</td><td><a href="profil.html#' + idx.c[r] + '">' + esc(idx.n[r]) + '</a> <span class="cp-meta">(' +
          esc(d ? d.code : "") + ')</span></td><td class="rk-val"><span class="rk-bar"><i style="width:' + w.toFixed(0) + '%"></i></span><span class="num">' +
          fmtVal(col[r], m) + "</span></td></tr>";
      });
      box.innerHTML = html + "</tbody></table>";
    }).catch(function (e) { console.error(e); box.innerHTML = '<p class="js-loading">Données indisponibles.</p>'; });
  }

  function boot() {
    var sel = document.getElementById("rk-ind");
    if (!sel) return;
    var controls = document.querySelectorAll("#rk-ind, #rk-dept, #rk-strate, input[name=rk-order]");
    controls.forEach(function (c) { c.disabled = true; });
    document.getElementById("rk-table").innerHTML = '<div class="js-loading">Chargement des communes…</div>';
    CMap.data.index().then(function (ix) {
      idx = ix;
      IND.forEach(function (m, i) { var o = document.createElement("option"); o.value = i; o.textContent = m.label; sel.appendChild(o); });
      var ds = document.getElementById("rk-dept");
      ix.depts.forEach(function (d, i) { var o = document.createElement("option"); o.value = i; o.textContent = d.code + " · " + d.nom; ds.appendChild(o); });
      var ss = document.getElementById("rk-strate");
      ix.strates.forEach(function (s, i) { var o = document.createElement("option"); o.value = i; o.textContent = s; ss.appendChild(o); });
      controls.forEach(function (c) { c.disabled = false; c.addEventListener("change", render); });
      render();
    }).catch(function (e) { console.error(e); document.getElementById("rk-table").innerHTML = '<p class="js-loading">Données indisponibles.</p>'; });
  }
  if (document.readyState === "complete") boot(); else window.addEventListener("load", boot);
})();
