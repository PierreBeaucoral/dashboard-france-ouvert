/* ============================================================
 * semblables.js : « Les communes qui vous ressemblent ».
 * Sélection : assets/peers-core.js (k plus proches voisins, critères au choix).
 * Rendu : mêmes lignes que Profil (assets/portrait-kit.js, classes .pt-*).
 * Configuration injectée par pages/semblables.qmd : window.PEERS_CRIT.
 * URL : #63113&c=pop,dens,rev&k=15&f=dept,cls&p=69123 (défauts omis).
 * ============================================================ */
(function () {
  "use strict";
  var CRIT = window.PEERS_CRIT || [], K = window.PortraitKit, esc = function (s) { return CMap.esc(s); };
  var BYID = {}, BYF = {};
  CRIT.forEach(function (c) { BYID[c.id] = c; BYF[c.field] = c; });
  var DEF = CRIT.filter(function (c) { return c.def; }).map(function (c) { return c.id; });
  var DEFAULT_CODE = "63113", DEFAULT_K = 15;
  var reduce = window.matchMedia && matchMedia("(prefers-reduced-motion: reduce)").matches;

  var S = { idx: null, row: {}, cols: {}, dist: {}, st: null, names: [], target: null, pin: null,
            active: DEF.slice(), k: DEFAULT_K, sameDept: false, sameClass: false, res: null, ticks: {}, loading: {} };
  var $ = function (id) { return document.getElementById(id); };
  var tfs = function () { var o = {}; Object.keys(S.cols).forEach(function (f) { o[f] = BYF[f].tf; }); return o; };

  /* ---- Données : colonnes à la demande ---- */
  function need(fields) {
    return Promise.all(fields.map(function (f) {
      if (S.cols[f]) return null;
      return S.loading[f] || (S.loading[f] = CMap.data.col(f).then(function (c) {
        S.cols[f] = c; S.dist[f] = K.buildDist(BYF[f], c); S.st = null;
      }));
    }));
  }
  function stats() { if (!S.st) S.st = PeersCore.stats(S.cols, tfs()); return S.st; }

  /* ---- Hash ---- */
  function readHash() {
    var p = (location.hash || "").replace("#", "").split("&"), o = { code: (p[0] || "").toUpperCase() };
    p.slice(1).forEach(function (kv) { var a = kv.split("="); o[a[0]] = a[1]; });
    return o;
  }
  function writeHash() {
    var p = [S.target], same = S.active.length === DEF.length && DEF.every(function (d) { return S.active.indexOf(d) >= 0; });
    if (!same) p.push("c=" + S.active.join(","));
    if (S.k !== DEFAULT_K) p.push("k=" + S.k);
    var f = (S.sameDept ? ["dept"] : []).concat(S.sameClass ? ["cls"] : []);
    if (f.length) p.push("f=" + f.join(","));
    if (S.pin) p.push("p=" + S.pin);
    try { history.replaceState(null, "", "#" + p.join("&")); } catch (e) {}
  }

  /* ---- Formats ---- */
  function info(code) {
    var i = S.row[code]; if (i == null) return null;
    var d = S.idx.depts[S.idx.d[i]];
    return { code: code, i: i, nom: S.idx.n[i], dept: d ? d.nom + " (" + d.code + ")" : "" };
  }
  function compactPop(v) {
    var n = Math.pow(10, v);
    return n >= 1e6 ? CMap.fmt(n / 1e6, 1) + " M" : n >= 1e4 ? CMap.fmt(Math.round(n / 1e3), 0) + " k" : n >= 1e3 ? CMap.fmt(n / 1e3, 1) + " k" : CMap.fmt(Math.round(n), 0);
  }
  function val(c, v, compact) {
    if (c.id === "pop" && compact) return compactPop(v);
    return K.axVal(c, v);
  }
  function median(a) {
    a = a.filter(function (x) { return x != null; }).sort(function (x, y) { return x - y; });
    var m = a.length; return m ? (m % 2 ? a[(m - 1) / 2] : (a[m / 2 - 1] + a[m / 2]) / 2) : null;
  }
  function activeFields() {
    var t = S.row[S.target];
    return S.active.map(function (id) { return BYID[id]; }).filter(function (c) { var v = S.cols[c.field] && S.cols[c.field][t]; return v != null && isFinite(v); });
  }

  /* ---- Calcul ---- */
  function compute() {
    var act = activeFields().map(function (c) { return c.field; });
    if (!act.length) { S.res = { peers: [], nCand: 0, used: [], short: true }; return; }
    S.res = PeersCore.select({ cols: S.cols, tfs: tfs(), st: stats(), target: S.row[S.target], active: act, k: S.k,
      sameDept: S.sameDept, sameClass: S.sameClass, dept: S.idx.d, dens: S.cols.densite_rang });
  }

  /* ---- Rendu ---- */
  function renderChips() {
    var t = S.row[S.target], box = $("sm-chips");
    if (!box.firstChild) {
      box.innerHTML = CRIT.map(function (c) { return '<button type="button" class="sm-chip" data-id="' + c.id + '">' + esc(c.label.replace(/ [€%]$/, "")) + "</button>"; }).join("");
      box.addEventListener("click", function (e) {
        var b = e.target.closest("button"); if (!b || b.disabled) return;
        var id = b.getAttribute("data-id"), i = S.active.indexOf(id);
        if (i >= 0) { if (S.active.length === 1) return; S.active.splice(i, 1); } else S.active.push(id);
        refresh();
      });
    }
    CRIT.forEach(function (c) {
      var b = box.querySelector('[data-id="' + c.id + '"]'), col = S.cols[c.field];
      var na = col ? (col[t] == null) : false;
      b.disabled = na; b.title = na ? "Non publié pour cette commune : critère ignoré" : "";
      b.setAttribute("aria-pressed", S.active.indexOf(c.id) >= 0 && !na ? "true" : "false");
    });
  }

  function renderIdent() {
    var a = info(S.target), pop = S.cols.ctx_log_pop[a.i], dens = S.cols.densite_rang[a.i];
    $("sm-ident").innerHTML = '<h3 class="pt-name">' + esc(a.nom) + '</h3><div class="pt-dept">' + esc(a.dept) + " · " + a.code + "</div>" +
      '<div class="pt-facts"><span>' + (pop != null ? CMap.fmt(Math.round(Math.pow(10, pop)), 0) + " hab." : "population n.d.") + "</span><span>" +
      (dens ? esc(CMap.densLabels[Math.round(dens) - 1]) : "densité n.d.") + "</span></div>" +
      '<div class="pt-pinned">' + (S.pin ? '<span class="pt-pin" aria-hidden="true"></span>' + esc(info(S.pin).nom) + ' <a href="#" id="sm-unpin">retirer</a>' : "") + "</div>";
    var u = $("sm-unpin"); if (u) u.addEventListener("click", function (e) { e.preventDefault(); S.pin = null; paint(); });
  }

  function statusText() {
    var r = S.res, used = r.used || [], msg;
    if (!used.length) return "Aucun critère disponible pour cette commune : cochez-en un autre.";
    msg = r.peers.length + " semblables parmi " + CMap.fmt(r.nCand, 0) + " communes comparables sur " + used.length + " critère" + (used.length > 1 ? "s" : "") + ".";
    if (r.short && r.peers.length < S.k) msg += " Moins de " + S.k + " communes répondent aux filtres" + (r.peers.length ? "" : " : aucune") + ".";
    if (r.relaxed) msg += " Trop peu de communes dans la même classe de densité : complété avec des communes d'autres classes.";
    else if (r.wide) msg += " Peu de communes de taille comparable : le groupe s'élargit à des communes de taille différente (plus de 2 fois).";
    return msg;
  }

  // Phrase « pourquoi elle vous ressemble » : valeurs sur les critères actifs, à côté des vôtres (HTML déjà échappé).
  function whyLine(p) {
    var t = S.row[S.target];
    return (S.res.used || []).map(function (f) {
      var c = BYF[f], vp = S.cols[f][p.i], vt = S.cols[f][t];
      if (c.id === "dens") return c.short + " " + (vp === vt ? val(c, vp) + " (identique)" : val(c, vp) + " (vous " + val(c, vt) + ")");
      return c.short + " " + val(c, vp, true) + " (vous " + val(c, vt, true) + ")";
    }).join(" · ");
  }

  function renderList() {
    var box = $("sm-list"), r = S.res;
    box.innerHTML = r.peers.map(function (p) {
      var c = S.idx.c[p.i], d = S.idx.depts[S.idx.d[p.i]], pop = S.cols.ctx_log_pop[p.i];
      return '<button type="button" class="sm-peer" role="listitem" data-c="' + c + '" aria-pressed="' + (S.pin === c) + '">' +
        '<span class="sm-pn"><span>' + esc(S.idx.n[p.i]) + "</span><em>" + (d ? d.code + " · " : "") + (pop != null ? compactPop(pop) + " hab. · " : "") + "dist. " + CMap.fmt(p.d, 2) + "</em></span>" +
        '<span class="sm-why">' + whyLine(p) + "</span></button>";
    }).join("") || '<p class="pt-note">Aucune commune semblable avec ces critères et ces filtres.</p>';
  }

  function buildRows() {
    var host = $("sm-rows"), rows = CRIT.filter(function (c) { return c.row; });
    host.innerHTML = rows.map(function (c) {
      return '<div class="pt-row" data-f="' + c.field + '"><div class="pt-lab">' + esc(c.label) + '<span class="pt-src">' + esc(c.src) + "</span></div>" +
        '<div class="pt-plot"><span class="sm-dist"></span><i class="pt-mk pt-a" aria-hidden="true"></i><i class="pt-mk pt-b" aria-hidden="true"></i>' +
        '<div class="pt-axis"><span></span><span></span></div></div>' +
        '<div class="pt-val"><b class="pt-va"></b><span class="pt-vb"></span><span class="pt-vc"></span></div></div>';
    }).join("");
    host.setAttribute("data-built", "1");
  }
  function setMk(el, x) { if (x == null) { el.style.opacity = "0"; } else { el.style.opacity = "1"; el.style.left = x.toFixed(1) + "%"; } }

  function renderRows() {
    if (!$("sm-rows").getAttribute("data-built")) buildRows();
    var t = S.row[S.target], pin = S.pin ? S.row[S.pin] : null;
    CRIT.filter(function (c) { return c.row; }).forEach(function (c) {
      var row = document.querySelector('.pt-row[data-f="' + c.field + '"]'), col = S.cols[c.field];
      if (!row || !col) return;                                  // colonne pas encore arrivée : ligne vide, hauteur stable
      var d = S.dist[c.field], pl = row.querySelector(".pt-plot");
      if (!row.getAttribute("data-d")) {
        row.querySelector(".sm-dist").outerHTML = K.distSVG(d);
        var e = K.ends(c, d), sp = row.querySelectorAll(".pt-axis span"); sp[0].textContent = e[0]; sp[1].textContent = e[1];
        row.setAttribute("data-d", "1");
      }
      var vt = col[t], vals = S.res.peers.map(function (p) { return col[p.i]; });
      setMk(row.querySelector(".pt-a"), K.pos(c, d, vt));
      setMk(row.querySelector(".pt-b"), pin != null ? K.pos(c, d, col[pin]) : null);
      row.querySelector(".pt-va").innerHTML = val(c, vt) + '<span class="sm-yr">' + c.yr + "</span>";
      var med = median(vals);
      row.querySelector(".pt-vb").innerHTML = med != null ? "Semblables (méd.) " + val(c, med) : (S.res.peers.length ? "Semblables : n.d." : "");
      row.querySelector(".pt-vc").innerHTML = pin != null ? '<span class="pt-pin" aria-hidden="true"></span>' + val(c, col[pin]) : "";
      // traits des semblables : un élément par commune (clé = code), déplacé ou estompé avec transition
      var tk = S.ticks[c.field] = S.ticks[c.field] || {}, keep = {};
      S.res.peers.forEach(function (p) {
        var code = S.idx.c[p.i], x = K.pos(c, d, col[p.i]), el = tk[code];
        keep[code] = 1;
        if (x == null) { if (el) el.classList.remove("on"); return; }
        if (!el) { el = tk[code] = document.createElement("i"); el.className = "sm-tk"; el.setAttribute("data-c", code); el.style.left = x.toFixed(1) + "%"; pl.appendChild(el); void el.offsetWidth; }
        el.style.left = x.toFixed(1) + "%"; el.classList.add("on");
      });
      Object.keys(tk).forEach(function (code) {
        if (keep[code]) return;
        var el = tk[code]; delete tk[code]; el.classList.remove("on");
        if (reduce) el.remove(); else setTimeout(function () { el.remove(); }, 280);
      });
    });
  }

  function renderDetail() {
    var box = $("sm-detail"), t = S.row[S.target];
    if (!S.pin) {
      box.innerHTML = "<h4>Comment lire</h4><p>Cliquez une commune de la liste pour voir pourquoi elle vous ressemble, critère par critère, et la repérer sur chaque ligne. " +
        "Le groupe retient les <strong>" + S.k + "</strong> communes les plus proches de " + esc(info(S.target).nom) + " sur : " +
        esc((S.res.used || []).map(function (f) { return BYF[f].short.toLowerCase(); }).join(", ") || "—") + ".</p>";
      return;
    }
    var p = info(S.pin), hit = S.res.peers.filter(function (q) { return q.i === p.i; })[0], st = stats();
    var body = (S.res.used || []).map(function (f) {
      var c = BYF[f], a = S.cols[f][t], b = S.cols[f][p.i];
      var z = (K.tf(c, b) - K.tf(c, a)) / st[f].sd;
      return "<tr><td>" + esc(c.label) + '<span class="sm-yr">' + c.yr + "</span></td><td>" + val(c, a) + "</td><td>" + val(c, b) + "</td><td>" + (z >= 0 ? "+" : "−") + CMap.fmt(Math.abs(z), 2) + " σ</td></tr>";
    }).join("");
    box.innerHTML = "<h4>" + esc(p.nom) + ' <span class="cp-meta">' + esc(p.dept) + "</span></h4>" +
      "<p>" + (hit ? "Distance " + CMap.fmt(hit.d, 2) + " σ (moyenne quadratique des écarts réduits)." : "Commune hors du groupe actuel.") + "</p>" +
      "<table><tr><th>Critère</th><th>Vous</th><th>" + esc(p.nom) + "</th><th>Écart</th></tr>" + body + "</table>" +
      '<button type="button" class="sm-btn" id="sm-retarget">Prendre comme commune de référence</button>';
    $("sm-retarget").addEventListener("click", function () { setTarget(p.code); });
  }

  function paint() {
    renderChips(); renderIdent(); renderList(); renderRows(); renderDetail();
    $("sm-status").textContent = statusText();
    $("sm-kv").textContent = S.k;
    PortraitKit.locator($("sm-canvas"), { target: S.target, pin: S.pin, peers: S.res.peers.map(function (p) { return S.idx.c[p.i]; }) });
    writeHash();
  }
  function refresh() {
    var f = S.active.map(function (id) { return BYID[id].field; });
    need(f).then(function () { compute(); paint(); }).catch(function (e) { console.error(e); });
  }
  function setTarget(code) {
    if (S.row[code] == null) return;
    S.target = code; S.pin = null; refresh();
  }

  /* ---- Entrées ---- */
  function wireSearch() {
    var input = $("sm-search"), list = $("sm-suggest");
    input.addEventListener("input", function () {
      var q = input.value.trim().toLowerCase(), m = [], i;
      if (q.length < 2) { list.hidden = true; return; }
      for (i = 0; i < S.names.length && m.length < 8; i++) if (S.names[i][0].indexOf(q) === 0) m.push(S.names[i][1]);
      if (!m.length) { list.hidden = true; return; }
      list.innerHTML = m.map(function (c) { var o = info(c); return '<li role="option" data-code="' + c + '">' + esc(o.nom) + ' <span class="cp-meta">(' + c + ")</span></li>"; }).join("");
      list.hidden = false;
    });
    list.addEventListener("click", function (e) { var li = e.target.closest("li"); if (!li) return; input.value = ""; list.hidden = true; setTarget(li.getAttribute("data-code")); });
    document.addEventListener("click", function (e) { if (e.target !== input) list.hidden = true; });
    $("sm-k").addEventListener("input", function (e) { S.k = +e.target.value; refresh(); });
    $("sm-dept").addEventListener("change", function (e) { S.sameDept = e.target.checked; refresh(); });
    $("sm-cls").addEventListener("change", function (e) { S.sameClass = e.target.checked; refresh(); });
    var lst = $("sm-list");
    lst.addEventListener("click", function (e) { var b = e.target.closest(".sm-peer"); if (!b) return; var c = b.getAttribute("data-c"); S.pin = S.pin === c ? null : c; paint(); });
    function hl(e, on) {
      var b = e.target.closest && e.target.closest(".sm-peer"); if (!b) return;
      document.querySelectorAll('.sm-tk[data-c="' + b.getAttribute("data-c") + '"]').forEach(function (t) { t.classList.toggle("hl", on); });
    }
    lst.addEventListener("mouseover", function (e) { hl(e, true); });
    lst.addEventListener("mouseout", function (e) { hl(e, false); });
    lst.addEventListener("focusin", function (e) { hl(e, true); });
    lst.addEventListener("focusout", function (e) { hl(e, false); });
    window.addEventListener("themechange", function () { PortraitKit.resetLocator(); if (S.res) paint(); });
  }

  // Lit l'URL (au chargement et sur hashchange : lien collé, retour arrière).
  function applyHash() {
    var h = readHash();
    S.target = S.row[h.code] != null ? h.code : DEFAULT_CODE;
    var ids = (h.c || "").split(",").filter(function (x) { return BYID[x]; });
    S.active = ids.length ? ids : DEF.slice();
    S.k = h.k && +h.k >= 5 && +h.k <= 30 ? +h.k : DEFAULT_K;
    var f = (h.f || "").split(",");
    S.sameDept = f.indexOf("dept") >= 0; S.sameClass = f.indexOf("cls") >= 0;
    S.pin = h.p && S.row[h.p] != null && h.p !== S.target ? h.p : null;
    $("sm-k").value = S.k; $("sm-dept").checked = S.sameDept; $("sm-cls").checked = S.sameClass;
  }

  function boot() {
    if (!$("sm")) return;
    var inp = $("sm-search"); inp.disabled = true;
    Promise.all([CMap.data.index(), need(["ctx_log_pop", "densite_rang"])]).then(function (r) {
      S.idx = r[0];
      S.idx.c.forEach(function (c, i) { S.row[c] = i; S.names.push([S.idx.n[i].toLowerCase() + " (" + c.toLowerCase() + ")", c]); });
      applyHash();
      wireSearch(); inp.disabled = false;
      window.addEventListener("hashchange", function () { applyHash(); refresh(); });
      var rowF = CRIT.filter(function (c) { return c.row; }).map(function (c) { return c.field; });
      buildRows();
      return need(S.active.map(function (id) { return BYID[id].field; })).then(function () { compute(); paint(); return need(rowF); })
        .then(function () { compute(); paint(); });
    }).catch(function (e) { console.error(e); var b = $("sm-rows"); if (b) b.innerHTML = '<p class="js-loading">Données indisponibles.</p>'; });
  }
  if (document.readyState === "complete") boot(); else window.addEventListener("load", boot);
})();
