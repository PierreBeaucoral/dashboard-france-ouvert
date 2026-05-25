# Research journal — Dashboard France

Append-only. Une entrée par invocation d'agent ou transition de phase.

---

### 2026-05-25 — Setup
**Phase :** Étape 0 (Setup)
**Cible :** scaffolding du projet
**Score :** N/A
**Verdict :** structure projet créée, thème SCSS posé, dashboard.qmd squelette
fonctionnel, docs scaffolding (sources, décisions, corrélations).
**Rapport :** quality_reports/plans/2026-05-25_plan-global.md

### 2026-05-25 — Squelette carte
**Phase :** Étape 1 (carte fonctionnelle, sans données métier)
**Cible :** `scripts/01_geo_boundaries.R`, `R/map_helpers.R`, `dashboard.qmd`
**Score :** N/A
**Verdict :** carte métropole + 5 cartouches DROM opérationnelle.
Contours départements téléchargés depuis le dépôt gregoiredavid/france-geojson
(métropole en un fichier, DROM en 5 fichiers séparés à fusionner). Simplification
Visvalingam-Whyatt à `keep=0.20` → 101 polygones, 299 KB.
Bascule depuis `geo.api.gouv.fr` parce que son endpoint ignore le paramètre
`geometry=contour` (cf. `docs/data-sources.qmd`).
**Rapport :** `docs/data-sources.qmd` (sources), `docs/decisions.qmd` (méthodo)

### 2026-05-25 — Législatives 2024 T2 + choroplèthe communal
**Phase :** Étape 2 (Législatives bout-en-bout)
**Cible :** `scripts/02_elections.R`, extension `01_geo_boundaries.R` (communes),
`R/map_helpers.R::choropleth_metropole`, `dashboard.qmd`
**Score :** N/A
**Verdict :**
- Dataset officiel MI identifié via API data.gouv.fr (le MCP datagouv a été
  ajouté côté config mais ne sera disponible qu'à la prochaine session ;
  WebFetch sur l'API publique a fait le même travail de discovery).
- Pipeline `02_elections.R` : pivot wide→long sur les blocs candidat,
  agrégation au niveau commune (nuance gagnante, marge, % par famille,
  abstention). 31 392 communes au T2.
- Mapping nuance MI → 5 familles politiques documenté dans
  `docs/decisions.qmd` (HOR→ENS, UDI→LR, UXD/EXD→RN, etc.).
- Contours communaux ajoutés : 35 927 polygones simplifiés à 16 MB
  (Visvalingam-Whyatt keep=0.04).
- Choroplèthe en canvas (`preferCanvas=TRUE`) pour gérer 35 k polygones,
  opacité ∝ marge, tooltip riche, sur-couche départementale en noir fin,
  légende custom en bas à droite. Cartouches DROM identiques.
- Poids HTML rendu : 20 MB. Lourd mais charge en quelques secondes ;
  optimisation possible plus tard via PMTiles si nécessaire.
**Rapport :** `docs/data-sources.qmd`, `docs/decisions.qmd`
