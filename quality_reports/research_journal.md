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

### 2026-05-25 — Étape 3 (DVF)
**Phase :** Étape 3
**Cible :** `scripts/03_dvf_aggregate.R`, `R/dvf_helpers.R`, page Immobilier
**Verdict :** pipeline duckdb sur 4 années DVF (2021-2024) — Etalab "latest" ne
garde pas 2019/2020. 655k ventes mono-local filtrées, 148k lignes en sortie
(commune × année × type). Choroplèthe avec sélecteurs radio JS, recoloration
via setStyle() sans rebuild. Panneau trend temporel par barres verticales.

### 2026-05-25 — Étape 4 (Qualité de l'air)
**Phase :** Étape 4
**Cible :** `scripts/04_air_atmo.R`, `R/air_helpers.R`, page Air
**Verdict :** bascule de l'approche "stations en points" vers commune-level
via indice ATMO (Atmo France). Avantages : maille cohérente avec élections
et DVF, pas de rattachement spatial. Limite : snapshot uniquement (pas
d'historique ; le flux ne contient que J + J+1 + J+2). 25 089 communes
couvertes pour le jour J. Sélecteur sous-polluant (NO₂, O₃, PM₁₀, PM₂.₅),
palette officielle Atmo France 6 classes.

### 2026-05-25 — Étapes 5 + 6 (Bivariate + Explorer)
**Phase :** Étapes 5 et 6
**Cible :** `scripts/05_merge_and_explore.R`, page Bivarié, page Explorer
**Verdict :**
- Merge inter-sources au niveau commune : 35k communes × variables élec +
  DVF (2024) + ATMO. Stratification par quartiles d'inscrits (proxy taille
  faute de grille INSEE).
- Corrélations Spearman + Pearson, global et par strate. Patterns connus
  confirmés : LR/RN ρ=-0.65, ENS/RN -0.59, log(inscrits)/prix maison +0.50.
- Page Bivarié : 5 paires pré-calculées (terciles X × terciles Y),
  palette Stevens 3x3, sélecteur radio. JS recolore les polygones à la
  sélection. Légende grille 3x3 interactive.
- Page Explorer : heatmap interactive ggiraph des corrélations (clic-survol →
  tooltip avec ρ et n), scatter hexbin %RN × prix App + LOESS, note
  méthodo permanente (biais écologique + confondant urbain/rural).

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
