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
