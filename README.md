# Tableau de bord · Données ouvertes France

Dashboard exploratoire croisant trois sources publiques (data.gouv.fr) à la
maille communale :

- **Élections** — Législatives 2024, second tour, par commune
- **Immobilier** — Prix médian au m² par commune (DVF, 2019-2024)
- **Qualité de l'air** — Indicateurs par station (Geod'air / LCSQA)

Le dashboard permet l'exploration de chaque sujet **et** l'analyse des
**corrélations** inter-sujets, avec stratification par densité urbaine.

## Stack

- **Quarto Dashboard** en R (`.qmd`, rendu HTML statique)
- **Interactivité** : `crosstalk` + `leaflet` + `plotly`/`ggiraph`
- **Pipeline data** : R + `duckdb` + `sf` + `arrow`
- **Déploiement** : GitHub Pages / Netlify (100% statique)

## Démarrage rapide

```bash
# 1. Installer l'environnement R
Rscript scripts/00_setup_env.R

# 2. Construire les artefacts data (à venir, par étape)
# Rscript scripts/01_geo_boundaries.R    # Étape 1
# Rscript scripts/02_elections.R         # Étape 2
# ...

# 3. Rendre le dashboard
quarto render
```

Le site rendu est dans `_site/`.

## État d'avancement

- [x] **Étape 0 — Setup** : structure projet, thème, scaffolding
- [x] **Étape 1 — Squelette carte** : contours départements (101 entités, simplifiés à 299 KB), 5 cartouches DROM, navigation cliquable
- [x] **Étape 2 — Législatives 2024** : choroplèthe communal par famille gagnante (NFP/ENS/LR/RN/Divers) sur 35 232 communes T1+T2, tooltip riche, légende, rendu canvas, panneau latéral au clic
- [x] **Étape 3 — DVF** : agrégation duckdb 4 ans (2021-2024), choroplèthe prix m² par commune, sélecteurs année/type, panneau trend temporel
- [x] **Étape 4 — Qualité de l'air** : indice ATMO snapshot par commune (~25k), sélecteur sous-polluant (NO₂/O₃/PM₁₀/PM₂.₅), panneau pills colorées
- [x] **Étape 5 — Mode bivarié** : choroplèthe 3×3 sur paires pré-calculées (% RN × Prix App, % NFP × Prix, Abstention × Prix, % RN × PM₂.₅, Prix × ATMO), légende Stevens
- [x] **Étape 6 — Explorer** : heatmap interactive ggiraph des corrélations Spearman, scatter hexbin %RN × prix App avec LOESS, note méthodo

## Structure

```
.
├── _quarto.yml           Config Quarto (site web)
├── index.qmd             Page d'accueil
├── dashboard.qmd         Dashboard (format: dashboard)
├── R/                    Helpers R sourcés dans les .qmd
├── scripts/              Pipeline data (R one-shot, numéroté)
├── data/
│   ├── raw/              Téléchargements bruts (gitignored)
│   └── processed/        Artefacts (parquet, geojson) — commit
├── styles/custom.scss    Thème
└── docs/                 Sources, décisions, note sur corrélations
```

## Méthodologie

Toutes les décisions structurantes (filtres DVF, rattachement commune-station,
choix de la métrique de corrélation, stratification) sont documentées dans
[`docs/decisions.qmd`](docs/decisions.qmd). La note dédiée aux corrélations
([`docs/correlations.qmd`](docs/correlations.qmd)) couvre le biais écologique
et la lecture du *bivariate choropleth*.

## Reproductibilité

- Versions R figées via `renv.lock`
- Scripts du pipeline idempotents
- Snapshots datés des sources dans `docs/data-sources.qmd`
- Code source du dashboard visible (chunks repliables)
