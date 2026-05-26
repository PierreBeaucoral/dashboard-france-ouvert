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

# 2. Construire les artefacts data dans l'ordre
Rscript scripts/01_geo_boundaries.R       # contours géo
Rscript scripts/02_elections.R            # Législatives 2024
Rscript scripts/03_dvf_aggregate.R        # DVF (immobilier)
Rscript scripts/04_air_atmo.R             # Atmo France (air)
Rscript scripts/06_insee_populations.R    # INSEE populations + COG
Rscript scripts/07_ssmsi_dgfip.R          # SSMSI + DGFiP
Rscript scripts/08_drees_apl.R            # APL santé (5 pros)
Rscript scripts/09_esperance_vie.R        # INSEE espérance vie
Rscript scripts/10_filosofi_densite_defm.R # FiLoSoFi + densité + DARES
Rscript scripts/11_baac.R                 # accidents BAAC
Rscript scripts/12_municipales_2026.R     # Municipales 2026 T2
Rscript scripts/05_merge_and_explore.R    # merge final pour Explorer

# 3. Rendre le site
quarto render

# 4. Tester localement (les modules JS Quarto exigent un serveur HTTP)
cd _site && python3 -m http.server 8765
# Ouvrir http://localhost:8765/
```

## Déploiement GitHub Pages

Limite GitHub Pages : 100 MB par fichier, 1 GB par site. Toutes les pages
sont en dessous (la plus grosse, Élections, fait 45 MB).

```bash
# Option 1 : publier _site/ sur la branche gh-pages
git checkout --orphan gh-pages
git rm -rf .
cp -r _site/* .
git add -A && git commit -m "Deploy"
git push origin gh-pages

# Option 2 : utiliser Quarto Publish
quarto publish gh-pages
```

Activer GitHub Pages dans Settings → Pages → Source = `gh-pages` branch.

## État d'avancement

**État actuel : 10 pages thématiques + analyse + méthodologie, déployable GitHub Pages.**

- [x] **Étapes 0-6** : socle (Législatives, DVF, Air, Bivariate, Explorer)
- [x] **+ Sécurité** : SSMSI délinquance 2025 + BAAC accidents 2024
- [x] **+ Finances** : DGFiP comptes communaux 2024
- [x] **+ Santé** : APL 5 pros (médecins, infirmières, sages-femmes, dentistes, kinés) + espérance de vie INSEE 2024
- [x] **+ Revenus** : INSEE FiLoSoFi 2021 (médiane, déciles, Gini)
- [x] **+ Municipales 2026** : Min. Intérieur, communes T2 (1 526)
- [x] **+ Démographie** : populations INSEE + grille de densité officielle
- [x] **Bivariate étendu** : 2 dropdowns groupés, 37 variables, 1 369 paires possibles
- [x] **Split multi-page** : 10 pages séparées (chacune < 100 MB), compatible GitHub Pages

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
