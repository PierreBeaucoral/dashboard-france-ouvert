# 🇫🇷 Dashboard France · Données ouvertes

> Tableau de bord exploratoire — **35 000 communes**, **13 sources data.gouv.fr** publiques croisées, **49 indicateurs** disponibles.

[![Built with Quarto](https://img.shields.io/badge/built%20with-Quarto-2A6F97)](https://quarto.org)
[![License: MIT](https://img.shields.io/badge/license-MIT-A4243B)](LICENSE)
[![Data: open](https://img.shields.io/badge/data-data.gouv.fr-5C8B7D)](https://www.data.gouv.fr)

## 🌐 Site live

**https://pierrebeaucoral.github.io/dashboard-france-ouvert/**

## ✨ Contenu

**12 pages thématiques** (chacune avec carte cliquable + panel commune + 5 cartouches DROM) :

🗳️ Législatives 2024 · 🏛️ Municipales 2026 · 🏠 Immobilier DVF · 🌬️ Qualité air ATMO · 🚨 Sécurité SSMSI+BAAC · 💶 Finances DGFiP · ⚕️ Désert médical DREES + EV INSEE · 📊 Revenus FiLoSoFi · 👥 Démographie INSEE+DARES · 🔥 Carbone RARE/CITEPA · 🚗 Mobilité MOBPRO · 🌱 Agriculture bio Agence Bio

**4 pages analyse croisée** :

🔀 Carte bivariée (palette Stevens 3×3, 1 176 combinaisons) · 📈 Explorer (heatmap 49×49 + top 25 corrélations + scatter LOESS) · 🕸️ Profil commune (radar 8D rang centile) · 🗂️ Small multiples départements (96 tuiles, 9 indicateurs)

## 🚀 Quickstart

### Pré-requis

- R ≥ 4.3 (+ packages : `tidyverse`, `arrow`, `duckdb`, `leaflet`, `sf`, `networkD3`, `ggiraph`, `here`)
- Quarto ≥ 1.5
- ~2 GB d'espace disque (raw downloads) + ~500 MB pour le rendu HTML

### Build complet depuis zéro

```bash
git clone https://github.com/PierreBeaucoral/dashboard-france-ouvert.git
cd dashboard-france-ouvert

# Installer les packages R (1 commande)
Rscript -e 'install.packages(c("tidyverse","arrow","duckdb","leaflet","sf",
                                "networkD3","ggiraph","here","jsonlite",
                                "readr","curl","htmlwidgets","htmltools",
                                "tibble","rlang","stringr","tidyr","glue",
                                "rmapshaper"))'

# Pipeline data — chaque script télécharge ses sources brutes la 1ère fois,
# puis lit le cache local
for i in $(seq -w 1 18); do
  Rscript scripts/${i}_*.R
done

# Rendu Quarto (22 pages, ~5 min)
quarto render
```

### Build incrémental (si parquets déjà présents)

```bash
quarto render
quarto preview  # serveur local sur http://localhost:4848
```

## 📦 Sources de données

13 sources publiques, toutes via data.gouv.fr (sauf contours géo via [gregoiredavid/france-geojson](https://github.com/gregoiredavid/france-geojson)). Détails complets : [`docs/data-sources.qmd`](docs/data-sources.qmd).

| Source | Org | Couverture | Snapshot |
|--------|-----|------------|----------|
| Législatives 2024 | Min. Intérieur | ~35 000 communes | 2024-07 |
| Municipales 2026 | Min. Intérieur | 34 836 communes (élus toutes communes) | 2026-03 |
| DVF immobilier | DGFiP / Etalab | 655 495 ventes | 2021-2024 |
| Atmo air | Atmo France | ~25 000 communes | 30 j glissants |
| SSMSI délinquance + BAAC accidents | Min. Intérieur / ONISR | ~18 000 + 11 099 | 2025 / 2024 |
| DGFiP finances | DGFiP | ~35 000 | 2024 |
| APL santé (5 pros) + EV INSEE | DREES + INSEE | ~35 000 + 28 000 | 2023-2024 |
| FiLoSoFi revenus | INSEE | ~30 000 | 2021 |
| Populations + Grille densité INSEE | INSEE | ~35 000 | 2017-2024 |
| DEFM chômage | DARES | ~7 400 | 1 trimestre |
| Empreinte carbone | RARE / CITEPA | ~34 800 | base 2018 |
| MOBPRO mobilité | INSEE RP 2019 | ~34 800 | 2019 |
| Agriculture bio | Agence Bio | 23 100 | 2024 |

## 🏗️ Architecture

- **Stack** : Quarto Dashboard en R, leaflet pour les cartes, ggplot2+ggiraph pour les heatmaps, networkD3 pour le Sankey, SVG natif pour les radar/scatter custom.
- **Pipeline** : 18 scripts R numérotés (`scripts/01_…` à `scripts/18_…`), idempotents (cache local des raw downloads).
- **Multi-page** : 1 page Quarto par thématique pour respecter la limite GitHub Pages de 100 MB/fichier (max actuel : 49 MB).
- **Data** : 13 parquets dans `data/processed/`, ~41 MB total, versionnés. Raw downloads (`data/raw/`) gitignorés.

## ⚠️ Avertissements

**Ce dashboard est descriptif, pas causal.** Toute corrélation observée est une association au niveau communal, soumise au [biais écologique (Robinson, 1950)](docs/correlations.qmd). La densité urbaine est un confondant majeur de presque toutes les variables.

Les snapshots des sources sont décalés (MOBPRO 2019, FiLoSoFi 2021, DGFiP 2024, etc.). Les croisements impliquent "dernière observation disponible × dernière observation disponible".

Cf. [`docs/decisions.qmd`](docs/decisions.qmd) pour les choix méthodologiques détaillés.

## 📂 Structure du repo

```
.
├── _quarto.yml                # config Quarto multi-page
├── index.qmd                  # page d'accueil
├── pages/                     # 12 thématiques + 4 analyse
│   ├── elections.qmd
│   ├── municipales.qmd
│   ├── immobilier.qmd
│   ├── ...
│   ├── explorer.qmd
│   ├── bivariate.qmd
│   ├── profil.qmd
│   └── departements.qmd
├── docs/                      # docs méthodo (Quarto)
│   ├── data-sources.qmd
│   ├── decisions.qmd
│   └── correlations.qmd
├── R/                         # helpers partagés
│   ├── setup_dashboard.R      # chargement données (sourcé par chaque page)
│   ├── map_helpers.R          # choropleth_metropole, choropleth_municipales, DROM insets
│   ├── socio_helpers.R        # choropleth_socio générique
│   ├── dvf_helpers.R          # DVF breaks
│   └── air_helpers.R          # ATMO
├── scripts/                   # 18 scripts pipeline numérotés
│   ├── 01_geo_boundaries.R
│   ├── 02_elections.R
│   ├── ...
│   └── 18_municipales_elus.R
├── data/
│   ├── raw/                   # téléchargements .gitignore'd
│   └── processed/             # parquets versionnés (~41 MB)
└── styles/                    # SCSS custom
```

## 📜 Licence

Code : **MIT** (cf. [LICENSE](LICENSE)).

Données sources : restent sous leur licence d'origine, principalement **Licence Ouverte 2.0** (Etalab) — utilisation libre y compris commerciale, avec mention de la source. Cf. [data.gouv.fr/licences](https://www.data.gouv.fr/licences).

## 🤝 Contribuer

Issues + PR bienvenus. Pour ajouter une source :

1. Créer `scripts/19_ma_source.R` qui télécharge + agrège par commune
2. Étendre `R/setup_dashboard.R` pour charger le parquet
3. Créer une page `pages/ma_source.qmd` (template depuis `pages/securite.qmd`)
4. Ajouter au navbar dans `_quarto.yml`
5. Documenter dans `docs/data-sources.qmd`

---

*Pierre Beaucoral · 2026*
