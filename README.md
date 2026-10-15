# 🇫🇷 Dashboard France · Données ouvertes

> Tableau de bord exploratoire — **34 875 communes**, **26 jeux de données ouverts** croisés, **80 indicateurs** disponibles (chiffres calculés par `R/site_numbers.R`).

[![Built with Quarto](https://img.shields.io/badge/built%20with-Quarto-2A6F97)](https://quarto.org)
[![License: MIT](https://img.shields.io/badge/license-MIT-A4243B)](LICENSE)
[![Data: open](https://img.shields.io/badge/data-data.gouv.fr-5C8B7D)](https://www.data.gouv.fr)

## 🌐 Site live

**https://pierrebeaucoral.github.io/dashboard-france-ouvert/**

## ✨ Contenu

**15 pages thématiques** (chacune avec carte cliquable + panel commune + 5 cartouches DROM) :

🗳️ Législatives 2024 · 🏛️ Municipales 2026 · 🏠 Immobilier DVF · 🌬️ Qualité air ATMO · 🚨 Sécurité SSMSI+BAAC · 💶 Finances DGFiP · ⚕️ Désert médical DREES + EV INSEE · 📊 Revenus FiLoSoFi · 👥 Démographie INSEE+DARES · 🔥 Carbone RARE/CITEPA + énergie résidentielle ORE · 🚗 Mobilité MOBPRO · 🌱 Agriculture bio Agence Bio · 🏘️ Logement RP 2022 + DPE · 🌊 Risques et sols CatNat + ENAF · 🏪 Services BPE + fibre ARCEP

**4 pages analyse croisée** :

🔀 Carte bivariée (palette Stevens 3×3, 3 160 combinaisons) · 📈 Explorer (heatmap des indicateurs + top 25 corrélations + scatter LOESS) · 🕸️ Profil commune (radar 8D rang centile) · 🗂️ Small multiples départements (96 tuiles, 9 indicateurs)

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

# Rendu Quarto (26 pages, ~5 min)
quarto render
```

### Build incrémental (si parquets déjà présents)

```bash
quarto render
quarto preview  # serveur local sur http://localhost:4848
```

## 📦 Sources de données

26 jeux publics, via data.gouv.fr ou les portails des producteurs (INSEE, CNAF, ARCEP, ADEME…) (sauf contours géo via [gregoiredavid/france-geojson](https://github.com/gregoiredavid/france-geojson)). Détails complets : [`docs/data-sources.qmd`](docs/data-sources.qmd).

| Source | Org | Couverture | Snapshot |
|--------|-----|------------|----------|
| Législatives 2024 | Min. Intérieur | ~35 000 communes | 2024-07 |
| Municipales 2026 | Min. Intérieur | 34 836 communes (élus toutes communes) | 2026-03 |
| DVF immobilier | DGFiP / Etalab | 3,98 M ventes | 2021-2025 (2025 provisoire) |
| Atmo air | Atmo France | ~25 000 communes | 30 j glissants |
| SSMSI délinquance + BAAC accidents | Min. Intérieur / ONISR | ~18 000 + 11 099 | 2025 / 2024 |
| DGFiP finances | DGFiP | ~35 000 | 2024 |
| APL santé (5 pros) + EV INSEE | DREES + INSEE | ~35 000 + 28 000 | 2024 |
| FiLoSoFi revenus (2021 composition ; 2023 niveau, série non comparable) | INSEE | ~31 300 ; ~30 800 | 2021 ; 2023 |
| Populations + Grille densité INSEE | INSEE | ~35 000 | 2017-2024 |
| DEFM chômage | DARES | ~34 900 | 2024-T4 |
| Empreinte carbone | RARE / CITEPA | ~34 800 | base 2018 |
| MOBPRO mobilité | INSEE RP 2022 | ~34 800 | 2022 |
| Agriculture bio | Agence Bio | 23 100 | 2024 |
| Participation électorale 2002-2026 | Min. Intérieur (élections agrégées) | 34 869 | 2002-2026 |
| Recensement 2022 : logement et emploi | INSEE | 34 903 | RP 2022 |
| CatNat + PPRN (GASPAR) | MTE / Géorisques | 34 875 | 1982-2025 |
| Équipements (BPE) | INSEE | 34 875 | 01/01/2025 |
| CAF allocataires (RSA, aides au logement) | CNAF | 34 836 (taux ≥ 1 000 hab.) | 2024-12 |
| Fibre FttH | ARCEP | 34 920 | 2026 T2 |
| Artificialisation (ENAF) | Cerema | 34 856 | 2011-2025 |
| DPE | ADEME | 22 432 (≥ 30 DPE) | depuis 07/2021 |
| Électricité + gaz résidentiels (ORE) | Agence ORE | 34 851 (gaz 9 324) | 2011-2024 |

## 🏗️ Architecture

- **Stack** : Quarto Dashboard en R, leaflet pour les cartes, ggplot2+ggiraph pour les heatmaps, networkD3 pour le Sankey, SVG natif pour les radar/scatter custom.
- **Pipeline** : 30 scripts R numérotés (`scripts/00_…` à `scripts/28_…`, plus `99_validate_data.R`), idempotents (cache local des raw downloads).
- **Multi-page** : 1 page Quarto par thématique pour respecter la limite GitHub Pages de 100 MB/fichier (max actuel : 49 MB).
- **Data** : 13 parquets dans `data/processed/`, ~41 MB total, versionnés. Raw downloads (`data/raw/`) gitignorés.

## ⚠️ Avertissements

**Ce dashboard est descriptif, pas causal.** Toute corrélation observée est une association au niveau communal, soumise au [biais écologique (Robinson, 1950)](docs/correlations.qmd). La densité urbaine est un confondant majeur de presque toutes les variables.

Les snapshots des sources sont décalés (MOBPRO 2022, FiLoSoFi 2021 et 2023, DGFiP 2024, etc.). Les croisements impliquent "dernière observation disponible × dernière observation disponible".

Cf. [`docs/decisions.qmd`](docs/decisions.qmd) pour les choix méthodologiques détaillés.

## 📂 Structure du repo

```
.
├── _quarto.yml                # config Quarto multi-page
├── index.qmd                  # page d'accueil
├── pages/                     # 15 thématiques + 5 analyse
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
├── scripts/                   # 30 scripts pipeline numérotés (00 à 28, plus 99)
│   ├── 01_geo_boundaries.R
│   ├── 02_elections.R
│   ├── ...
│   └── 28_filosofi_2023.R
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
