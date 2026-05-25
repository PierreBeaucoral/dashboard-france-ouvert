# Plan global — Dashboard France · Données ouvertes

**Statut.** APPROVED (utilisateur a validé "go" le 2026-05-25)
**Étape courante.** Étape 0 — Setup (en cours)

## Objectif

Tableau de bord interactif en Quarto/R croisant élections législatives 2024,
prix immobilier DVF (2019-2024) et qualité de l'air à la maille commune, avec
un onglet dédié aux corrélations inter-sujets.

## Stack verrouillée

| Couche | Choix |
|---|---|
| Format | Quarto Dashboard (`.qmd`, format: dashboard) |
| Langage | R 4.6 |
| Carto | `leaflet` (R) |
| Interactivité | `crosstalk` + `plotly` + `ggiraph` |
| Pipeline | `duckdb` + `sf` + `arrow` + `data.table` |
| Déploiement | Static HTML → GitHub Pages / Netlify |
| Env | `renv` |

## Décisions méthodo verrouillées (cf. docs/decisions.qmd)

- Périmètre : métropole + DROM en cartouches
- Législatives : nuance gagnante (couleur) × marge (opacité)
- DVF : mono-local, App+Maison, surface > 9 m², 200 < €/m² < 30 000, médiane, N ≥ 5
- Air : rattachement commune ↔ station la plus proche dans ≤ 15 km
- Corrélations : Spearman par défaut, Pearson optionnel
- Stratification : grille densité INSEE (4 classes)
- Hexbin auto pour scatter > 5k points
- Population de la commune = covariable systématique

## Étapes

| # | Statut | Livrable principal |
|---|---|---|
| 0 | EN COURS | Structure projet, thème, scaffolding |
| 1 | À VENIR | Carte France interactive + DROM (sans données métier) |
| 2 | À VENIR | Législatives 2024 T2 bout-en-bout |
| 3 | À VENIR | DVF agrégé + slider temporel |
| 4 | À VENIR | Stations air + rattachement commune |
| 5 | À VENIR | Bivariate choropleth (mode "2 variables") |
| 6 | À VENIR | Onglet Explorer (scatter + matrice corr + strates) |

## Variables candidates aux corrélations (pré-calcul)

- `elec_pct_nfp`, `elec_pct_ens`, `elec_pct_rn`, `elec_pct_lr`,
  `elec_abstention`, `elec_marge_vainqueur`
- `dvf_prix_m2_appart`, `dvf_prix_m2_maison`, `dvf_n_transactions`
- `air_no2_moy`, `air_pm25_moy`, `air_pm10_moy`
- `ctx_pop_commune`, `ctx_densite`

## Risques identifiés

- Poids GeoJSON communes (mitigé par `rmapshaper::ms_simplify`)
- Crosstalk ne recalcule pas → toute interactivité passe par pré-calcul exhaustif
- DVF parsing : 10M lignes/an, géré via `duckdb`
- Geod'air API capricieuse — sample d'abord, inspecter le schéma
- Couverture air : ~30-40% des communes rurales hors rayon 15 km
