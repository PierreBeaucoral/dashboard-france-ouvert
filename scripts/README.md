# Pipeline de données

Scripts numérotés, à exécuter dans l'ordre depuis la racine du projet.

| # | Script | Étape projet | Produit |
|---|---|---|---|
| 00 | `00_setup_env.R` | Setup | `renv.lock`, packages installés |
| 01 | `01_geo_boundaries.R` | Étape 1 | `data/processed/geo/{departements,communes,drom_insets}.geojson` |
| 02 | `02_elections.R` | Étape 2 | `data/processed/elections/legislatives_2024_t2.parquet` |
| 03 | `03_dvf_aggregate.R` | Étape 3 | `data/processed/dvf/dvf_aggregated.parquet` |
| 04 | `04_air_stations.R` | Étape 4 | `data/processed/air/stations.parquet` |
| 05 | `05_link_commune_station.R` | Étape 4 | `data/processed/air/commune_station_link.parquet` |
| 06 | `06_bivariate_bins.R` | Étape 5 | `data/processed/explorer/bivariate_bins.parquet` |
| 07 | `07_correlations.R` | Étape 6 | `data/processed/explorer/correlation_matrices.parquet` |

Chaque script :

- prend ses entrées dans `data/raw/` (téléchargements bruts, gitignored),
- produit un artefact dans `data/processed/` (commit-able),
- documente sa source dans `docs/data-sources.md`,
- est idempotent (relançable sans casser l'état).
