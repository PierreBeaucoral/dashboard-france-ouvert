# Pipeline de données

Scripts numérotés, à exécuter dans l'ordre depuis la racine du projet.

| # | Script | Étape projet | Produit |
|---|---|---|---|
| 00 | `00_setup_env.R` | Setup | `renv.lock`, packages installés |
| 01 | `01_geo_boundaries.R` | Étape 1 | `data/processed/geo/{departements,communes,drom_insets}.geojson` |
| 02 | `02_elections.R` | Étape 2 | `data/processed/elections/legislatives_2024_t2.parquet` (tour décisif de chaque circonscription, agrégé par bureau de vote ; `tour` = T1/T2/mixte) et `legislatives_2024_arrondissements.parquet` (Paris, Lyon, Marseille) |
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

---

## Mise à jour 2026-10-14 — millésimes rafraîchis et scripts 19-28

Tous les scripts ci-dessous : cache brut dans `data/raw/` (gitignoré, téléchargé une
fois), chemins `here()`, sortie parquet, contrôles `stopifnot()` en fin de script.
**Règle d'harmonisation COG 2026** (helper partagé `scripts/cog_helpers.R`, qui lit
`data/processed/insee/{commune_fusions,populations}.parquet` produits par `06_`) : les
**effectifs** sont remappés vers le COG 2026 puis sommés ; les taux sont toujours
recalculés après, jamais moyennés. Un code encore vivant en 2026 n'est jamais remappé.
Dépendance : lancer `06_insee_populations.R` avant.

### Millésimes rafraîchis (schémas inchangés)

| Script | Changement | Notes |
|---|---|---|
| `03_dvf_aggregate.R` | `YEARS` = 2021-2025 | geo-dvf `latest/csv/2025/full.csv.gz` (98 MB). Même schéma : `code_commune, annee, type_local, n_transactions, prix_m2_median, prix_m2_q25, prix_m2_q75, estimation_insuffisante`. 184 309 lignes. 2025 = année en cours à la date d'extraction (déclarations tardives possibles). |
| `14_mobilite.R` | MOBPRO RP 2019 -> **RP 2022** | https://www.insee.fr/fr/statistiques/8589904 `RP2022_mobpro.parquet` (66 MB, 7 388 023 lignes, géographie COG 2024, remappée COG 2026). Même schéma `mobilite_commune.parquet`. Codes TRANS vérifiés contre `varmod_mobpro_2022.csv` (1 pas de transport, 2 marche, 3 vélo y c. VAE, 4 2RM, 5 voiture, 6 TC) : inchangés, garde-fou `stopifnot` ajouté. |
| `16_mobilite_flows.R`, `17_flows_per_commune.R` | idem, lisent le parquet RP 2022 | schémas inchangés. |

Colonnes dont le sens change (MOBPRO RP 2022 vs RP 2019) :
- `mob_pct_emploi_local` pour Paris, Lyon, Marseille : en RP 2022 `COMMUNE` est la commune
  mère et `DCLT` l'arrondissement ; le script consolide `DCLT` vers la commune mère. Les
  trois villes affichent désormais ~64-84 % (au lieu de 0 % en RP 2019, artefact).
- `mob_top_dest_code` : la destination « étranger » (99999) est exclue.
- `mob_pct_velo` : inclut le vélo à assistance électrique (libellé RP 2022) ; série non strictement
  comparable avec 2019. Le fichier 2022 est pondéré avec IPONDI non arrondi.
- `flows_per_commune.flux` peut valoir 0 (somme IPONDI < 0,5 arrondie), comme avant.

### 19 — `19_participation_elections.R`
- Source : data.gouv.fr « Données des élections agrégées » (Min. Intérieur), licence Ouverte 2.0.
  https://data-pipeline-open.s3.sbg.io.cloud.ovh.net/elections/general_results.parquet (71 MB), niveau bureau de vote.
- Millésime : 2002-2026 (47 scrutins : prés., légis., eur., rég., dép., municipales ; cantonales exclues).
- Sorties : `data/processed/elections/participation_long.parquet` (commune x scrutin : `inscrits, votants, abstentions, blancs, nuls, exprimes, part_abstention, part_blancs_nuls, annee, type_election, tour`) et `participation_commune.parquet` (`abst_pres02_t1, abst_pres22_t1, abst_pres22_t2, abst_euro24, abst_legi24_t1, abst_legi24_t2, abst_muni26_t1, blancnul_pres22_t1, abst_delta_pres02_pres22, abst_ecart_euro24_pres22`, en %/points).
- Caveats : Français de l'étranger et COM du Pacifique hors périmètre (342 codes) ; abstention
  prés. 2022 T1 = 24,7 % hors étranger (26,3 % officiel avec étranger) ; blancs/nuls absents des scrutins 2002-2015 (NA) ; municipales 2008 seulement communes >= 3 500 hab. ; 1 ligne source incohérente (60400, mun. 2026, votants > inscrits) mise à NA.

### 20 — `20_rp2022_logement_emploi.R`
- Source : INSEE RP 2022 base-cc logement (8581474) et emploi-population active (8581444) ; licence Ouverte (conditions INSEE). Géographie COG 2025 -> 2026.
- Sortie : `data/processed/insee_extra/rp2022_logement_emploi.parquet` : effectifs `rp_*` + `pct_res_secondaires, pct_vacants, pct_proprietaires, pct_menages_sans_voiture, pct_maisons, taux_chomage_rp, taux_activite_rp, indic_concentration_emploi, delta_pct_res_secondaires_2011_2022`.
- Caveats : Mayotte absente ; taux masqués si dénominateur < 50 ; vacance RP != LOVAC ; chômage RP != DEFM ; communes < 10 000 hab. = recensement exhaustif sur 5 ans, >= 10 000 = sondage. Les lignes d'arrondissements PLM sont conservées en plus de la commune mère.

### 21 — `21_catnat_gaspar.R`
- Source : Géorisques / GASPAR, http://files.georisques.fr/GASPAR/gaspar.zip (8 MB, extraction 2026-10-05), Licence Ouverte 2.0.
- Sortie : `data/processed/risques/catnat_commune.parquet` : `n_catnat_total, n_catnat_since_2015, n_catnat_inondation, n_catnat_secheresse, n_catnat_mvt_terrain, n_catnat_submersion, n_catnat_tempete, annee_dernier_catnat, pprn_opposable, pprn_inondation_opposable, pprn_rga_opposable`.
- Règle COG : arrêtés **distincts** recomptés après remappage (un arrêté visant deux communes fusionnées compte 1). Zéro = vrai zéro. 34 875 communes, DROM et Mayotte inclus.
- Caveats : arrêté accordé sur demande communale (pas une mesure d'intensité) ; événements 2025 sous-comptés (délai de publication).

### 22 — `22_bpe_equipements.R`
- Source : INSEE BPE 2025, https://www.insee.fr/fr/statistiques/fichier/8217527/DS_BPE_CSV_FR.zip (13 MB) + dictionnaire TYPEQU_2025 ; Licence Ouverte (INSEE). Filtre `GEO_OBJECT == "COM"`.
- Sortie : `data/processed/insee_extra/bpe_equipements.parquet` : `bpe_n_<domaine>` (7 domaines), `bpe_n_total`, comptes par famille (médecin gén., pharmacie, boulangerie, alimentation, école, poste, banque, coiffure, restaurant, dentiste, urgences, gare, maison de santé, France services), `bpe_<domaine>_pour1000`, `bpe_panier_score` (0-10), `bpe_panier_manquants`.
- Choix à consigner dans `docs/decisions.qmd` : le panier « vie courante » = 10 familles listées dans l'en-tête du script.
- Caveats : présence != accès ; ligne absente = 0 ; taux pour 1 000 hab. NA si pop < 50 ; Mayotte incluse.

### 23 — `23_caf_allocataires.R`
- Source : data.caf.fr `s_ben_com_f` (CNAF), Licence Ouverte 2.0 ; dernier mois de droit lu à l'exécution (2024-12). Géographie COG N+1.
- Sortie : `data/processed/insee_extra/caf_allocataires.parquet` : `caf_foyers_*`, `caf_pers_*` (ndur, rsa, ppa, af, al), `caf_pct_pop_*` (personnes couvertes / pop x 100), `caf_taux_affiche`, `caf_annee`.
- Règles : compteurs arrondis à 5 (CNAF) ; **taux uniquement si pop >= 1 000** (9 994 communes) ; taux > 100 % mis à NA.
- Caveats : **MSA exclue** (régime agricole) -> taux sous-estimés en zone rurale ; Mayotte absente (CSSM) ; couverture RSA != taux de pauvreté ; PLM publié par arrondissement (conservés + commune mère sommée).

### 24 — `24_arcep_fibre.R`
- Source : ARCEP, data.gouv.fr « marché du haut et très haut débit fixe - déploiements » (ressource `AAAAT#-Commune`, shapefile zippé, URL résolue via l'API data.gouv à chaque exécution). Dernier : 2026 T2. Licence Ouverte 2.0.
- Sortie : `data/processed/insee_extra/arcep_fibre.parquet` : `arcep_trimestre, arcep_locaux, arcep_locaux_ftth, arcep_pct_ftth` (NA si locaux < 20). 34 920 lignes (communes + arrondissements PLM, + commune mère sommée).
- Caveats : éligibilité théorique ; référentiel de locaux mouvant ; DBF lu par un lecteur minimal (nom de colonne non UTF-8) ; Mayotte ~0 % (réel).

### 25 — `25_conso_enaf.R`
- Source : Cerema, consommation d'ENAF 2011-2025 (data.gouv.fr 6a01b9280dd2d45907e5fc61), `conso-com.csv`, Licence Ouverte 2.0. COG 2025 -> 2026.
- Sortie : `data/processed/risques/enaf_commune.parquet` : `enaf_ha_2011_2025, enaf_ha_2011_2021, enaf_ha_2021_2025, enaf_habitat_ha_2011_2025, enaf_part_habitat_pct, enaf_variation_vs_ref_pct, enaf_ha_par_1000hab_2011_2025, enaf_hab_m2_par_nouveau_menage_1122`.
- Caveats : flux (ha), jamais ramené à la surface communale ; Fichiers fonciers -> non cadastré manquant ; pas de Mayotte ; variation vs référence ZAN (ha/an 2021-25 vs 2011-21) NA si référence < 5 ha.

### 26 — `26_dpe_ademe.R`
- Source : ADEME data-fair `dpe03existant` (15,8 M DPE, 07/2021 -> jour), agrégation côté serveur (`values_agg`, 101 appels, `agg_size` max 1 000), Licence Ouverte 2.0. Cache JSON par département (30 jours).
- Sortie : `data/processed/insee_extra/dpe_commune.parquet` : `dpe_n, dpe_n_A..G, dpe_pct_fg, dpe_pct_ab` (NA si n < 30), `dpe_date_extraction`.
- Caveats : échantillon de DPE (ventes/locations), non représentatif du parc ; changement de méthode 01/07/2024 (< 40 m2) ; 288 DPE sur codes hors COG 2026 écartés (< 0,002 %).

### 27 — `27_energie_ore.R`
- Source : Agence ORE, « Consommation annuelle d'électricité et de gaz par commune » (LO 2.0, data.gouv 65f1185fd7321d8e647ba548), 2011-2024, mise à jour 2026-01-12. Exports agrégés côté serveur (plus de téléchargement de 800 MB) ; COG 2026 (MWh sommés) ; cellules masquées = NA ; rupture de série en 2018.
- Sorties : `data/processed/energie_ore/conso_commune_long.parquet` (commune × année × énergie × secteur : `conso_mwh, nb_pdl, n_mailles_secretisees, secret_partiel`) et `energie_ore_commune.parquet` (1 ligne / commune : `elec_res_mwh_hab, elec_res_mwh_pdl, gaz_res_mwh_hab, part_gaz_res, evol_*_19_last_pct, gaz_res_publie, secret_tot`, ...).
- Caveats : gaz dans 9 324 communes seulement (`gaz_res_publie`) ; une évolution est une borne basse si une cellule est masquée (le script 05 ne garde que les communes sans cellule résidentielle masquée en 2019 et en 2024) ; par habitant = population INSEE 2021 ; le total ORE inclut ~57 TWh de clients industriels RTE. Contrôle national : Enedis 317,0 contre 321,7 TWh publiés (−1,5 %), GRDF 223,7 contre 230,7 (−3,0 %).

### 28 — `28_filosofi_2023.R`
- Source : INSEE, Filosofi 2023 (« Filosofi 2 »), FILOSOFI_CC_csv.zip (https://www.insee.fr/fr/statistiques/8984752, publié le 2026-08-06, Licence Ouverte).
- Sortie : `data/processed/filosofi_2023/filosofi_2023_communes.parquet` (34 768 lignes) : `rev_median_2023` (€/UC, 30 794 publiées), `taux_pauvrete_2023` (%, 5 242 publiées), `perimetre_elargi_2023`. Seules MED_SL et PR_MD60 existent au niveau communal ; métropole et La Réunion.
- Caveat : **non comparable avec Filosofi 2021** (INSEE : « à proscrire ») ; Spearman 2021 contre 2023 = 0,935 (validité seulement, jamais affiché).

### 10 — `10_filosofi_densite_defm.R` (mise à jour 2026-10-14)
- Filosofi 2021 : le script **télécharge** désormais le fichier officiel INSEE (https://www.insee.fr/fr/statistiques/7756855, `FILO2021_DISP_COM.csv` et `FILO2021_DEC_COM.csv`, décimale virgule, secret = `s`). Re-run complet : les neuf colonnes sont identiques à l'ancien parquet (miroir Geoptis) ; différences : +3 lignes (villes 75056, 69123, 13055, ajoutées par l'INSEE ; `05_` les recalcule depuis les arrondissements) et `nom_commune` tiré de `populations.parquet`.
- Grille de densité et DEFM : fichiers bruts posés à la main (`data/raw/insee_densite/`, `data/raw/dares/`), sans URL stable : si absents, le parquet existant est conservé. Le lookup `densite_rang` reste vérifié par `99_validate_data.R`.

### 05 — `05_merge_and_explore.R` (mise à jour 2026-10-14)
- Ajoute 28 indicateurs à `commune_merged` (participation, RP 2022, CatNat, BPE, CAF, ARCEP, ENAF, DPE, ORE, Filosofi 2023), uniquement des taux et parts. Règles : CAF si pop ≥ 1 000, DPE si n ≥ 30, évolutions ORE sans cellule résidentielle masquée. Écrit `explorer/indicator_meta.parquet` (var, label, group, unit, dig) : registre unique des 80 indicateurs, lu par l'Explorer, la Carte bivariée et les Classements.



## Mise à jour 2026-10-15 — corrections de l'audit Referee 2

- **Ordre** : `06_insee_populations.R` AVANT `02_elections.R` (les élections sont harmonisées au COG 2026 avec `cog_helpers.R`) et avant 19-28 ; `05_merge_and_explore.R` en dernier, puis `99_validate_data.R`.
- `06_` : populations légales RP 2017-2021 **sommées** vers le COG 2026 (codes absorbés). Mayotte = recensement 2017, téléchargé par `06_` lui-même (INSEE Focus 105, `if105.xls`, appariement par libellé, total 256 518 vérifié) ; 10 communes rétablies 2024-2025 = populations légales 2023 (`ensemble.zip`, INSEE 8680726), `pop_millesime` 2017 / 2023 / 2021 selon le cas (audit N4, N9).
- `02_elections.R` : sortie au COG 2026 (communes COG seulement, 34 865) ; codes consulaires/COM écartés.
- `05_` : table des communes = COG 2026 (34 875) ; villes PLM : ligne officielle conservée si la source la publie.
- `07_` : cambriolages en ‰ logements (`ssmsi_log_ref`, `del_cambriolages_n`), `del_total_pour_mille` = Σ effectifs / population × 1 000, `del_n_total`.
- `19_` : « abstention attendue 24,7 % » supprimée (non sourcée) ; l'abstention 2022 T1 de 24,7 % hors étranger est une valeur observée, pas un contrôle.

## Mise à jour 2026-10-15 (tour 2) — autonomie du refresh et COG 2026 complet

- **Refresh depuis un `data/raw/` vide (N1)** : `06_`, `07_` (SSMSI + DGFiP) et `11_` (BAAC) téléchargent eux-mêmes leurs fichiers (URL « latest » data.gouv.fr ou INSEE, cache idempotent). `08_drees_apl.R` n'est **pas** dans la boucle automatique (XLSX DREES sans URL stable, mise en page de la nouvelle édition différente) : il s'arrête sans écrire s'il n'a aucun fichier. `01_geo_boundaries.R` n'y est pas non plus (change avec l'édition du COG). `15_agriculture_bio.R` non plus (URL datées Agence Bio en 404 depuis la republication de septembre 2026 ; nouvelle édition 2025 à migrer à la main). Boucle : `06 07 09 10 11 13 02 19 20 21 22 23 24 25 26 27 28`, puis `05`, puis `99`.
- `01_geo_boundaries.R` : contours communaux Etalab édition 2026 (COG 2026, 34 917 polygones : communes + 45 arrondissements PLM) ; lit `populations.parquet` (lancer `06_` avant). Départements inchangés (gregoiredavid).
- `02_elections.R` : vainqueur = famille aux voix cumulées de la commune (une seule règle, N2) ; remappage depuis 2015 ; un code non communal inconnu fait échouer le script (N5).
- `02_elections.R` (2026-10-15, reconstruction au bureau de vote) : sources = fichiers MI par bureau de vote T1/T2 (`bv-2024-t*.csv`), par circonscription (`circo-2024-t*.csv`, drapeau « élu ») et « France entière » (`france-2024-t1.xlsx`, `-t2.csv`) dans `data/raw/elections/` (téléchargés par le script). La circonscription d'un bureau est déduite de ses candidats ; chaque bureau compte au tour qui a élu le député de sa circonscription ; `tour` = T1 / T2 / mixte ; `couverture_inscrits` ≈ 1 (min 0,859, écart observé d'inscrits entre tours) ; `repli_t1` = Geay (T2 sans voix publiées). Contrôles bloquants : voix par candidat bureaux = circonscriptions, totaux nationaux = « France entière ». REC et RDG restent « Divers » (choix du propriétaire) ; nuance inconnue = arrêt. Sortie en plus : `legislatives_2024_arrondissements.parquet` (45 arrondissements PLM, code BV = arrondissement + bureau ; même colonnes que le fichier commune, T1 et couverture par arrondissement ; affichés par la carte Législatives). Durée ≈ 1-2 min. Ne lit plus les fichiers `resultats-2024-t*-par-commune.csv` (inutilisés, supprimables de `data/raw/elections/`).
- `05_` : `cog_rename()` applique les changements de code (MOD 41/50) à toutes les sources avant les jointures (N6) ; strate NA si ni population ni inscrits (N7).
- `11_` : accidents remappés au COG 2026 avant agrégation (N3).
- `99_validate_data.R` : plus de constante figée (Paris = ligne brute SSMSI) ; contrôles de vainqueur, BAAC, couverture géométrique, indicateurs des communes renumérotées ; les comparaisons au brut sont ignorées avec un message quand `data/raw/` est absent.
