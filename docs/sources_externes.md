# Données complémentaires : où les trouver, comment les ajouter

Le fichier DGESS ne dit rien du **contexte** du décrochage (insécurité, déplacements, pauvreté).
Ajouter ces données permet d'**expliquer** les baisses d'achèvement, pas seulement de les constater.

## Priorité 1 : le Data Pack du challenge (déjà en CSV, vérifié par les organisateurs)

Drive du challenge → ouvrir `00 Documentation et référentiels/Catalogue_sources_x_defis.xlsx`, colonne Défi 1 (★★★ en priorité).
Chaque fichier source a son jumeau CSV au même chemin sous `data/`.

| Donnée | Où dans le Data Pack | Variable à produire | Usage dans le projet |
|---|---|---|---|
| Personnes déplacées internes (PDI) par région | `03 Conditions de vie` ou `02 Démographie` (CONASUR) | `pdi` | Déplacés pour 1 000 habitants ; lien avec la baisse des effectifs |
| Écoles fermées pour insécurité (rapports ESU) | `01 Éducation` (MENAPLN, rapports ESU) | `ecoles_fermees` | Part des écoles fermées ; levier « réouverture » |
| Population par région (projections INSD) | `02 Démographie` | `population` | Dénominateur des PDI |
| Pauvreté par région (EHCVM 2021) | `03 Conditions de vie et emploi` | `pauvrete_pct` | Facteur structurel du non-achèvement |

## Priorité 2 : sources publiques en ligne (à télécharger soi-même, puis vérifier)

| Donnée | Source | Lien |
|---|---|---|
| PDI par région (tableaux de bord mensuels) | SP/CONASUR, relayés par OCHA / UNHCR | https://data.unhcr.org/en/country/bfa · contact : idps.data@conasur.gov.bf |
| Écoles fermées (rapport statistique mensuel ESU) | MENAPLN / Cluster Éducation | rapports mensuels ESU (Data Pack ou site du ministère) |
| Écoles géolocalisées (OpenStreetMap) | HOTOSM via HDX | https://data.humdata.org/m/dataset/hotosm_bfa_education_facilities |
| Déplacements (événements) | IDMC via HDX | https://data.humdata.org/dataset/idmc-event-data-for-bfa |

⚠️ Ces sources sont surtout des **PDF** : recopier les chiffres à la main et **vérifier les totaux** (somme des régions = total national).

## Comment ajouter une donnée

1. Copier `data/externes/MODELE_contexte_regions.csv` sous un nouveau nom, par exemple `data/externes/pdi_conasur.csv`.
2. Remplir une ligne par **région × année × variable** :
   `zone` (nom de région, toute orthographe reconnue), `annee` (année de fin d'année scolaire : décembre 2022 → 2023),
   `variable` (`pdi`, `population`, `ecoles_fermees`, `pauvrete_pct`), `valeur` (nombre, sans espace), `source` (producteur, document, date).
3. Lancer `source("R/00_run_all.R")` : le script `03b_contexte_externe.R` contrôle, harmonise et intègre les données.

Variables calculées automatiquement : `ctx_pdi_pour_1000` (si `pdi` et `population`), `ctx_ecoles_fermees_pct`
(écoles fermées / écoles primaires DGESS), `ctx_pauvrete_pct`. Elles apparaissent sur la carte et dans l'analyse
`contexte-correlations` (lien avec l'évolution de l'achèvement sur 3 ans).
