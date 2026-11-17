# Data Challenge Burkina – Édition Éducation 2026

**Défi 1 : Cartographie et anticipation du risque de décrochage scolaire (primaire), par région et province.**

> Question : *Quelles provinces risquent le plus de voir leurs élèves du primaire décrocher l'an prochain, et pourquoi ?*

## Solution

1. **SRD – Score de Risque de Décrochage** : indice composite par province et par année (abandon, redoublement, achèvement, baisse d'effectifs, parité, ratios d'encadrement).
2. **Modèle de prévision** du taux d'abandon N+1 (entraînement 2017-2023, test 2024-2025, comparé à une référence naïve).
3. **Tableau de bord Shiny** : carte d'alerte par province, fiche province, top 10, fiche décideur d'une page.

## Organisation du dépôt (LISEZ-MOI)

| Dossier | Contenu |
|---|---|
| `data/raw/` | Données brutes (DGESS 2016-2025 + Data Pack). **Non versionnées** (usage réservé au concours). |
| `R/` | Scripts numérotés dans l'ordre d'exécution |
| `sorties/01_collecte` … `05_interpretation` | Fichiers produits à chaque étape (déposés sur la plateforme) |
| `app/` | Application Shiny |
| `docs/` | Journal de nettoyage, dictionnaire des variables, documentation (15 p.) |

## Relancer le projet

1. Ouvrir `data-challenge-burkina.Rproj` dans RStudio.
2. Copier le fichier DGESS dans `data/raw/` sous le nom `DGESS_prescolaire_primaire_2016_2025.xlsx`.
3. Lancer `source("R/00_run_all.R")`.
4. Lancer l'application : `shiny::runApp("app")`.

Packages : `tidyverse`, `readxl`, `stringi`, `here`, `sf`, `leaflet`, `shiny`, `ranger`.

## Convention de nommage des dépôts

`EQUIPE_ÉTAPE_DÉFI_RÔLE_description_vN.ext`
ex. `E07_02-nettoyage_D1_engineer_effectifs-primaire-propres_v2.csv`
Pas d'espace ni d'accent, une étape par fichier, versions numérotées.

## Sources

- DGESS / Ministère de l'Éducation nationale – Statistiques préscolaire et primaire 2016-2025.
- (à compléter : Data Pack, INSD, OCHA…)
