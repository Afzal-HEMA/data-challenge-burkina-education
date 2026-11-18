# Data Challenge Burkina – Édition Éducation 2026

**Défi 1 : Cartographie et anticipation du risque de décrochage scolaire (primaire), par région.**

> Question : *Quelles régions risquent le plus de voir leurs élèves du primaire décrocher, et pourquoi ?*

## Solution : un outil d'alerte et d'aide à la décision (application Shiny)

| Onglet | Ce qu'il permet |
|---|---|
| **Carte** (ouverture sur 2026) | Grande carte des 13 régions avec leur nom ; filtres niveau (primaire, préscolaire), indicateur, année (2016-2025 observé, 2026-2027 prédit), sexe, statut public/privé ; clic sur une région : variables qui expliquent son résultat et recommandations ; flash info et notifications (passage au rouge / sortie du rouge) |
| **Simulateur** | Faire varier écoles, enseignants, maintien à l'école, remédiation, actions pour les filles et voir l'effet sur l'achèvement, le score et la couleur des régions ; effort nécessaire pour passer au vert |
| **Investissement** | Répartir un budget (10 milliards FCFA par défaut) entre les régions selon l'urgence, puis entre les actions dans chaque région ; carte avant / après |
| **Tableau de bord** | Vue d'ensemble : écoles, élèves, admission, scolarisation, achèvement, filles, niveaux, statut, milieu |
| **Classement** | Classement et matrice de risque pour toute année, achèvement prédit avec fourchette |
| **Méthode** | Composantes et poids du score, choix du modèle de prédiction, variables les plus explicatives |

Méthode en bref :
1. **SRD – Score de Risque de Décrochage** : situation (achèvement, non-achèvement des entrants, scolarisation, CEP, parité) et dégradation sur 3 ans ; alerte vert / jaune / orange / rouge.
2. **Prédictions 2026-2027** : pour le taux d'achèvement, 4 modèles (naïf, tendance amortie, régression, forêt aléatoire) comparés par validation sur le passé ; le meilleur est retenu pour chaque horizon. Autres indicateurs : tendance amortie choisie sur le passé.
3. **Aide à la décision** : simulateur de mesures et répartition d'un budget (coûts et effets en hypothèses modifiables).

## Organisation du dépôt (LISEZ-MOI)

| Dossier | Contenu |
|---|---|
| `data/raw/` | Données brutes (DGESS 2016-2025). Le fichier Excel n'est pas versionné. |
| `R/` | Scripts numérotés dans l'ordre d'exécution |
| `sorties/01_collecte` … `05_interpretation` | Fichiers produits à chaque étape (versionnés et déposés sur la plateforme) |
| `app/` | Application Shiny |
| `docs/` | Journal de nettoyage, dictionnaire des variables, documentation (15 p.) |

## Relancer le projet

1. Ouvrir `data-challenge-burkina.Rproj` dans RStudio.
2. Copier le fichier DGESS dans `data/raw/` sous le nom `DGESS_prescolaire_primaire_2016_2025.xlsx`.
3. Lancer `source("R/00_run_all.R")`.
4. Lancer l'application : `shiny::runApp("app", launch.browser = TRUE)`.

Installation des packages (une seule fois) :

```r
install.packages(c("tidyverse", "readxl", "stringi", "here", "sf", "ranger",
                   "shiny", "bslib", "leaflet", "DT"))
```

Mettre l'application en ligne (shinyapps.io) : voir `R/06_publier_application.R`.

## Convention de nommage des dépôts

`EQUIPE_ÉTAPE_DÉFI_RÔLE_description_vN.ext`
ex. `E07_02-nettoyage_D1_engineer_effectifs-primaire-propres_v2.csv`
Pas d'espace ni d'accent, une étape par fichier, versions numérotées.

## Sources

- DGESS / Ministère de l'Éducation nationale – Statistiques préscolaire et primaire 2016-2025.
- (à compléter : Data Pack, INSD, OCHA…)
