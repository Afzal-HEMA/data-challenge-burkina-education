# Dictionnaire des variables

## Table d'indicateurs (sortie de l'étape 3 = entrée des étapes 4 et 5)

Fichier : `sorties/03_transformation/EXX_03-transformation_D1_architect_indicateurs-province_vN.csv`
Une ligne = **une province × une année × un sexe**. Les scripts 04 lisent automatiquement la version la plus récente.
Une colonne absente ou vide est tolérée : le SRD redistribue son poids et le modèle l'ignore.

| Variable | Type | Description | Unité / valeurs | Obligatoire | Source |
|---|---|---|---|---|---|
| `province` | texte | Nom officiel (voir `data/referentiels/provinces_regions.csv`) | 45 provinces | oui | DGESS |
| `region` | texte | Région de rattachement (13 anciennes régions) | | oui | référentiel |
| `annee` | entier | Année de fin de l'année scolaire | 2016 = 2015-2016 | oui | – |
| `sexe` | texte | G = garçons, F = filles, T = total | G / F / T | oui (au moins T) | DGESS |
| `effectif` | numérique | Élèves inscrits au primaire | élèves | recommandé | DGESS |
| `taux_abandon` | numérique | Taux d'abandon au primaire | % (0-100) | **oui** | DGESS |
| `taux_redoublement` | numérique | Taux de redoublement | % | recommandé | DGESS |
| `taux_achevement` | numérique | Taux d'achèvement du primaire | % | recommandé | DGESS |
| `eleves_par_maitre` | numérique | Élèves / enseignants | ratio | option | DGESS (calcul) |
| `eleves_par_classe` | numérique | Élèves / salles de classe | ratio | option | DGESS (calcul) |
| `indice_parite` | numérique | Indice de parité filles/garçons (TBS F / TBS G ou effectifs F / G) | ~1 | option (ligne T) | DGESS (calcul) |
| `part_ecoles_fermees` | numérique | Part des écoles fermées pour insécurité | % | option | ESU / Data Pack |

## Table de l'onglet Accueil (sortie de l'étape 3)

Fichier : `sorties/03_transformation/EXX_03-transformation_D1_architect_tableau-accueil_vN.csv`
Format long : une ligne = une valeur. Chaque bloc de l'accueil s'affiche seulement si ses lignes existent.

| Colonne | Valeurs |
|---|---|
| `annee` | année de fin (2016 = 2015-2016) |
| `zone` | `National`, nom de région ou nom de province |
| `niveau_zone` | `national` / `region` / `province` |
| `dimension` | `ensemble`, `niveau`, `statut`, `milieu`, `sexe` |
| `modalite` | `Total` (pour `ensemble`) ; `Préscolaire`, `CP1`…`CM2` ; `Public`, `Privé` (ou `Communautaire`) ; `Urbain`, `Rural` ; `Filles`, `Garçons` |
| `indicateur` | `ecoles`, `eleves`, `enseignants`, `salles_classe` |
| `valeur` | nombre |

Blocs alimentés : cartes chiffrées (`ensemble` × 4 indicateurs), élèves par niveau (`niveau`/`eleves`),
écoles et élèves par statut et par milieu, filles scolarisées (`sexe`/`eleves`), élèves par région ou province
(`ensemble`/`eleves` au niveau inférieur).

## Variables calculées à l'étape 4

| Variable | Description |
|---|---|
| `evol_abandon` | Taux d'abandon N − taux d'abandon N−1 (points) |
| `evol_effectif` | Variation des effectifs N / N−1 (%) |
| `ecart_parite` | \|1 − indice_parite\| |
| `s_<variable>` | Rang percentile de la composante (0-1, sens « risque ») |
| `srd` | Score de Risque de Décrochage (0-100) |
| `alerte` | vert / jaune / orange / rouge (seuils : médiane, P75, P90 de toutes les années) |
| `prevu_retenu` | Taux d'abandon prévu pour N+1 par le meilleur modèle |

## Référentiels

| Fichier | Contenu | Source |
|---|---|---|
| `data/referentiels/provinces_regions.csv` | 45 provinces et leur région (13 anciennes régions) | Découpage administratif 2016-2024 |
| `data/referentiels/alias_noms.csv` | Orthographes alternatives → clé normalisée | Constitué au fil du nettoyage |
| `data/referentiels/provinces_bfa.geojson` | Contours des 45 provinces (simplifiés) | geoBoundaries gbOpen BFA ADM2 (wmgeolab, licence ouverte) |
