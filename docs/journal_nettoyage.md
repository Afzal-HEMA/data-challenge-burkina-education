# Journal de nettoyage

Source : *Données statistiques préscolaire et primaire 2016-2025*, DGESS / Ministère de l'Éducation nationale.
Script : `R/02_nettoyage_dgess.R` (catalogue des 21 tableaux en tête de script).
Sortie : une table longue unique `donnees-dgess-long` (`tableau, cycle, indicateur, zone, niveau_zone, dimension, modalite, modalite2, sexe, annee, valeur`).

## Inventaire des tableaux

| Code | Feuille | Contenu | Niveau géographique | Forme |
|---|---|---|---|---|
| P01 | Feuil1 | Effectifs du primaire par sexe | 13 régions + national | région × sexe en lignes |
| P02 | Feuil2 | Effectifs du primaire par statut (public/privé) et sexe | national | statut × sexe |
| P03 | Feuil3 | Effectifs du primaire par milieu (urbain/rural) et sexe | national | milieu × sexe |
| P04 | Feuil4 | Effectifs par statut, niveau (CP1-CM2) et sexe | national | statut × niveau × sexe |
| P05 | Feuil5 | Nouveaux inscrits au CP1 par sexe | régions + national | région × sexe |
| P06 | Feuil6 | Taux brut d'admission (TBA) | régions + national | G/F/T sous chaque année |
| P07 | Feuil7 | Taux brut de scolarisation (TBS) | régions + national | G/F/T |
| P08 | Feuil8 | Taux d'achèvement du primaire (TAP) | régions + national | G/F/T |
| P09 | Feuil9 | Taux de réussite au CEP | régions + national | région × sexe |
| P10-P12 | Feuil10-12 | Indices de parité (TBA, TBS, TAP) | régions + national | région |
| P13 | Feuil13 | Nombre d'écoles primaires par statut | régions + national | région × statut |
| S01 | PRESCOLAIRE T1 | Effectifs du préscolaire | régions + national | G/F/T |
| S02-S03 | PRESCOLAIRE T2-T3 | Préscolaire par statut / par milieu (milieu : 2018-2025) | national | G/F/T |
| S04 | PRESCOLAIRE T4 | Préscolaire par statut et section | national | G/F/T |
| S05-S06 | PRESCOLAIRE T5-T6 | TBS du préscolaire, indice de parité | régions + national | G/F/T, région |
| S07-S08 | PRESCOLAIRE T7-T8 | Structures et salles du préscolaire par statut | régions + national | région × statut + sous-totaux |

**Limites constatées :** aucune donnée par province ; pas de taux d'abandon ni de redoublement ; pas d'effectifs enseignants.
→ L'analyse est conduite au niveau des **13 régions** et le décrochage est mesuré par des indicateurs indirects (voir `04_analyse_srd.R`).

## Corrections appliquées

| Problème constaté | Tableaux | Correction | Fonction |
|---|---|---|---|
| Cellules fusionnées (région, statut, niveau écrits sur la 1re ligne du bloc) | tous | Remplissage vers le bas des identifiants | `fill_fusions()` |
| Années écrites de 4 façons : `2015-2016`, `2015/2016`, `2015/16`, `2016` | tous | Codage unique par l'année de fin (2015-2016 → 2016) | `annee_fin()` |
| Années sur 3 colonnes fusionnées avec une ligne G / F / T | P06-P08, S01-S05 | Lecture dédiée, année propagée sur les 3 colonnes | `lire_bloc_gft()` |
| Texte parasite dans certaines cellules d'en-tête d'année | plusieurs | Seule la 1re ligne de la cellule est lue | `annee_fin()` |
| Noms de régions variables : `HAUT-BASSINS`, `Burkina Faso`, `NATIONAL`, `Total général` | P09, P13, P10-P12, S01... | Clé normalisée + table d'alias | `identifier_entite()`, `alias_noms.csv` |
| Sexe écrit `GARCONS`, `Garçons`, `G`... | tous | Normalisé en G / F / T | `normaliser_sexe()` |
| Statut écrit `PRIVE`, `Privé`... | P02, P04, P13, S02... | Libellés harmonisés | `normaliser_modalite()` |
| Lignes de sous-total `Total BOUCLE DU MOUHOUN`, `Total Public` | S04, S07, S08 | Repérées et étiquetées « Total » (non additionnées) | `rattacher_zone()` |
| Nombres avec espaces, virgules, `%`, `-`, `ND` | tous | Conversion numérique, `-` et `ND` → manquant | `nettoyer_valeur()` |
| Le total par niveau (tous statuts) n'existe pas | P04 | Recalculé : public + privé | `03_transformation_indicateurs.R` |

## Contrôles de cohérence (à remplir après exécution sur les vraies données)

| Contrôle | Fichier produit | Résultat |
|---|---|---|
| Garçons + Filles = Total (effectifs) | `controle-garcons-filles-total` | … cas d'écart |
| Somme des 13 régions = National | `controle-regions-national` | … cas d'écart sur … |
| Valeurs manquantes | sortie console | … |
| Comparaison avec l'annuaire publié (au moins 3 chiffres) | à faire à la main | … |
