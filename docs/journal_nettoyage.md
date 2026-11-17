# Journal de nettoyage

Une ligne par correction : ce qui a été fait, pourquoi, et l'effet sur les données.

| Date | Tableau source | Problème constaté | Correction appliquée | Script | Lignes concernées |
|---|---|---|---|---|---|
| 17/11 | Effectifs primaire par sexe et région | Région écrite seulement sur la 1re ligne (cellules fusionnées) | `fill(region)` vers le bas | `02_nettoyage_effectifs.R` | toutes |
| 17/11 | idem | Années au format « 2015-2016 » | Codage par année de fin (2015-2016 → 2016) | idem | toutes |
| 17/11 | idem | Écritures variables des régions | Clé normalisée + table d'alias | idem | à compléter |

## Contrôles de cohérence

| Contrôle | Résultat |
|---|---|
| Garçons + Filles = Total | à compléter |
| Somme des 13 régions = National | à compléter |
| Comparaison avec l'annuaire publié | à compléter |
