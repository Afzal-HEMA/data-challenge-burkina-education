# Dictionnaire des variables

Modèle commun : **province × année × sexe** (et région de rattachement, 13 anciennes régions).

| Variable | Type | Description | Unité / valeurs | Source |
|---|---|---|---|---|
| `region` | texte | Région (13 anciennes régions) | ex. « Boucle du Mouhoun » | DGESS |
| `province` | texte | Province (nom harmonisé) | 45 provinces | DGESS |
| `pcode` | texte | P-code OCHA de la province (jointure carte) | ex. BF4601 | OCHA |
| `annee` | entier | Année de fin de l'année scolaire | 2016 = 2015-2016 | – |
| `sexe` | texte | G = garçons, F = filles, T = total | G/F/T | DGESS |
| `effectif` | numérique | Élèves inscrits au primaire | élèves | DGESS |
| `taux_abandon` | numérique | à compléter | % | DGESS / calcul |
| `taux_redoublement` | numérique | à compléter | % | DGESS / calcul |
| `taux_achevement` | numérique | à compléter | % | DGESS / calcul |
| `srd` | numérique | Score de Risque de Décrochage | 0 à 100 | calcul |
