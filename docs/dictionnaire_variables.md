# Dictionnaire des variables

Unité d'analyse : **région** (13 anciennes régions) × **année** (année de fin d'année scolaire : 2016 = 2015-2016) × **sexe** (G / F / T).
Le fichier DGESS ne contient pas de données par province.

## 1. Table nettoyée (étape 2) : `donnees-dgess-long`

| Colonne | Description |
|---|---|
| `tableau` | Code du tableau source (P01-P13 primaire, S01-S08 préscolaire), voir `docs/journal_nettoyage.md` |
| `cycle` | `primaire` / `prescolaire` |
| `indicateur` | `eleves`, `nouveaux_cp1`, `tba`, `tbs`, `tap`, `reussite_cep`, `ips_tba`, `ips_tbs`, `ips_tap`, `ecoles`, `structures`, `salles_classe` |
| `zone` | Nom de région harmonisé ou `National` |
| `niveau_zone` | `region` / `national` |
| `dimension` | `ensemble`, `statut`, `milieu`, `statut_niveau` |
| `modalite`, `modalite2` | Public / Privé / Communautaire, Urbain / Rural, CP1…CM2, sections du préscolaire, `Total` |
| `sexe` | G / F / T |
| `annee`, `valeur` | Année de fin, valeur numérique (NA si `-` ou `ND`) |

## 2. Table d'indicateurs (étape 3) : `indicateurs-region`

Une ligne = zone × année × sexe. Entrée du SRD et du modèle.

| Variable | Description | Unité | Source |
|---|---|---|---|
| `eleves` | Effectifs du primaire | élèves | P01 |
| `nouveaux_cp1` | Nouveaux inscrits au CP1 | élèves | P05 |
| `tba` | Taux brut d'admission au CP1 | % | P06 |
| `tbs` | Taux brut de scolarisation | % | P07 |
| `tap` | Taux d'achèvement du primaire | % | P08 |
| `reussite_cep` | Taux de réussite au CEP | % | P09 |
| `ips_tba`, `ips_tbs`, `ips_tap` | Indices de parité F/G (ligne T) | ratio | P10-P12 |
| `ecoles` | Écoles primaires (tous statuts, ligne T) | écoles | P13 |
| `eleves_prescolaire`, `tbs_prescolaire` | Effectifs et TBS du préscolaire | élèves, % | S01, S05 |
| `evol_eleves` | Variation des effectifs N / N-1 | % | calcul |
| `evol_nouveaux_cp1` | Variation des nouveaux inscrits N / N-1 | % | calcul |
| `evol_tap` | TAP N − TAP N-1 | points | calcul |
| `ecart_acces_achevement` | TBA − TAP : enfants qui entrent mais n'achèvent pas | points | calcul |
| `part_filles` | Part des filles dans les effectifs (ligne T) | % | calcul |

## 3. Table de l'onglet Accueil (étape 3) : `tableau-accueil`

Format long : `annee, zone, niveau_zone, dimension, modalite, indicateur, valeur`.
`ensemble` (tous les indicateurs ci-dessus, ligne T), `sexe` (Filles/Garçons), `statut` (élèves national, écoles par région),
`milieu` (élèves, national), `niveau` (Préscolaire, CP1…CM2, national).
Quand un tableau n'existe qu'au niveau national, l'application l'affiche avec la mention « national ».

## 4. Passage apparent (étape 3) : `passage-apparent-niveaux`

Élèves au niveau k+1 l'année N+1 / élèves au niveau k l'année N, national, par sexe. Approximation : les redoublants ne sont pas distingués.

## 5. Variables de l'étape 4

| Variable | Description |
|---|---|
| `s_<variable>` | Rang percentile de la composante (0-1, sens « risque ») |
| `srd` | Score de Risque de Décrochage (0-100) |
| `alerte` | vert / jaune / orange / rouge (seuils : médiane, P75, P90 de toutes les années) |
| `prevu_retenu` | Taux d'achèvement prévu pour N+1 par le meilleur modèle |

## 6. Prédictions sur 2 ans (`04_analyse_projection.R`)

| Fichier | Contenu |
|---|---|
| `projection-region` | Région × année prédite (T) : indicateurs, `tap_bas`/`tap_haut` (fourchette 10e-90e percentiles des erreurs passées), `modele_tap`, `erreur_moyenne_tap`, SRD, alerte |
| `projection-sexe` | Taux et effectifs prédits pour les filles (F) et les garçons (G) |
| `projection-statut` | Écoles primaires, structures et salles du préscolaire prédites par statut |
| `prevision-comparaison-modeles` | MAE, RMSE, sens correct de chaque modèle et horizon (validation sur le passé) |
| `prevision-modele-retenu` | Modèle retenu par horizon |
| `prevision-importance-variables` | Importance des variables (forêt aléatoire) dans l'évolution de l'achèvement |
| `prevision-validation-passe` | Toutes les prédictions passées et leurs erreurs |

Taux d'achèvement : modèles naïf, tendance amortie, régression linéaire (évolution sur 1 et 3 ans, effectifs, non-achèvement),
forêt aléatoire (toutes les variables). Validation « comme en vrai » : chaque modèle n'apprend que sur ce qui était connu à
la date de départ. Autres indicateurs : tendance amortie des 3 dernières années, amortissement choisi sur le passé.

## 6 bis. Contexte externe (`03b_contexte_externe.R`, colonnes `ctx_*` de `indicateurs-region`)

| Variable | Description | Source |
|---|---|---|
| `ctx_enseignants_primaire` | Enseignants du primaire | INSD, « Région … en chiffres 2024 » ; annuaire 2024 du Centre |
| `ctx_eleves_par_enseignant` | Élèves du primaire (DGESS) / enseignants (INSD) | calcul |
| `ctx_eleves_par_classe` | Élèves du primaire (DGESS) / classes du primaire (INSD) | calcul |
| `ctx_pdi_pour_1000`, `ctx_ecoles_fermees_pct`, `ctx_pauvrete_pct` | Si les fichiers correspondants sont ajoutés | voir `docs/sources_externes.md` |

Valeurs exclues (statut « à vérifier ») : `sorties/03_transformation/…_contexte-valeurs-exclues_vN.csv`.
Lien avec l'achèvement et les effectifs : `…_contexte-correlations_vN.csv`.

## 7. Simulateur (`app/simulateur.R`, paramètres : `04_analyse_leviers.R` → `leviers-parametres`)

| Levier | Effet sur les indicateurs | Origine du paramètre |
|---|---|---|
| Écoles supplémentaires | élèves × ((écoles + N) / écoles)^élasticité ; TBS et TBA suivent | **estimée** : variation de log(élèves) ~ variation de log(écoles) + effets année, régions 2016-2025 |
| Enseignants supplémentaires | + N × (élèves par enseignant) élèves, plafonné aux enfants hors de l'école | **hypothèse** réglable (le fichier DGESS ne contient pas d'enseignants) |
| Programme de maintien | non-achèvement × (1 − x %) ; TAP = TBA × (1 − non-achèvement) | objectif fixé par l'utilisateur |
| Remédiation scolaire | réussite au CEP + x points | objectif fixé par l'utilisateur |
| Actions pour les filles | écart de parité de l'achèvement × (1 − x %) | objectif fixé par l'utilisateur |

Population de 6-11 ans déduite : élèves / (TBS / 100). Enfants hors de l'école : population − élèves (si positif).
Plan « pour changer de couleur » : toutes les composantes de la situation sont rapprochées ensemble d'un bon niveau
(25 % des meilleures valeurs observées) ; on retient le plus petit pas qui atteint la couleur visée, puis on le traduit
en enfants à scolariser, enseignants et écoles aux ratios actuels de la région.

## 7 bis. Investissement (`app/budget.R`)

Budget découpé en 80 tranches ; chaque tranche va à la combinaison région × action qui fait le plus baisser le SRD,
pondérée par la taille d'une cohorte d'âge (population 6-11 ans / 6) et par l'urgence (vert 1 ; jaune 1,3 ; orange 1,7 ; rouge 2,2).

| Action | Coût (hypothèse modifiable) | Effet (hypothèse modifiable) |
|---|---|---|
| Ouvrir une classe (salle + enseignant 1 an) | 15 M + 2,4 M FCFA | + 50 élèves accueillis, plafonné aux enfants hors de l'école |
| Maintien à l'école (cantine + kit) | 30 000 FCFA / élève / an | non-achèvement − 25 % si tous les élèves sont couverts |
| Remédiation CM1-CM2 | 15 000 FCFA / élève | + 10 points au CEP si tous les CM1-CM2 sont couverts |
| Actions pour les filles | 25 000 FCFA / fille / an | écart de parité − 60 % si toutes les filles sont couvertes |

Ces valeurs sont indicatives et doivent être remplacées par les coûts et les évaluations d'impact du ministère.

## 8. Référentiels

| Fichier | Contenu | Source |
|---|---|---|
| `data/referentiels/provinces_regions.csv` | 45 provinces et leur région | Découpage administratif 2016-2024 |
| `data/referentiels/alias_noms.csv` | Orthographes alternatives → clé normalisée | Constitué au fil du nettoyage |
| `data/referentiels/provinces_bfa.geojson` | Contours des 45 provinces | geoBoundaries gbOpen BFA ADM2 (wmgeolab, licence ouverte) |
| `data/referentiels/regions_bfa.geojson` | Contours des 13 régions (fusion des provinces) | dérivé du précédent |
