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

Une ligne = zone × année × sexe. Entrée du score de risque et des modèles de prédiction du décrochage.

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
| **`taux_decrochage`** | **Indicateur clé. Taux de décrochage sur le cycle primaire = 100 × (1 − TAP / TBA)** : part des enfants entrés au CP1 qui quittent l'école avant d'achever le primaire | % | calcul |
| `evol_decrochage`, `evol_decrochage_3ans` | Variation du taux de décrochage sur 1 an et sur 3 ans | points | calcul |
| `pop_6_11` | Population de 6-11 ans déduite : élèves / (TBS / 100) | enfants | calcul |
| `achevants` | Enfants qui achèvent le primaire : TAP / 100 × pop_6_11 / 6 | enfants | calcul |
| `decrocheurs_annuels` | Bilan des flux : élèves(N-1) + nouveaux CP1(N) − achevants(N-1) − élèves(N) | enfants | calcul |
| `decrochage_annuel_apparent` | décrocheurs annuels / (élèves(N-1) − achevants(N-1)) : contrôle croisé (attendu : 1 − (1 − annuel)^6 ≈ taux sur le cycle) | % | calcul |
| `evol_ecoles_3ans` | Variation du nombre d'écoles sur 3 ans | % | calcul |

### Pourquoi ce calcul du décrochage

Le fichier DGESS ne contient ni taux d'abandon ni redoublants. Le taux de décrochage est donc calculé à partir
de deux taux qu'il contient : sur 100 enfants admis au CP1 (TBA), seuls « TAP » achèvent le primaire ; l'écart
rapporté au TBA mesure la part qui décroche en cours de cycle (méthode dite du « taux d'abandon cumulé apparent »,
UNESCO-ISU). Limites : le TAP d'une année concerne des enfants entrés au CP1 environ 6 ans plus tôt ; les
redoublements et les migrations entre régions ne sont pas distingués. Le décrochage annuel apparent, calculé par
bilan des flux d'élèves, sert de contrôle de cohérence (même ordre de grandeur attendu).

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
| `situation` | Rang percentile (0-100) du taux de décrochage parmi toutes les régions et années 2016-2025 |
| `degradation` | Rang percentile (0-100) de la hausse du décrochage sur 3 ans |
| `srd` | Score de risque de décrochage = 70 % situation + 30 % dégradation (0-100) |
| `alerte` | Rouge : situation ≥ P90, ou ≥ P75 avec dégradation ≥ médiane. Orange : situation ≥ P75, ou ≥ médiane avec dégradation ≥ P75. Jaune : situation ≥ médiane ou dégradation ≥ P75. Sinon vert |

### Facteurs associés au décrochage (`04_analyse_leviers.R` → `facteurs-decrochage`)

Régression du taux de décrochage sur toutes les régions et années, avec un effet propre à chaque année :
modèle A (2016-2025) : TBS, réussite au CEP, écart de parité de l'achèvement, évolution des écoles et des effectifs sur 3 ans ;
modèle B : ajoute les élèves par enseignant (INSD, années disponibles). Colonnes : coefficient, IC 95 %, p, n, R²,
`signe_conforme` (signe attendu ?), `effet_retenu` (= coefficient si signe conforme, sinon 0). Les effets retenus
servent au diagnostic de la carte (contribution = effet × (valeur de la région − médiane des régions)), au simulateur
et à la répartition du budget. Ce sont des associations, pas des effets causaux prouvés.

## 6. Prédictions sur 2 ans (`04_analyse_projection.R`)

| Fichier | Contenu |
|---|---|
| `projection-region` | Région × année prédite (T) : `taux_decrochage` prédit, `dec_bas`/`dec_haut` (fourchette 10e-90e percentiles des erreurs passées), `modele_dec`, `erreur_moyenne_dec`, autres indicateurs, SRD, alerte ; TAP déduit = TBA × (1 − décrochage) |
| `projection-sexe` | Taux et effectifs prédits pour les filles (F) et les garçons (G) |
| `projection-statut` | Écoles primaires, structures et salles du préscolaire prédites par statut |
| `prevision-comparaison-modeles` | MAE, RMSE, sens correct de chaque modèle et horizon (validation sur le passé) |
| `prevision-modele-retenu` | Modèle retenu par horizon |
| `prevision-importance-variables` | Importance des variables (forêt aléatoire) dans l'évolution du décrochage |
| `prevision-validation-passe` | Toutes les prédictions passées et leurs erreurs |

Taux de décrochage (cible) : modèles naïf, tendance amortie, régression linéaire, forêt aléatoire. Pour chaque horizon
(1 et 2 ans) le modèle dont l'erreur moyenne (MAE) est la plus faible en validation sur le passé est retenu. Validation « comme en vrai » : chaque modèle n'apprend que sur ce qui était connu à
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
| Programme de maintien | décrochage × (1 − x %) | objectif fixé par l'utilisateur |
| Remédiation scolaire | réussite au CEP + x points → décrochage via l'effet estimé du CEP | objectif fixé par l'utilisateur |
| Actions pour les filles | écart de parité de l'achèvement × (1 − x %) → décrochage via l'effet estimé | objectif fixé par l'utilisateur |

Décrochage après mesures = décrochage + Σ effet_retenu(facteur) × variation du facteur (TBS, CEP, parité, écoles,
élèves par enseignant), puis × (1 − maintien %). TAP après = TBA × (1 − décrochage).

Population de 6-11 ans déduite : élèves / (TBS / 100). Enfants hors de l'école : population − élèves (si positif).
Plan « pour changer de couleur » : toutes les composantes de la situation sont rapprochées ensemble d'un bon niveau
(25 % des meilleures valeurs observées) ; on retient le plus petit pas qui atteint la couleur visée, puis on le traduit
en enfants à scolariser, enseignants et écoles aux ratios actuels de la région.

## 7 bis. Investissement (`app/budget.R`)

Budget découpé en 80 tranches ; chaque tranche va à la combinaison région × action qui fait le plus baisser le taux de décrochage,
pondérée par la taille d'une cohorte d'âge (population 6-11 ans / 6) et par l'urgence (vert 1 ; jaune 1,3 ; orange 1,7 ; rouge 2,2).

| Action | Coût (hypothèse modifiable) | Effet (hypothèse modifiable) |
|---|---|---|
| Ouvrir une classe (salle + enseignant 1 an) | 15 M + 2,4 M FCFA | + 50 élèves accueillis, plafonné aux enfants hors de l'école |
| Maintien à l'école (cantine + kit) | 30 000 FCFA / élève / an | décrochage − 25 % si tous les élèves sont couverts |
| Remédiation CM1-CM2 | 15 000 FCFA / élève | + 10 points au CEP si tous les CM1-CM2 sont couverts |
| Actions pour les filles | 25 000 FCFA / fille / an | écart de parité − 60 % si toutes les filles sont couvertes |

Rendements décroissants : effet × (1 − (1 − couverture)²), car les premiers élèves couverts sont les plus à risque.
Résultat principal : « enfants qui ne décrochent plus » = génération d'âge × baisse du décrochage (points) / 100.
Ces valeurs sont indicatives et doivent être remplacées par les coûts et les évaluations d'impact du ministère.

## 8. Référentiels

| Fichier | Contenu | Source |
|---|---|---|
| `data/referentiels/provinces_regions.csv` | 45 provinces et leur région | Découpage administratif 2016-2024 |
| `data/referentiels/alias_noms.csv` | Orthographes alternatives → clé normalisée | Constitué au fil du nettoyage |
| `data/referentiels/provinces_bfa.geojson` | Contours des 45 provinces | geoBoundaries gbOpen BFA ADM2 (wmgeolab, licence ouverte) |
| `data/referentiels/regions_bfa.geojson` | Contours des 13 régions (fusion des 45 provinces) et point d'étiquette de chaque région (point le plus éloigné de ses limites) | Natural Earth 10m admin-1 (domaine public), construit par `R/outils/construire_contours_regions.R` |
