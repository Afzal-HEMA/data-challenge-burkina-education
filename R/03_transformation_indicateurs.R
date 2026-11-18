# ============================================================
# Étape 3 : transformation - Indicateurs par région et par année
# Entrée  : table longue nettoyée (étape 2)
# Sorties :
#  a) indicateurs-region : une ligne = région (ou National) x année x sexe
#     -> entrée du SRD et du modèle (étape 4)
#  b) tableau-accueil    : format long pour l'onglet Accueil de l'application
#  c) passage-apparent   : flux d'élèves d'un niveau à l'autre (national)
#
# Indicateurs dérivés (le fichier DGESS ne donne pas de taux d'abandon) :
#  - ecart_acces_achevement = TBA - TAP (points)
#  - non_achevement_relatif = 100 x (1 - TAP / TBA) : part des enfants qui
#    entrent au CP1 mais n'achèvent pas le primaire (mesure indirecte du décrochage)
#  - evol_tap_3ans, evol_eleves_3ans : évolutions sur 3 ans
#  - evol_eleves, evol_nouveaux_cp1 : variation annuelle des effectifs (%)
#  - evol_tap : variation du taux d'achèvement (points)
#  - part_filles : part des filles dans les effectifs (%)
# ============================================================

source(here::here("R", "fonctions_communes.R"))

d <- charger_donnees_propres()

# ---- a) Indicateurs par région --------------------------------
base <- d |>
  filter(dimension == "ensemble" | (indicateur == "ecoles" & modalite == "Total")) |>
  mutate(variable = case_when(
    cycle == "prescolaire" & indicateur == "eleves" ~ "eleves_prescolaire",
    cycle == "prescolaire" & indicateur == "tbs"    ~ "tbs_prescolaire",
    cycle == "prescolaire" & indicateur == "ips_tbs" ~ "ips_tbs_prescolaire",
    TRUE ~ indicateur)) |>
  filter(cycle == "primaire" | variable %in% c("eleves_prescolaire", "tbs_prescolaire")) |>
  select(zone, niveau_zone, annee, sexe, variable, valeur)

ind <- base |>
  pivot_wider(names_from = variable, values_from = valeur) |>
  arrange(zone, sexe, annee) |>
  group_by(zone, sexe) |>
  mutate(
    evol_eleves       = 100 * (eleves / lag(eleves) - 1),
    evol_nouveaux_cp1 = 100 * (nouveaux_cp1 / lag(nouveaux_cp1) - 1),
    evol_tap          = tap - lag(tap),
    ecart_acces_achevement = tba - tap,
    # part des entrants (TBA) qui n'achèvent pas (TAP) : ne diminue pas
    # artificiellement quand l'accès s'effondre, contrairement à l'écart TBA - TAP
    non_achevement_relatif = if_else(tba > 0, 100 * (1 - tap / tba), NA_real_),
    # évolutions sur 3 ans : captent les effets durables (crise sécuritaire)
    evol_tap_3ans     = tap - lag(tap, 3),
    evol_eleves_3ans  = 100 * (eleves / lag(eleves, 3) - 1)
  ) |>
  ungroup()

# part des filles (calculée sur les effectifs, reportée sur la ligne T)
filles <- ind |> select(zone, annee, sexe, eleves) |>
  pivot_wider(names_from = sexe, values_from = eleves) |>
  transmute(zone, annee, sexe = "T", part_filles = 100 * F / T)
ind <- ind |> left_join(filles, by = c("zone", "annee", "sexe"))

# ---- b) Tableau de l'onglet Accueil ---------------------------
acc_ensemble <- base |>
  filter(sexe == "T") |>
  transmute(annee, zone, niveau_zone, dimension = "ensemble", modalite = "Total",
            indicateur = variable, valeur)

acc_sexe <- d |>
  filter(cycle == "primaire", indicateur == "eleves", dimension == "ensemble", sexe != "T") |>
  transmute(annee, zone, niveau_zone, dimension = "sexe",
            modalite = if_else(sexe == "F", "Filles", "Garçons"), indicateur = "eleves", valeur)

acc_statut <- d |>
  filter(cycle == "primaire", dimension == "statut", modalite != "Total", sexe == "T") |>
  transmute(annee, zone, niveau_zone, dimension = "statut", modalite, indicateur, valeur)

acc_milieu <- d |>
  filter(cycle == "primaire", dimension == "milieu", modalite != "Total", sexe == "T") |>
  transmute(annee, zone, niveau_zone, dimension = "milieu", modalite, indicateur, valeur)

# élèves par niveau : somme public + privé (le tableau ne donne pas le total par niveau)
acc_niveau <- d |>
  filter(cycle == "primaire", dimension == "statut_niveau", sexe == "T",
         modalite != "Total", modalite2 != "Total") |>
  group_by(annee, zone, niveau_zone, modalite = modalite2) |>
  summarise(valeur = if (all(is.na(valeur))) NA_real_ else sum(valeur, na.rm = TRUE), .groups = "drop") |>
  mutate(dimension = "niveau", indicateur = "eleves")
acc_presco <- base |>
  filter(variable == "eleves_prescolaire", sexe == "T") |>
  transmute(annee, zone, niveau_zone, dimension = "niveau", modalite = "Préscolaire",
            indicateur = "eleves", valeur)

accueil <- bind_rows(acc_ensemble, acc_sexe, acc_statut, acc_milieu, acc_niveau, acc_presco) |>
  select(annee, zone, niveau_zone, dimension, modalite, indicateur, valeur)

# ---- c) Passage apparent d'un niveau à l'autre (national) -----
# Élèves en niveau k+1 l'année N+1 / élèves en niveau k l'année N.
# Approximation (les redoublants ne sont pas distingués) : une valeur
# nettement inférieure à 1 signale des sorties du système.
niveaux <- c("CP1", "CP2", "CE1", "CE2", "CM1", "CM2")
eff_niv <- d |>
  filter(cycle == "primaire", dimension == "statut_niveau",
         modalite != "Total", modalite2 %in% niveaux) |>
  group_by(annee, sexe, niveau = modalite2) |>
  summarise(eleves = sum(valeur, na.rm = TRUE), .groups = "drop")
passage <- eff_niv |>
  mutate(rang = match(niveau, niveaux)) |>
  inner_join(eff_niv |> mutate(rang = match(niveau, niveaux) - 1L, annee = annee - 1L) |>
               select(annee, sexe, rang, eleves_suivant = eleves),
             by = c("annee", "sexe", "rang")) |>
  transmute(annee_depart = annee, sexe,
            transition = paste(niveau, "->", niveaux[rang + 1]),
            eleves_depart = eleves, eleves_arrivee = eleves_suivant,
            taux_passage_apparent = eleves_suivant / eleves)

# ---- Contrôles -------------------------------------------------
cat("Indicateurs région :", nrow(ind), "lignes ;",
    n_distinct(ind$zone[ind$niveau_zone == "region"]), "régions ;",
    paste(range(ind$annee), collapse = "-"), "\n")
manq <- ind |> filter(sexe == "T", niveau_zone == "region") |>
  summarise(across(c(eleves, tba, tbs, tap, reussite_cep, ips_tap), \(x) sum(is.na(x))))
cat("Valeurs manquantes (régions, total) :\n"); print(manq)

# ---- Exports ---------------------------------------------------
exporter(ind,     "03_transformation", "architect", "indicateurs-region")
exporter(accueil, "03_transformation", "architect", "tableau-accueil")
exporter(passage, "03_transformation", "analyst",   "passage-apparent-niveaux")

# ---- d) Établissements par statut et par région (filtre « statut » de la carte) ----
# Écoles primaires (P13), structures et salles du préscolaire (S07, S08).
# Les taux (TBA, TBS, TAP...) ne sont pas ventilés par statut dans le fichier DGESS.
etab <- d |>
  filter(indicateur %in% c("ecoles", "structures", "salles_classe"), sexe == "T") |>
  transmute(zone, niveau_zone, annee, cycle, indicateur, statut = modalite, valeur)
exporter(etab, "03_transformation", "architect", "etablissements-statut-region")
