# ============================================================
# Étape 4d : analyse - Effet estimé des leviers (pour le simulateur)
# Entrée  : indicateurs-region (étape 3)
# Sortie  : leviers-parametres (paramètres utilisés par l'onglet Simulateur)
#
# Levier « écoles » : effet ESTIMÉ sur les données.
#   Question : quand le nombre d'écoles d'une région augmente de 1 %, de
#   combien augmentent ses effectifs ?
#   Modèle : variation annuelle de log(élèves) ~ variation de log(écoles)
#            + effet de chaque année (absorbe les chocs communs à tout le pays)
#   Le coefficient est une élasticité : 0,5 = +10 % d'écoles -> +5 % d'élèves.
#   Prudence : c'est une association observée 2016-2025 (ouvertures ET
#   fermetures liées à l'insécurité), pas une preuve causale.
#
# Levier « enseignants » : HYPOTHÈSE de capacité (le fichier DGESS ne contient
#   pas d'effectifs d'enseignants) : un enseignant de plus accueille N élèves
#   de plus, dans la limite des enfants non scolarisés. N est réglable dans
#   l'application.
#
# Leviers « maintien », « CEP », « parité » : objectifs fixés par l'utilisateur
#   (réduction du non-achèvement en %, points de réussite, réduction de
#   l'écart filles/garçons) ; leur effet sur le SRD est calculé, pas estimé.
# ============================================================

source(here::here("R", "fonctions_communes.R"))

ind <- charger_indicateurs() |>
  filter(niveau_zone == "region", sexe == "T", !is.na(ecoles), !is.na(eleves)) |>
  arrange(zone, annee) |>
  group_by(zone) |>
  mutate(dl_eleves = log(eleves) - lag(log(eleves)),
         dl_ecoles = log(ecoles) - lag(log(ecoles))) |>
  ungroup() |>
  filter(!is.na(dl_eleves), !is.na(dl_ecoles), is.finite(dl_eleves), is.finite(dl_ecoles))

m <- lm(dl_eleves ~ dl_ecoles + factor(annee), data = ind)
est <- summary(m)$coefficients["dl_ecoles", ]
ic <- confint(m)["dl_ecoles", ]
elasticite_brute <- unname(est["Estimate"])
# borne prudente : un effet négatif ou supérieur à 1,5 n'est pas plausible
elasticite <- min(max(elasticite_brute, 0), 1.5)

cat("Élasticité élèves / écoles :", round(elasticite_brute, 3),
    "| IC 95 % : [", round(ic[1], 3), ";", round(ic[2], 3), "]",
    "| p =", signif(est["Pr(>|t|)"], 2), "| n =", nrow(ind), "\n")
if (est["Pr(>|t|)"] > 0.05) message("Effet non significatif au seuil de 5 % : à présenter comme incertain.")
if (elasticite != elasticite_brute) message("Élasticité bornée à ", elasticite, " (valeur estimée hors plage plausible).")

parametres <- tibble(
  levier = c("ecoles", "enseignants"),
  parametre = c("elasticite_eleves_ecoles", "eleves_par_enseignant_defaut"),
  valeur = c(elasticite, 45),
  ic_bas = c(unname(ic[1]), NA), ic_haut = c(unname(ic[2]), NA),
  p_valeur = c(unname(est["Pr(>|t|)"]), NA), n = c(nrow(ind), NA),
  origine = c("estimé sur les données DGESS 2016-2025 (effets année)",
              "hypothèse réglable dans l'application (aucune donnée enseignants dans le fichier DGESS)")
)
print(parametres)
exporter(parametres, "04_analyse", "scientist", "leviers-parametres")
