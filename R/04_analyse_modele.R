# ============================================================
# Étape 4b : analyse - Prévision du taux d'achèvement de l'année suivante
# Entrée  : indicateurs-region (étape 3)
# Sorties : fiabilité des modèles, prévisions, importance des variables
#
# Cible : taux d'achèvement du primaire (TAP) de la région l'année N+1
#   (le fichier ne contient pas de taux d'abandon ; un TAP qui baisse =
#   plus d'enfants qui quittent l'école avant la fin du primaire).
#
# Version 2 : les modèles prédisent la VARIATION du TAP (N+1 - N), puis
#   prévision = TAP(N) + variation prévue. Avec 13 régions, prédire le niveau
#   revenait surtout à apprendre les écarts entre régions, déjà connus ; la
#   variation est ce qu'il faut anticiper.
#
# Modèles comparés :
#   naif      : variation = 0 (« l'an prochain = cette année »)
#   tendance  : variation = moitié de la variation annuelle moyenne des 3 dernières années
#   lineaire  : régression de la variation sur quelques signaux
#   foret     : forêt aléatoire sur tous les signaux
# Validation : apprentissage sur les cibles <= 2023, test sur 2024 et 2025.
# ============================================================

source(here::here("R", "fonctions_communes.R"))
suppressPackageStartupMessages(library(ranger))
set.seed(2026)

ANNEE_FIN_APPRENTISSAGE <- 2023   # dernière année CIBLE vue à l'apprentissage
K_ALERTE <- 3                     # « les 3 régions au plus faible achèvement »

# ---- 1. Table d'apprentissage -------------------------------
ind <- charger_indicateurs() |>
  filter(niveau_zone == "region", sexe == "T") |>
  arrange(zone, annee)

candidates <- c("tap", "evol_tap", "evol_tap_3ans", "evol_eleves", "evol_eleves_3ans",
                "evol_nouveaux_cp1", "non_achevement_relatif", "tba", "tbs",
                "reussite_cep", "ips_tap", "part_filles")
predicteurs <- candidates[candidates %in% names(ind)]
predicteurs <- predicteurs[map_lgl(predicteurs, \(v) mean(is.na(ind[[v]])) < 0.5)]
pred_lineaire <- intersect(c("evol_tap", "evol_tap_3ans", "evol_eleves_3ans", "non_achevement_relatif"),
                           predicteurs)
cat("Variables (forêt) :", paste(predicteurs, collapse = ", "), "\n")
cat("Variables (linéaire) :", paste(pred_lineaire, collapse = ", "), "\n")

tab <- ind |>
  group_by(zone) |>
  mutate(cible = lead(tap), annee_cible = lead(annee),
         variation = cible - tap) |>
  ungroup()

imputer <- function(df, ref) {
  for (v in predicteurs) df[[v]][is.na(df[[v]])] <- median(ref[[v]], na.rm = TRUE)
  df
}
tendance <- function(df) {
  t3 <- if ("evol_tap_3ans" %in% names(df)) df$evol_tap_3ans / 3 else df$evol_tap
  0.5 * coalesce(t3, df$evol_tap, 0)
}
borner <- function(x) pmin(pmax(x, 0), 150)

avec_cible <- tab |> filter(!is.na(cible), !is.na(tap))
appr <- avec_cible |> filter(annee_cible <= ANNEE_FIN_APPRENTISSAGE)
test <- avec_cible |> filter(annee_cible >  ANNEE_FIN_APPRENTISSAGE)
cat("Apprentissage :", nrow(appr), "lignes | Test :", nrow(test), "lignes",
    "(années cibles", paste(sort(unique(test$annee_cible)), collapse = ", "), ")\n")

appr_i <- imputer(appr, appr)
test_i <- imputer(test, appr)

# ---- 2. Modèles (sur la variation) --------------------------
m_lin <- lm(reformulate(pred_lineaire, "variation"), data = appr_i)
m_rf  <- ranger(reformulate(predicteurs, "variation"), data = appr_i, num.trees = 500,
                importance = "permutation", min.node.size = 5)

pred_test <- test_i |>
  mutate(naif     = tap,
         tendance = borner(tap + tendance(test)),
         lineaire = borner(tap + predict(m_lin, newdata = test_i)),
         foret    = borner(tap + predict(m_rf, data = test_i)$predictions))

# ---- 3. Fiabilité ------------------------------------------
# MAE : erreur moyenne en points de pourcentage
# rappel_alerte : parmi les K régions au plus faible achèvement réel,
#                 combien le modèle en a-t-il placé dans ses K plus faibles ?
# sens_correct : part des régions dont le sens de variation (hausse/baisse)
#                est bien prévu (non défini pour le modèle naïf)
rappel_alerte <- function(reel, prevu, k = K_ALERTE) {
  length(intersect(order(reel)[1:k], order(prevu)[1:k])) / k
}

fiabilite <- pred_test |>
  pivot_longer(c(naif, tendance, lineaire, foret), names_to = "modele", values_to = "prevu") |>
  group_by(modele, annee_cible) |>
  summarise(MAE  = mean(abs(prevu - cible)),
            RMSE = sqrt(mean((prevu - cible)^2)),
            rappel_alerte = rappel_alerte(cible, prevu),
            sens_correct = if (first(modele) == "naif") NA_real_ else
                             mean(sign(prevu - tap) == sign(cible - tap)),
            .groups = "drop") |>
  arrange(annee_cible, MAE)
cat("\nFiabilité sur les années de test :\n")
print(fiabilite)

synthese <- fiabilite |> group_by(modele) |>
  summarise(across(c(MAE, RMSE, rappel_alerte, sens_correct), mean)) |> arrange(MAE)
meilleur <- synthese$modele[1]
cat("\nMeilleur modèle (MAE moyenne sur 2024-2025) :", meilleur, "\n")
print(synthese)
if (meilleur == "naif") message("Aucun modèle ne bat la référence naïve : à signaler honnêtement dans la documentation.")

# ---- 4. Modèles finaux et prévision de l'année suivante ----
tout_i <- imputer(avec_cible, avec_cible)
m_final_rf  <- ranger(reformulate(predicteurs, "variation"), data = tout_i, num.trees = 500,
                      importance = "permutation", min.node.size = 5)
m_final_lin <- lm(reformulate(pred_lineaire, "variation"), data = tout_i)

derniere <- max(tab$annee)
base_prev <- tab |> filter(annee == derniere)
a_prevoir <- imputer(base_prev, avec_cible)
prevision <- a_prevoir |>
  transmute(zone, annee_base = annee, annee_prevue = annee + 1L,
            tap_actuel = tap,
            prevu_naif = tap,
            prevu_tendance = borner(tap + tendance(base_prev)),
            prevu_lineaire = borner(tap + predict(m_final_lin, newdata = a_prevoir)),
            prevu_foret = borner(tap + predict(m_final_rf, data = a_prevoir)$predictions))
prevision$prevu_retenu <- prevision[[paste0("prevu_", meilleur)]]
prevision <- prevision |>
  mutate(variation_prevue = prevu_retenu - tap_actuel,
         modele_retenu = meilleur,
         erreur_moyenne_attendue = synthese$MAE[1]) |>
  arrange(prevu_retenu)
cat("\nRégions au plus faible achèvement prévu en", derniere + 1, ":\n")
print(prevision |> select(zone, tap_actuel, prevu_retenu, variation_prevue), n = Inf)

importance <- tibble(variable = names(m_final_rf$variable.importance),
                     importance = m_final_rf$variable.importance) |>
  arrange(desc(importance))
cat("\nSignaux qui expliquent le plus la variation de l'achèvement (forêt aléatoire) :\n")
print(importance)

coefs <- tibble(variable = names(coef(m_final_lin)), coefficient = unname(coef(m_final_lin)))
cat("\nCoefficients du modèle linéaire (effet sur la variation du TAP, en points) :\n")
print(coefs)

# ---- 5. Exports --------------------------------------------
exporter(fiabilite, "04_analyse", "scientist", "modele-fiabilite-test")
exporter(synthese,  "04_analyse", "scientist", "modele-fiabilite-synthese")
exporter(pred_test |> select(zone, annee_cible, tap_base = tap, cible, naif, tendance, lineaire, foret),
         "04_analyse", "scientist", "modele-predictions-test")
exporter(prevision, "04_analyse", "scientist", paste0("prevision-achevement-", derniere + 1))
exporter(importance, "04_analyse", "scientist", "modele-importance-variables")
exporter(coefs, "04_analyse", "scientist", "modele-coefficients-lineaire")
