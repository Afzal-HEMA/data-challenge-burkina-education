# ============================================================
# Étape 4b : analyse - Prévision du taux d'achèvement de l'année suivante
# Entrée  : indicateurs-region (étape 3)
# Sorties : fiabilité des modèles, prévisions, importance des variables
#
# Cible     : taux d'achèvement du primaire (TAP) de la région l'année N+1
#             (le fichier ne contient pas de taux d'abandon ; un TAP qui
#             baisse = plus d'enfants qui quittent l'école avant la fin)
# Variables : situation de l'année N et évolution depuis N-1
# Validation (conseil du jury) : apprentissage sur les cibles <= 2023,
#   test sur les cibles 2024 et 2025, jamais vues pendant l'apprentissage.
# Référence : modèle naïf « N+1 = N ». Un modèle utile doit faire mieux.
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

candidates <- c("tap", "tba", "tbs", "evol_tap", "evol_eleves", "evol_nouveaux_cp1",
                "reussite_cep", "ips_tap", "ips_tbs", "ecart_acces_achevement", "part_filles")
predicteurs <- candidates[candidates %in% names(ind)]
predicteurs <- predicteurs[map_lgl(predicteurs, \(v) mean(is.na(ind[[v]])) < 0.5)]
# Modèle linéaire volontairement simple (peu d'observations : 13 régions)
pred_lineaire <- intersect(c("tap", "evol_tap", "ecart_acces_achevement", "evol_eleves"), predicteurs)
cat("Variables (forêt) :", paste(predicteurs, collapse = ", "), "\n")
cat("Variables (linéaire) :", paste(pred_lineaire, collapse = ", "), "\n")

tab <- ind |>
  group_by(zone) |>
  mutate(cible = lead(tap), annee_cible = lead(annee)) |>
  ungroup()

imputer <- function(df, ref) {
  for (v in predicteurs) df[[v]][is.na(df[[v]])] <- median(ref[[v]], na.rm = TRUE)
  df
}

avec_cible <- tab |> filter(!is.na(cible), !is.na(tap))
appr <- avec_cible |> filter(annee_cible <= ANNEE_FIN_APPRENTISSAGE)
test <- avec_cible |> filter(annee_cible >  ANNEE_FIN_APPRENTISSAGE)
cat("Apprentissage :", nrow(appr), "lignes | Test :", nrow(test), "lignes",
    "(années cibles", paste(sort(unique(test$annee_cible)), collapse = ", "), ")\n")

appr_i <- imputer(appr, appr)
test_i <- imputer(test, appr)

# ---- 2. Modèles --------------------------------------------
m_lin <- lm(reformulate(pred_lineaire, "cible"), data = appr_i)
m_rf  <- ranger(reformulate(predicteurs, "cible"), data = appr_i, num.trees = 500,
                importance = "permutation", min.node.size = 3)

pred_test <- test_i |>
  mutate(naif     = tap,
         lineaire = predict(m_lin, newdata = test_i),
         foret    = predict(m_rf, data = test_i)$predictions)

# ---- 3. Fiabilité ------------------------------------------
# MAE : erreur moyenne en points de pourcentage
# rappel_alerte : parmi les K régions au plus faible achèvement réel,
#                 combien le modèle en a-t-il placé dans ses K plus faibles ?
rappel_alerte <- function(reel, prevu, k = K_ALERTE) {
  length(intersect(order(reel)[1:k], order(prevu)[1:k])) / k
}

fiabilite <- pred_test |>
  pivot_longer(c(naif, lineaire, foret), names_to = "modele", values_to = "prevu") |>
  group_by(modele, annee_cible) |>
  summarise(MAE  = mean(abs(prevu - cible)),
            RMSE = sqrt(mean((prevu - cible)^2)),
            rappel_alerte = rappel_alerte(cible, prevu),
            .groups = "drop") |>
  arrange(annee_cible, MAE)
cat("\nFiabilité sur les années de test :\n")
print(fiabilite)

synthese <- fiabilite |> group_by(modele) |>
  summarise(across(c(MAE, RMSE, rappel_alerte), mean)) |> arrange(MAE)
meilleur <- synthese$modele[1]
cat("\nMeilleur modèle (MAE moyenne) :", meilleur, "\n")
if (meilleur == "naif") message("Aucun modèle ne bat la référence naïve : à signaler honnêtement dans la documentation.")

# ---- 4. Modèle final et prévision de l'année suivante ------
tout_i <- imputer(avec_cible, avec_cible)
m_final_rf  <- ranger(reformulate(predicteurs, "cible"), data = tout_i, num.trees = 500,
                      importance = "permutation", min.node.size = 3)
m_final_lin <- lm(reformulate(pred_lineaire, "cible"), data = tout_i)

derniere <- max(tab$annee)
a_prevoir <- tab |> filter(annee == derniere) |> imputer(avec_cible)
prevision <- a_prevoir |>
  transmute(zone, annee_base = annee, annee_prevue = annee + 1L,
            tap_actuel = tap,
            prevu_naif = tap,
            prevu_lineaire = predict(m_final_lin, newdata = a_prevoir),
            prevu_foret = predict(m_final_rf, data = a_prevoir)$predictions) |>
  mutate(prevu_retenu = case_when(meilleur == "foret" ~ prevu_foret,
                                  meilleur == "lineaire" ~ prevu_lineaire,
                                  TRUE ~ prevu_naif),
         variation_prevue = prevu_retenu - tap_actuel,
         modele_retenu = meilleur,
         erreur_moyenne_attendue = synthese$MAE[1]) |>
  arrange(prevu_retenu)
cat("\nRégions au plus faible achèvement prévu en", derniere + 1, ":\n")
print(prevision |> select(zone, tap_actuel, prevu_retenu, variation_prevue), n = Inf)

importance <- tibble(variable = names(m_final_rf$variable.importance),
                     importance = m_final_rf$variable.importance) |>
  arrange(desc(importance))
cat("\nVariables les plus explicatives (forêt aléatoire) :\n")
print(importance)

# ---- 5. Exports --------------------------------------------
exporter(fiabilite, "04_analyse", "scientist", "modele-fiabilite-test")
exporter(pred_test |> select(zone, annee_cible, cible, naif, lineaire, foret),
         "04_analyse", "scientist", "modele-predictions-test")
exporter(prevision, "04_analyse", "scientist", paste0("prevision-achevement-", derniere + 1))
exporter(importance, "04_analyse", "scientist", "modele-importance-variables")
