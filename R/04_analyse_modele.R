# ============================================================
# Étape 4b : analyse - Prévision du taux d'abandon de l'année suivante
# Entrée  : table d'indicateurs province x année (étape 3)
# Sorties : fiabilité des modèles, prévisions, importance des variables
#
# Cible     : taux d'abandon de la province l'année N+1
# Variables : situation de l'année N (et évolution depuis N-1)
# Validation (conseil du jury) : apprentissage sur les cibles <= 2023,
#   test sur les cibles 2024 et 2025, jamais vues pendant l'apprentissage.
# Référence : modèle naïf « N+1 = N ». Un modèle utile doit faire mieux.
# ============================================================

source(here::here("R", "fonctions_communes.R"))
suppressPackageStartupMessages(library(ranger))
set.seed(2026)

ANNEE_FIN_APPRENTISSAGE <- 2023   # dernière année CIBLE vue à l'apprentissage

# ---- 1. Construction de la table d'apprentissage -----------
ind <- charger_indicateurs() |>
  filter(sexe == "T") |>
  arrange(province, annee)

candidates <- c("taux_abandon", "taux_redoublement", "taux_achevement",
                "eleves_par_maitre", "eleves_par_classe", "indice_parite",
                "part_ecoles_fermees")
vars_dispo <- candidates[candidates %in% names(ind)]
vars_dispo <- vars_dispo[map_lgl(vars_dispo, \(v) mean(is.na(ind[[v]])) < 0.5)]

tab <- ind |>
  group_by(province) |>
  mutate(
    cible         = lead(taux_abandon),
    annee_cible   = lead(annee),
    evol_abandon  = taux_abandon - lag(taux_abandon),
    evol_effectif = if ("effectif" %in% names(ind)) 100 * (effectif / lag(effectif) - 1) else NA_real_
  ) |>
  ungroup() |>
  mutate(region = factor(region))

predicteurs <- c(vars_dispo, "evol_abandon",
                 if (any(!is.na(tab$evol_effectif))) "evol_effectif", "region")
cat("Variables explicatives :", paste(predicteurs, collapse = ", "), "\n")

# Imputation simple par la médiane de l'apprentissage (documentée)
imputer <- function(df, ref) {
  for (v in setdiff(predicteurs, "region")) {
    df[[v]][is.na(df[[v]])] <- median(ref[[v]], na.rm = TRUE)
  }
  df
}

avec_cible <- tab |> filter(!is.na(cible))
appr <- avec_cible |> filter(annee_cible <= ANNEE_FIN_APPRENTISSAGE)
test <- avec_cible |> filter(annee_cible >  ANNEE_FIN_APPRENTISSAGE)
cat("Apprentissage :", nrow(appr), "lignes | Test :", nrow(test), "lignes",
    "(années cibles", paste(sort(unique(test$annee_cible)), collapse = ", "), ")\n")

appr_i <- imputer(appr, appr)
test_i <- imputer(test, appr)

# ---- 2. Modèles --------------------------------------------
formule <- reformulate(predicteurs, "cible")
m_lin <- lm(formule, data = appr_i)
m_rf  <- ranger(formule, data = appr_i, num.trees = 500,
                importance = "permutation", min.node.size = 5)

pred_test <- test_i |>
  mutate(naif     = taux_abandon,
         lineaire = predict(m_lin, newdata = test_i),
         foret    = predict(m_rf, data = test_i)$predictions)

# ---- 3. Fiabilité ------------------------------------------
# MAE  : erreur moyenne en points de pourcentage
# Top10: parmi les 10 provinces au plus fort abandon réel, combien
#        le modèle en a-t-il placé dans son propre top 10 ?
rappel_top10 <- function(reel, prevu) {
  length(intersect(order(-reel)[1:10], order(-prevu)[1:10])) / 10
}

fiabilite <- pred_test |>
  pivot_longer(c(naif, lineaire, foret), names_to = "modele", values_to = "prevu") |>
  group_by(modele, annee_cible) |>
  summarise(MAE  = mean(abs(prevu - cible)),
            RMSE = sqrt(mean((prevu - cible)^2)),
            rappel_top10 = rappel_top10(cible, prevu),
            .groups = "drop") |>
  arrange(annee_cible, MAE)
cat("\nFiabilité sur les années de test :\n")
print(fiabilite)

synthese <- fiabilite |> group_by(modele) |>
  summarise(across(c(MAE, RMSE, rappel_top10), mean)) |> arrange(MAE)
meilleur <- synthese$modele[1]
cat("\nMeilleur modèle (MAE moyenne) :", meilleur, "\n")
if (meilleur == "naif") message("Aucun modèle ne bat la référence naïve : à signaler honnêtement dans la documentation.")

# ---- 4. Modèle final et prévision de l'année suivante ------
tout_i <- imputer(avec_cible, avec_cible)
m_final_rf  <- ranger(formule, data = tout_i, num.trees = 500,
                      importance = "permutation", min.node.size = 5)
m_final_lin <- lm(formule, data = tout_i)

derniere <- max(tab$annee)
a_prevoir <- tab |> filter(annee == derniere) |> imputer(avec_cible)
prevision <- a_prevoir |>
  transmute(province, region,
            annee_base = annee,
            annee_prevue = annee + 1L,
            taux_abandon_actuel = taux_abandon,
            prevu_naif = taux_abandon,
            prevu_lineaire = predict(m_final_lin, newdata = a_prevoir),
            prevu_foret = predict(m_final_rf, data = a_prevoir)$predictions) |>
  mutate(prevu_retenu = case_when(meilleur == "foret" ~ prevu_foret,
                                  meilleur == "lineaire" ~ prevu_lineaire,
                                  TRUE ~ prevu_naif),
         modele_retenu = meilleur,
         erreur_moyenne_attendue = synthese$MAE[1]) |>
  arrange(desc(prevu_retenu))
cat("\nProvinces au plus fort abandon prévu en", derniere + 1, ":\n")
print(head(prevision |> select(province, region, taux_abandon_actuel, prevu_retenu), 10))

importance <- tibble(variable = names(m_final_rf$variable.importance),
                     importance = m_final_rf$variable.importance) |>
  arrange(desc(importance))
cat("\nVariables les plus explicatives (forêt aléatoire) :\n")
print(importance)

# ---- 5. Exports --------------------------------------------
exporter(fiabilite, "04_analyse", "scientist", "modele-fiabilite-test")
exporter(pred_test |> select(province, region, annee_cible, cible, naif, lineaire, foret),
         "04_analyse", "scientist", "modele-predictions-test")
exporter(prevision, "04_analyse", "scientist", paste0("prevision-abandon-", derniere + 1))
exporter(importance, "04_analyse", "scientist", "modele-importance-variables")
