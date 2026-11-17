# ============================================================
# Étape 4a : analyse - Score de Risque de Décrochage (SRD)
# Entrée  : table d'indicateurs province x année (étape 3)
# Sorties : SRD par province et année, top 10, test de sensibilité
#
# Méthode :
#  1. Chaque composante est convertie en rang percentile (0-1), calculé
#     sur l'ensemble des provinces ET des années : un score de 2025 est
#     donc comparable à un score de 2018.
#  2. Le sens est inversé pour les indicateurs « protecteurs »
#     (achèvement, évolution des effectifs) : plus haut = moins de risque.
#  3. SRD = moyenne pondérée x 100. Si une composante manque, les poids
#     des composantes disponibles sont renormalisés.
#  4. Niveaux d'alerte : seuils fixés sur la distribution de toutes les
#     années (médiane, 75e et 90e percentiles).
# ============================================================

source(here::here("R", "fonctions_communes.R"))

# ---- 1. Composantes, sens et poids --------------------------
# sens = +1 : plus la valeur est haute, plus le risque est élevé
composantes <- tribble(
  ~variable,              ~sens, ~poids, ~justification,
  "taux_abandon",            1,   0.30, "Mesure directe du décrochage",
  "evol_abandon",            1,   0.10, "Une hausse récente signale une dégradation",
  "taux_achevement",        -1,   0.15, "Faible achèvement = sorties précoces cumulées",
  "taux_redoublement",       1,   0.10, "Le redoublement précède souvent l'abandon",
  "evol_effectif",          -1,   0.10, "Baisse des effectifs = départs, fermetures, déplacements",
  "eleves_par_maitre",       1,   0.05, "Encadrement insuffisant",
  "eleves_par_classe",       1,   0.05, "Classes surchargées",
  "ecart_parite",            1,   0.05, "Écart filles/garçons = exclusion d'un groupe",
  "part_ecoles_fermees",     1,   0.10, "Contexte sécuritaire (fermetures d'écoles)"
)

# ---- 2. Préparation ----------------------------------------
ind <- charger_indicateurs() |>
  filter(sexe == "T")
for (v in setdiff(INDICATEURS, names(ind))) ind[[v]] <- NA_real_   # colonnes absentes

ind <- ind |>
  arrange(province, annee) |>
  group_by(province) |>
  mutate(evol_abandon  = taux_abandon - lag(taux_abandon),
         evol_effectif = 100 * (effectif / lag(effectif) - 1)) |>
  ungroup() |>
  mutate(ecart_parite = abs(1 - indice_parite))

dispo <- composantes |>
  filter(variable %in% names(ind)) |>
  filter(map_lgl(variable, \(v) any(!is.na(ind[[v]]))))
absentes <- setdiff(composantes$variable, dispo$variable)
if (length(absentes)) message("Composantes absentes (poids redistribués) : ",
                              paste(absentes, collapse = ", "))

# ---- 3. Calcul du SRD --------------------------------------
calculer_srd <- function(df, comp) {
  scores <- map2_dfc(comp$variable, comp$sens, \(v, s) {
    r <- percent_rank(s * df[[v]])
    tibble(!!paste0("s_", v) := r)
  })
  poids <- matrix(comp$poids, nrow = nrow(df), ncol = nrow(comp), byrow = TRUE)
  m <- as.matrix(scores)
  poids[is.na(m)] <- 0
  m[is.na(m)] <- 0
  srd <- 100 * rowSums(m * poids) / rowSums(poids)
  srd[rowSums(poids) == 0] <- NA
  bind_cols(df, scores, srd = srd)
}

res <- calculer_srd(ind, dispo)

seuils <- quantile(res$srd, c(0.50, 0.75, 0.90), na.rm = TRUE)
res <- res |>
  mutate(alerte = cut(srd, c(-Inf, seuils, Inf),
                      labels = c("vert", "jaune", "orange", "rouge"))) |>
  group_by(province) |>
  mutate(evol_srd = srd - lag(srd)) |>
  ungroup()

cat("\nSeuils d'alerte (SRD) : jaune >=", round(seuils[1], 1),
    "| orange >=", round(seuils[2], 1), "| rouge >=", round(seuils[3], 1), "\n")

# ---- 4. Test de sensibilité : poids égaux ------------------
res_egal <- calculer_srd(ind, mutate(dispo, poids = 1))
sensib <- res |>
  select(province, annee, srd) |>
  left_join(res_egal |> select(province, annee, srd_poids_egaux = srd),
            by = c("province", "annee")) |>
  group_by(annee) |>
  summarise(correlation_rangs = cor(srd, srd_poids_egaux, method = "spearman",
                                    use = "complete.obs"),
            .groups = "drop")
cat("Robustesse (corrélation de Spearman avec des poids égaux) :\n")
print(sensib)

# ---- 5. Top 10 de la dernière année ------------------------
derniere <- max(res$annee)
top10 <- res |>
  filter(annee == derniere) |>
  slice_max(srd, n = 10) |>
  select(province, region, srd, alerte, evol_srd, any_of(c("taux_abandon", "taux_achevement")))
cat("\nTop 10 des provinces à risque en", derniere, ":\n")
print(top10)

# ---- 6. Exports --------------------------------------------
exporter(res |> select(province, region, annee, srd, alerte, evol_srd, starts_with("s_"),
                       any_of(c(INDICATEURS, "evol_abandon", "evol_effectif", "ecart_parite"))),
         "04_analyse", "scientist", "srd-province-annee")
exporter(top10, "04_analyse", "analyst", paste0("top10-srd-", derniere))
exporter(dispo |> mutate(poids_effectif = poids / sum(poids)),
         "04_analyse", "scientist", "srd-composantes-poids")
exporter(sensib, "04_analyse", "scientist", "srd-sensibilite-poids")
