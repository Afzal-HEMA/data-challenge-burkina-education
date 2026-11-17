# ============================================================
# Étape 4a : analyse - Score de Risque de Décrochage (SRD) par région
# Entrée  : indicateurs-region (étape 3)
# Sorties : SRD par région et année, classement, test de sensibilité
#
# Le fichier DGESS ne contient pas de taux d'abandon : le SRD combine des
# signaux indirects de décrochage, tous disponibles par région et par année.
#
# Méthode :
#  1. Chaque composante est convertie en rang percentile (0-1), calculé
#     sur toutes les régions ET toutes les années : un score de 2025 est
#     comparable à un score de 2018.
#  2. Le sens est inversé pour les indicateurs « protecteurs »
#     (achèvement, scolarisation, réussite...) : plus haut = moins de risque.
#  3. SRD = moyenne pondérée x 100. Si une composante manque, les poids
#     des composantes disponibles sont renormalisés.
#  4. Niveaux d'alerte : médiane, 75e et 90e percentiles de toutes les années.
# ============================================================

source(here::here("R", "fonctions_communes.R"))

# ---- 1. Composantes, sens et poids --------------------------
# sens = +1 : plus la valeur est haute, plus le risque est élevé
# Version 2 (après examen des résultats sur les vraies données) :
#  - l'écart TBA - TAP est remplacé par 1 - TAP/TBA : l'écart rétrécissait
#    quand l'accès s'effondrait (Sahel, Est), ce qui masquait le risque ;
#  - les évolutions sur 1 an sont remplacées par des évolutions sur 3 ans,
#    moins sensibles aux rebonds ponctuels.
composantes <- tribble(
  ~variable,                 ~sens, ~poids, ~justification,
  "tap",                       -1,   0.30, "Taux d'achèvement faible = beaucoup d'enfants sortent avant la fin du primaire",
  "non_achevement_relatif",     1,   0.15, "Part des entrants au CP1 qui n'achèvent pas le primaire (1 - TAP/TBA)",
  "evol_tap_3ans",             -1,   0.15, "Un achèvement qui recule sur 3 ans signale une dégradation durable",
  "evol_eleves_3ans",          -1,   0.15, "Baisse des effectifs sur 3 ans = départs, fermetures d'écoles, déplacements",
  "tbs",                       -1,   0.10, "Faible scolarisation = enfants hors de l'école",
  "reussite_cep",              -1,   0.05, "Échec au CEP = risque de sortie sans diplôme",
  "ecart_parite_tap",           1,   0.10, "Écart filles/garçons à l'achèvement = un groupe décroche plus"
)

# ---- 2. Préparation ----------------------------------------
ind <- charger_indicateurs() |>
  filter(niveau_zone == "region", sexe == "T") |>
  mutate(ecart_parite_tap = abs(1 - ips_tap))

dispo <- composantes |>
  filter(variable %in% names(ind)) |>
  filter(map_lgl(variable, \(v) any(!is.na(ind[[v]]))))
absentes <- setdiff(composantes$variable, dispo$variable)
if (length(absentes)) message("Composantes absentes (poids redistribués) : ",
                              paste(absentes, collapse = ", "))

# ---- 3. Calcul du SRD --------------------------------------
calculer_srd <- function(df, comp) {
  scores <- map2_dfc(comp$variable, comp$sens, \(v, s) {
    tibble(!!paste0("s_", v) := percent_rank(s * df[[v]]))
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
  group_by(zone) |>
  arrange(annee, .by_group = TRUE) |>
  mutate(evol_srd = srd - lag(srd)) |>
  ungroup()

cat("\nSeuils d'alerte (SRD) : jaune >=", round(seuils[1], 1),
    "| orange >=", round(seuils[2], 1), "| rouge >=", round(seuils[3], 1), "\n")

# ---- 4. Test de sensibilité : poids égaux ------------------
res_egal <- calculer_srd(ind, mutate(dispo, poids = 1))
sensib <- res |>
  select(zone, annee, srd) |>
  left_join(res_egal |> select(zone, annee, srd_poids_egaux = srd), by = c("zone", "annee")) |>
  group_by(annee) |>
  summarise(correlation_rangs = cor(srd, srd_poids_egaux, method = "spearman",
                                    use = "complete.obs"), .groups = "drop")
cat("Robustesse (corrélation de Spearman avec des poids égaux) :\n")
print(sensib)

# ---- 5. Classement de la dernière année --------------------
derniere <- max(res$annee)
classement <- res |>
  filter(annee == derniere) |>
  arrange(desc(srd)) |>
  mutate(rang = row_number()) |>
  select(rang, zone, srd, alerte, evol_srd, any_of(c("tap", "non_achevement_relatif", "evol_tap_3ans", "evol_eleves_3ans")))
cat("\nClassement des régions en", derniere, ":\n")
print(classement, n = Inf)

# ---- 6. Exports --------------------------------------------
exporter(res |> select(zone, annee, srd, alerte, evol_srd, starts_with("s_"),
                       any_of(c("eleves", "nouveaux_cp1", "tba", "tbs", "tap", "reussite_cep",
                                "ips_tba", "ips_tbs", "ips_tap", "ecoles", "part_filles",
                                "evol_eleves", "evol_nouveaux_cp1", "evol_tap",
                                "ecart_acces_achevement", "non_achevement_relatif",
                                "evol_tap_3ans", "evol_eleves_3ans", "ecart_parite_tap"))),
         "04_analyse", "scientist", "srd-region-annee")
exporter(classement, "04_analyse", "analyst", paste0("classement-srd-", derniere))
exporter(dispo |> mutate(poids_effectif = poids / sum(poids)),
         "04_analyse", "scientist", "srd-composantes-poids")
exporter(sensib, "04_analyse", "scientist", "srd-sensibilite-poids")
