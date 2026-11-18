# ============================================================
# Étape 4a : analyse - Score de Risque de Décrochage (SRD) par région
# Entrée  : indicateurs-region (étape 3)
# Sorties : SRD par région et année, classement, test de sensibilité
#
# Le fichier DGESS ne contient pas de taux d'abandon : le SRD combine des
# signaux indirects de décrochage, tous disponibles par région et par année.
#
# Version 4 - le TAUX DE DÉCROCHAGE est l'indicateur central (défi 1) :
#  - SITUATION   : niveau du taux de décrochage (100 x (1 - TAP / TBA)) ;
#  - DÉGRADATION : hausse du taux de décrochage sur 3 ans.
#  Les autres variables (scolarisation, CEP, écoles, enseignants, parité...)
#  ne font plus partie du score : ce sont des FACTEURS EXPLICATIFS, dont le
#  lien avec le décrochage est estimé dans 04_analyse_leviers.R.
#
# Historique - version 3, matrice à deux axes :
#  - SITUATION   : où en est la région (achèvement, non-achèvement des
#                  entrants, scolarisation, réussite au CEP, parité) ;
#  - DÉGRADATION : vers où elle va (achèvement et effectifs sur 3 ans).
#  Pourquoi : en un seul score, une région effondrée puis stabilisée à un
#  niveau très bas (Sahel : TAP 11 % en 2025) ressortait « jaune », car sa
#  tendance récente compensait sa situation. Avec la matrice, une situation
#  critique déclenche l'alerte rouge quelle que soit la tendance.
#
# Méthode :
#  1. Chaque composante -> rang percentile (0-1) sur toutes les régions et
#     toutes les années (un score de 2025 est comparable à un score de 2018).
#     Sens inversé pour les indicateurs protecteurs (plus haut = moins de risque).
#  2. Score de situation et score de dégradation = moyennes pondérées x 100.
#     Si une composante manque, les poids disponibles sont renormalisés.
#  3. SRD = 0,7 x situation + 0,3 x dégradation (sert au classement).
#  4. Niveau d'alerte = règle de la matrice (seuils = percentiles de toutes
#     les années) :
#       rouge  : situation >= P90, ou situation >= P75 et dégradation >= P50
#       orange : situation >= P75, ou situation >= P50 et dégradation >= P75
#       jaune  : situation >= P50, ou dégradation >= P75
#       vert   : sinon
# ============================================================

source(here::here("R", "fonctions_communes.R"))

POIDS_SITUATION <- 0.7

# ---- 1. Composantes, axe, sens et poids --------------------
# sens = +1 : plus la valeur est haute, plus le risque est élevé
composantes <- tribble(
  ~axe,          ~variable,              ~sens, ~poids, ~justification,
  "situation",   "taux_decrochage",        1,   1.00, "Part des enfants entrés au CP1 qui quittent l'école avant d'achever le primaire",
  "degradation", "evol_decrochage_3ans",   1,   1.00, "Hausse du décrochage sur 3 ans : la situation se dégrade"
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

# ---- 3. Calcul des scores ----------------------------------
rangs <- function(df, comp) {
  map2_dfc(comp$variable, comp$sens, \(v, s) tibble(!!paste0("s_", v) := percent_rank(s * df[[v]])))
}
score_pondere <- function(scores, comp) {
  m <- as.matrix(scores[paste0("s_", comp$variable)])
  w <- matrix(comp$poids, nrow = nrow(m), ncol = ncol(m), byrow = TRUE)
  w[is.na(m)] <- 0; m[is.na(m)] <- 0
  out <- 100 * rowSums(m * w) / rowSums(w)
  out[rowSums(w) == 0] <- NA
  out
}
calculer <- function(df, comp) {
  sc <- rangs(df, comp)
  sit <- score_pondere(sc, filter(comp, axe == "situation"))
  deg <- score_pondere(sc, filter(comp, axe == "degradation"))
  bind_cols(df, sc, situation = sit, degradation = deg) |>
    mutate(srd = if_else(is.na(degradation), situation,
                         POIDS_SITUATION * situation + (1 - POIDS_SITUATION) * degradation))
}

res <- calculer(ind, dispo)

q_sit <- quantile(res$situation, c(0.50, 0.75, 0.90), na.rm = TRUE)
q_deg <- quantile(res$degradation, c(0.50, 0.75), na.rm = TRUE)
res <- res |>
  mutate(
    deg0 = coalesce(degradation, 0),
    alerte = case_when(
      situation >= q_sit[3] | (situation >= q_sit[2] & deg0 >= q_deg[1]) ~ "rouge",
      situation >= q_sit[2] | (situation >= q_sit[1] & deg0 >= q_deg[2]) ~ "orange",
      situation >= q_sit[1] | deg0 >= q_deg[2]                           ~ "jaune",
      !is.na(situation)                                                   ~ "vert"),
    alerte = factor(alerte, levels = c("vert", "jaune", "orange", "rouge"))
  ) |>
  select(-deg0) |>
  group_by(zone) |>
  arrange(annee, .by_group = TRUE) |>
  mutate(evol_srd = srd - lag(srd)) |>
  ungroup()

seuils <- tibble(axe = c(rep("situation", 3), rep("degradation", 2)),
                 percentile = c("P50", "P75", "P90", "P50", "P75"),
                 seuil = round(c(q_sit, q_deg), 1))
cat("\nSeuils de la matrice :\n"); print(seuils)

# ---- 4. Test de sensibilité ---------------------------------
# a) poids égaux à l'intérieur de chaque axe ; b) part de la situation 0,5 au lieu de 0,7
res_egal <- calculer(ind, mutate(dispo, poids = 1))
POIDS_SITUATION_REF <- POIDS_SITUATION
POIDS_SITUATION <- 0.5
res_50 <- calculer(ind, dispo)
POIDS_SITUATION <- POIDS_SITUATION_REF
sensib <- res |>
  select(zone, annee, srd) |>
  left_join(res_egal |> select(zone, annee, srd_poids_egaux = srd), by = c("zone", "annee")) |>
  left_join(res_50 |> select(zone, annee, srd_situation_50 = srd), by = c("zone", "annee")) |>
  group_by(annee) |>
  summarise(corr_poids_egaux = cor(srd, srd_poids_egaux, method = "spearman", use = "complete.obs"),
            corr_situation_50 = cor(srd, srd_situation_50, method = "spearman", use = "complete.obs"),
            .groups = "drop")
cat("Robustesse du classement (corrélation de Spearman) :\n")
print(sensib)

# ---- 5. Classement de la dernière année --------------------
derniere <- max(res$annee)
classement <- res |>
  filter(annee == derniere) |>
  arrange(desc(srd)) |>
  mutate(rang = row_number()) |>
  select(rang, zone, srd, alerte, situation, degradation,
         any_of(c("taux_decrochage", "evol_decrochage_3ans", "decrochage_annuel_apparent", "tap", "tbs")))
cat("\nClassement des régions en", derniere, ":\n")
print(classement, n = Inf, width = Inf)

# ---- 6. Exports --------------------------------------------
exporter(res |> select(zone, annee, srd, alerte, situation, degradation, evol_srd, starts_with("s_"),
                       any_of(c("eleves", "nouveaux_cp1", "tba", "tbs", "tap", "reussite_cep",
                                "ips_tba", "ips_tbs", "ips_tap", "ecoles", "part_filles",
                                "evol_eleves", "evol_nouveaux_cp1", "evol_tap",
                                "ecart_acces_achevement", "non_achevement_relatif",
                                "evol_tap_3ans", "evol_eleves_3ans", "ecart_parite_tap",
                                "taux_decrochage", "evol_decrochage", "evol_decrochage_3ans",
                                "decrochage_annuel_apparent", "evol_ecoles_3ans", "pop_6_11")),
                       starts_with("ctx_")),
         "04_analyse", "scientist", "srd-region-annee")
exporter(classement, "04_analyse", "analyst", paste0("classement-srd-", derniere))
exporter(dispo |> group_by(axe) |> mutate(poids_dans_axe = poids / sum(poids)) |> ungroup(),
         "04_analyse", "scientist", "srd-composantes-poids")
exporter(seuils, "04_analyse", "scientist", "srd-seuils-matrice")
exporter(sensib, "04_analyse", "scientist", "srd-sensibilite-poids")
