# ============================================================
# Étape 4c : analyse - Prédictions sur 2 ans par région (2026-2027)
# Entrées : indicateurs-region, etablissements-statut-region (étape 3),
#           poids et seuils du SRD (04_analyse_srd.R)
#
# 1. Taux d'achèvement (TAP, indicateur central) : COMPARAISON DE MODÈLES
#    pour chaque horizon (1 an, 2 ans). Les modèles prédisent la variation
#    du TAP entre l'année de départ et l'année visée :
#      naif     : aucune variation
#      tendance : tendance des 3 dernières années, amortie (phi choisi sur le passé)
#      lineaire : régression sur quelques variables explicatives
#      foret    : forêt aléatoire sur toutes les variables explicatives
#    Validation « comme en vrai » : pour chaque année de départ passée, on
#    n'entraîne que sur ce qui était connu à cette date, on prédit, puis on
#    compare à ce qui s'est réellement passé. Le modèle retenu pour chaque
#    horizon est celui qui s'est le moins trompé (erreur absolue moyenne).
#    Fourchette : 10e et 90e percentiles des erreurs du modèle retenu.
#
# 2. Autres indicateurs (TBA, TBS, CEP, parité, effectifs, préscolaire,
#    écoles par statut, valeurs par sexe) : tendance amortie, phi choisi par
#    validation sur le passé pour chaque indicateur.
#
# 3. SRD et niveau d'alerte recalculés pour chaque année prédite (mêmes
#    poids, mêmes seuils que les années observées).
#
# Limite : une prédiction prolonge les dynamiques passées ; elle ne prévoit
# pas un choc (réouvertures massives d'écoles, nouvelle dégradation).
# ============================================================

source(here::here("R", "fonctions_communes.R"))
source(here::here("R", "fonctions_srd.R"))
suppressPackageStartupMessages(library(ranger))
set.seed(2026)

HORIZON <- 2
PHIS <- seq(0, 1, by = 0.1)

ind_tout <- charger_indicateurs() |> filter(niveau_zone == "region") |> arrange(zone, sexe, annee)
ind <- ind_tout |> filter(sexe == "T")
derniere <- max(ind$annee)

# ============================================================
# A. Tendance amortie générique (une série = une région x un indicateur)
# ============================================================
backtest_tendance <- function(s, phi) {
  origines <- sort(unique(s$annee))
  origines <- origines[origines - 3 >= min(origines) & origines < derniere]
  map_dfr(origines, \(o) {
    base <- s |> filter(annee %in% c(o, o - 3)) |>
      pivot_wider(names_from = annee, values_from = x, names_prefix = "a")
    if (!all(paste0("a", c(o, o - 3)) %in% names(base))) return(NULL)
    base <- base |> rename(x_o = !!paste0("a", o), x_o3 = !!paste0("a", o - 3)) |>
      mutate(b = coalesce((x_o - x_o3) / 3, 0))
    map_dfr(seq_len(HORIZON), \(hz) {
      if (o + hz > derniere) return(NULL)
      cumul <- sum(phi^(1:hz))
      base |>
        mutate(prevu = x_o + b * cumul) |>
        inner_join(s |> filter(annee == o + hz) |> select(zone, reel = x), by = "zone") |>
        transmute(zone, origine = o, h = hz, erreur = reel - prevu)
    })
  })
}

projeter_tendance <- function(s, en_log = FALSE, bornes = c(-Inf, Inf)) {
  s <- s |> filter(!is.na(x))
  if (!nrow(s)) return(NULL)
  if (en_log) s <- s |> mutate(x = log(pmax(x, 1)))
  mae <- map_dbl(PHIS, \(phi) { b <- backtest_tendance(s, phi); if (nrow(b)) mean(abs(b$erreur), na.rm = TRUE) else NA_real_ })
  phi <- if (all(is.na(mae))) 0 else PHIS[which.min(mae)]
  bt <- backtest_tendance(s, phi)
  err <- if (nrow(bt)) bt |> group_by(h) |>
    summarise(e_bas = quantile(erreur, 0.10, na.rm = TRUE), e_haut = quantile(erreur, 0.90, na.rm = TRUE), .groups = "drop")
    else tibble(h = integer(), e_bas = numeric(), e_haut = numeric())
  err <- tibble(h = seq_len(HORIZON)) |> left_join(err, by = "h") |>
    mutate(e_bas = coalesce(e_bas, 0), e_haut = coalesce(e_haut, 0))
  base <- s |> filter(annee %in% c(derniere, derniere - 3)) |>
    pivot_wider(names_from = annee, values_from = x, names_prefix = "a")
  if (!paste0("a", derniere) %in% names(base)) return(NULL)
  if (!paste0("a", derniere - 3) %in% names(base)) base[[paste0("a", derniere - 3)]] <- NA_real_
  out <- base |>
    rename(x_T = !!paste0("a", derniere), x_T3 = !!paste0("a", derniere - 3)) |>
    mutate(b = coalesce((x_T - x_T3) / 3, 0)) |>
    crossing(h = seq_len(HORIZON)) |>
    left_join(err, by = "h") |>
    mutate(annee = derniere + h,
           centre = x_T + b * map_dbl(h, \(k) sum(phi^(1:k))),
           bas = centre + e_bas, haut = centre + e_haut)
  if (en_log) out <- out |> mutate(across(c(centre, bas, haut), exp))
  out |>
    mutate(across(c(centre, bas, haut), \(x) pmin(pmax(x, bornes[1]), bornes[2]))) |>
    transmute(zone, annee, centre, bas, haut, phi = phi)
}

serie <- function(df, v) df |> select(zone, annee, x = all_of(v))

# ============================================================
# B. Taux d'achèvement : comparaison de modèles par horizon
# ============================================================
feat_lin <- intersect(c("evol_tap", "evol_tap_3ans", "evol_eleves_3ans", "non_achevement_relatif"), names(ind))
feat_rf  <- intersect(c("tap", "evol_tap", "evol_tap_3ans", "evol_eleves_3ans", "non_achevement_relatif",
                        "tba", "tbs", "reussite_cep", "ips_tap", "part_filles", "evol_nouveaux_cp1"), names(ind))

tab_h <- function(h) {
  ind |>
    group_by(zone) |>
    mutate(cible = lead(tap, h), variation = cible - tap) |>
    ungroup() |>
    filter(!is.na(evol_tap_3ans))           # les variables sur 3 ans existent à partir de 2019
}
imputer <- function(df, ref, vars) {
  for (v in vars) df[[v]][is.na(df[[v]])] <- median(ref[[v]], na.rm = TRUE)
  df
}
phi_tap <- {
  s <- serie(ind, "tap")
  ch <- map_dbl(PHIS, \(phi) mean(abs(backtest_tendance(s, phi)$erreur), na.rm = TRUE))
  PHIS[which.min(ch)]
}
predire_variation <- function(modele, appr, nouv, h) {
  switch(modele,
    naif = rep(0, nrow(nouv)),
    tendance = coalesce(nouv$evol_tap_3ans / 3, 0) * sum(phi_tap^(1:h)),
    lineaire = predict(lm(reformulate(feat_lin, "variation"), data = imputer(appr, appr, feat_lin)),
                       newdata = imputer(nouv, appr, feat_lin)),
    foret = predict(ranger(reformulate(feat_rf, "variation"), data = imputer(appr, appr, feat_rf),
                           num.trees = 300, min.node.size = 5, seed = 2026),
                    data = imputer(nouv, appr, feat_rf))$predictions)
}
MODELES <- c("naif", "tendance", "lineaire", "foret")

validation <- map_dfr(seq_len(HORIZON), \(h) {
  d <- tab_h(h)
  origines <- sort(unique(d$annee))
  origines <- origines[origines + h <= derniere]
  map_dfr(origines, \(o) {
    appr <- d |> filter(annee + h <= o, !is.na(variation))   # seulement ce qui était connu à la date o
    test <- d |> filter(annee == o, !is.na(cible))
    if (nrow(appr) < 13 || !nrow(test)) return(NULL)
    hz <- h
    map_dfr(MODELES, \(m) test |>
      transmute(zone, origine = o, modele = m, tap_depart = tap,
                prevu = tap + predire_variation(m, appr, test, hz), reel = cible) |>
      mutate(h = hz, .after = zone))
  })
}) |> mutate(erreur = reel - prevu)

comparaison <- validation |>
  group_by(h, modele) |>
  summarise(MAE = mean(abs(erreur)), RMSE = sqrt(mean(erreur^2)),
            sens_correct = if (first(modele) == "naif") NA_real_ else
              mean(sign(prevu - tap_depart) == sign(reel - tap_depart), na.rm = TRUE),
            n = n(), .groups = "drop") |>
  arrange(h, MAE)
cat("Comparaison des modèles (taux d'achèvement), validation sur le passé :\n"); print(comparaison)

retenu <- comparaison |> group_by(h) |> slice_min(MAE, n = 1, with_ties = FALSE) |> ungroup()
cat("\nModèle retenu par horizon :\n"); print(retenu |> select(h, modele, MAE))

pred_tap <- map_dfr(seq_len(HORIZON), \(h) {
  m <- retenu$modele[retenu$h == h]
  d <- tab_h(h)
  appr <- d |> filter(!is.na(variation))
  nouv <- d |> filter(annee == derniere)
  e <- validation |> filter(h == !!h, modele == m)
  nouv |>
    mutate(centre = tap + predire_variation(m, appr, nouv, h)) |>
    transmute(zone, annee = derniere + h,
              tap = pmin(pmax(centre, 0), 120),
              tap_bas = pmax(centre + quantile(e$erreur, 0.10), 0),
              tap_haut = pmin(centre + quantile(e$erreur, 0.90), 120),
              modele_tap = m, erreur_moyenne_tap = mean(abs(e$erreur)))
})

# Importance des variables (forêt, horizon 1) : quelles variables expliquent l'évolution ?
d1 <- tab_h(1) |> filter(!is.na(variation))
rf_imp <- ranger(reformulate(feat_rf, "variation"), data = imputer(d1, d1, feat_rf), num.trees = 500,
                 min.node.size = 5, importance = "permutation", seed = 2026)
importance <- tibble(variable = names(rf_imp$variable.importance), importance = rf_imp$variable.importance) |>
  arrange(desc(importance))
cat("\nVariables qui expliquent le plus l'évolution de l'achèvement :\n"); print(importance)

# ============================================================
# C. Autres indicateurs : tendance amortie
# ============================================================
bornes <- list(tap = c(0, 120), tba = c(0, 200), tbs = c(0, 200), reussite_cep = c(0, 100),
               ips_tap = c(0, 2), tbs_prescolaire = c(0, 100))
en_log <- c("eleves", "nouveaux_cp1", "eleves_prescolaire")

projeter_var <- function(df, v) {
  if (!v %in% names(df) || all(is.na(df[[v]]))) return(NULL)
  p <- projeter_tendance(serie(df, v), en_log = v %in% en_log, bornes = bornes[[v]] %||% c(0, Inf))
  if (is.null(p)) NULL else p |> mutate(variable = v)
}

vars_T <- intersect(c("tba", "tbs", "reussite_cep", "ips_tap", "eleves", "nouveaux_cp1",
                      "eleves_prescolaire", "tbs_prescolaire"), names(ind))
proj_T <- map_dfr(vars_T, \(v) projeter_var(ind, v))
phi_retenu <- proj_T |> distinct(variable, phi)

vars_sexe <- c("tap", "tba", "tbs", "reussite_cep", "eleves", "eleves_prescolaire", "tbs_prescolaire")
proj_sexe <- map_dfr(c("F", "G"), \(sx) {
  df <- ind_tout |> filter(sexe == sx)
  map_dfr(vars_sexe, \(v) projeter_var(df, v)) |> mutate(sexe = sx)
}) |> select(zone, annee, sexe, variable, valeur = centre)

etab <- lire_csv(dernier_fichier("03_transformation", "etablissements-statut-region")) |>
  filter(niveau_zone == "region")
proj_statut <- etab |>
  group_by(cycle, indicateur, statut) |>
  group_modify(\(g, k) {
    p <- projeter_tendance(g |> select(zone, annee, x = valeur), en_log = TRUE, bornes = c(0, Inf))
    if (is.null(p)) tibble() else p |> select(zone, annee, valeur = centre)
  }) |>
  ungroup()

# ============================================================
# D. Années prédites : indicateurs, SRD et alerte
# ============================================================
pred <- proj_T |> select(zone, annee, variable, centre) |>
  pivot_wider(names_from = variable, values_from = centre) |>
  left_join(pred_tap |> select(zone, annee, tap), by = c("zone", "annee"))

complet <- bind_rows(ind |> select(zone, annee, tap, any_of(vars_T)) |> mutate(type = "observé"),
                     pred |> mutate(type = "prédiction")) |>
  arrange(zone, annee) |>
  group_by(zone) |>
  mutate(non_achevement_relatif = if_else(tba > 0, 100 * (1 - tap / tba), NA_real_),
         evol_tap_3ans = tap - lag(tap, 3),
         evol_eleves_3ans = 100 * (eleves / lag(eleves, 3) - 1),
         ecart_parite_tap = abs(1 - ips_tap)) |>
  ungroup()

poids  <- lire_csv(dernier_fichier("04_analyse", "srd-composantes-poids"))
seuils <- lire_csv(dernier_fichier("04_analyse", "srd-seuils-matrice"))
ref_obs <- complet |> filter(type == "observé")

prediction <- complet |>
  filter(type == "prédiction") |>
  scorer_srd(ref_obs, poids, seuils) |>
  left_join(pred_tap |> select(zone, annee, tap_bas, tap_haut, modele_tap, erreur_moyenne_tap),
            by = c("zone", "annee")) |>
  select(zone, annee, type, srd, alerte, situation, degradation, tap, tap_bas, tap_haut,
         modele_tap, erreur_moyenne_tap, any_of(vars_T), non_achevement_relatif,
         evol_tap_3ans, evol_eleves_3ans, ecart_parite_tap)

cat("\nAlertes prédites (nombre de régions) :\n")
print(prediction |> count(annee, alerte) |> pivot_wider(names_from = alerte, values_from = n, values_fill = 0))
cat("\nTaux d'achèvement prédit :\n")
print(prediction |> select(zone, annee, tap, tap_bas, tap_haut, alerte) |> arrange(annee, tap), n = Inf)

# ---- Exports ------------------------------------------------
exporter(prediction, "04_analyse", "scientist", "projection-region")
exporter(proj_sexe, "04_analyse", "scientist", "projection-sexe")
exporter(proj_statut, "04_analyse", "scientist", "projection-statut")
exporter(comparaison, "04_analyse", "scientist", "prevision-comparaison-modeles")
exporter(retenu, "04_analyse", "scientist", "prevision-modele-retenu")
exporter(importance, "04_analyse", "scientist", "prevision-importance-variables")
exporter(validation, "04_analyse", "scientist", "prevision-validation-passe")
exporter(phi_retenu, "04_analyse", "scientist", "projection-amortissement-retenu")
