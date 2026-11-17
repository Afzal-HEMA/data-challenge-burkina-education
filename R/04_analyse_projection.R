# ============================================================
# Étape 4c : analyse - Projection 2026-2030 par région
# Entrées : indicateurs-region (étape 3), poids et seuils du SRD (04_analyse_srd.R)
# Sorties : indicateurs, SRD et niveau d'alerte projetés pour les 5 années à venir
#
# Méthode : tendance amortie, choisie par les données
#   Pour chaque indicateur et chaque région, à partir de la dernière année observée T :
#     pente b = (x_T - x_{T-3}) / 3            (tendance des 3 dernières années)
#     x_{T+h} = x_T + b x (phi + phi^2 + ... + phi^h)
#   phi (0 = pas de tendance, 1 = tendance prolongée telle quelle) est choisi
#   par validation sur le passé : on refait des « projections » depuis 2019,
#   2020... 2024, on les compare à ce qui s'est réellement passé, et on garde le
#   phi qui s'est le moins trompé (erreur absolue moyenne, horizons 1 à 5 ans).
#   Fourchette : 10e et 90e percentiles des erreurs passées, par horizon.
#   Les effectifs sont projetés en logarithme (évolution en %).
#
# Le SRD projeté réutilise les poids et les seuils de 04_analyse_srd.R ; chaque
# valeur projetée est située dans la distribution observée 2016-2025.
# Limite : une projection prolonge le passé ; elle ne prévoit pas un choc
# (réouvertures massives d'écoles, nouvelle dégradation sécuritaire).
# ============================================================

source(here::here("R", "fonctions_communes.R"))

HORIZON <- 5
PHIS <- seq(0, 1, by = 0.1)

ind <- charger_indicateurs() |>
  filter(niveau_zone == "region", sexe == "T") |>
  arrange(zone, annee)
derniere <- max(ind$annee)
annees_proj <- derniere + seq_len(HORIZON)

vars_proj <- intersect(c("tap", "tba", "tbs", "reussite_cep", "ips_tap", "eleves"), names(ind))
en_log <- c("eleves")
bornes <- list(tap = c(0, 120), tba = c(0, 200), tbs = c(0, 200), reussite_cep = c(0, 100),
               ips_tap = c(0, 2), eleves = c(0, Inf))

serie <- function(v) {
  ind |> select(zone, annee, x = all_of(v)) |>
    mutate(x = if (v %in% en_log) log(pmax(x, 1)) else x)
}

# ---- 1. Validation sur le passé : choix de phi -----------------
backtest <- function(s, phi) {
  origines <- sort(unique(s$annee))
  origines <- origines[origines - 3 >= min(origines) & origines < derniere]
  map_dfr(origines, \(o) {
    base <- s |> filter(annee %in% c(o, o - 3)) |>
      pivot_wider(names_from = annee, values_from = x, names_prefix = "a") |>
      rename(x_o = !!paste0("a", o), x_o3 = !!paste0("a", o - 3)) |>
      mutate(b = (x_o - x_o3) / 3)
    map_dfr(seq_len(HORIZON), \(hz) {
      if (o + hz > derniere) return(NULL)
      cumul <- sum(phi^(1:hz))
      base |>
        mutate(prevu = x_o + coalesce(b, 0) * cumul) |>
        inner_join(s |> filter(annee == o + hz) |> select(zone, reel = x), by = "zone") |>
        transmute(zone, origine = o, h = hz, erreur = reel - prevu)
    })
  })
}

choix <- map_dfr(vars_proj, \(v) {
  s <- serie(v)
  map_dfr(PHIS, \(phi) backtest(s, phi) |>
            summarise(MAE = mean(abs(erreur), na.rm = TRUE)) |>
            mutate(variable = v, phi = phi))
})
phi_retenu <- choix |> group_by(variable) |> slice_min(MAE, n = 1, with_ties = FALSE) |> ungroup()
cat("Amortissement retenu par indicateur (0 = pas de tendance, 1 = tendance complète) :\n")
print(phi_retenu |> select(variable, phi, MAE))

# ---- 2. Projections et fourchettes ----------------------------
projeter <- function(v) {
  s <- serie(v)
  phi <- phi_retenu$phi[phi_retenu$variable == v]
  err <- backtest(s, phi) |> group_by(h) |>
    summarise(e_bas = quantile(erreur, 0.10, na.rm = TRUE),
              e_haut = quantile(erreur, 0.90, na.rm = TRUE), n = n(), .groups = "drop")
  # horizons sans recul suffisant : on reprend l'horizon le plus long disponible
  err <- tibble(h = seq_len(HORIZON)) |> left_join(err, by = "h") |>
    fill(e_bas, e_haut, .direction = "down")
  base <- s |> filter(annee %in% c(derniere, derniere - 3)) |>
    pivot_wider(names_from = annee, values_from = x, names_prefix = "a") |>
    rename(x_T = !!paste0("a", derniere), x_T3 = !!paste0("a", derniere - 3)) |>
    mutate(b = coalesce((x_T - x_T3) / 3, 0))
  out <- base |>
    crossing(h = seq_len(HORIZON)) |>
    left_join(err, by = "h") |>
    mutate(annee = derniere + h,
           centre = x_T + b * map_dbl(h, \(k) sum(phi^(1:k))),
           bas = centre + e_bas, haut = centre + e_haut)
  if (v %in% en_log) out <- out |> mutate(across(c(centre, bas, haut), exp))
  lim <- bornes[[v]]
  out |>
    mutate(across(c(centre, bas, haut), \(x) pmin(pmax(x, lim[1]), lim[2]))) |>
    transmute(zone, annee, variable = v, centre, bas, haut)
}
proj_long <- map_dfr(vars_proj, projeter)

proj <- proj_long |>
  select(zone, annee, variable, centre) |>
  pivot_wider(names_from = variable, values_from = centre) |>
  left_join(proj_long |> filter(variable == "tap") |> select(zone, annee, tap_bas = bas, tap_haut = haut),
            by = c("zone", "annee"))

# ---- 3. Indicateurs dérivés (séries observées + projetées) ----
complet <- bind_rows(ind |> select(zone, annee, any_of(vars_proj)) |> mutate(type = "observé"),
                     proj |> mutate(type = "projection")) |>
  arrange(zone, annee) |>
  group_by(zone) |>
  mutate(non_achevement_relatif = if_else(tba > 0, 100 * (1 - tap / tba), NA_real_),
         evol_tap_3ans = tap - lag(tap, 3),
         evol_eleves_3ans = 100 * (eleves / lag(eleves, 3) - 1),
         ecart_parite_tap = abs(1 - ips_tap)) |>
  ungroup()

# ---- 4. SRD et alerte projetés --------------------------------
poids  <- lire_csv(dernier_fichier("04_analyse", "srd-composantes-poids"))
seuils <- lire_csv(dernier_fichier("04_analyse", "srd-seuils-matrice"))
obs <- complet |> filter(type == "observé")

# rang percentile d'une valeur dans la distribution observée 2016-2025
rang_hist <- function(v, sens, valeurs) {
  ref <- sens * obs[[v]]; ref <- ref[!is.na(ref)]
  map_dbl(sens * valeurs, \(x) if (is.na(x)) NA_real_ else mean(ref < x))
}
score_axe <- function(df, a) {
  p <- poids |> filter(axe == a)
  m <- sapply(seq_len(nrow(p)), \(i) rang_hist(p$variable[i], p$sens[i], df[[p$variable[i]]]))
  m <- matrix(m, nrow = nrow(df))
  w <- matrix(p$poids, nrow = nrow(m), ncol = ncol(m), byrow = TRUE)
  w[is.na(m)] <- 0; m[is.na(m)] <- 0
  100 * rowSums(m * w) / pmax(rowSums(w), 1e-9)
}
s_ <- function(axe, p) seuils$seuil[seuils$axe == axe & seuils$percentile == p]

projection <- complet |>
  filter(type == "projection") |>
  mutate(situation = score_axe(pick(everything()), "situation"),
         degradation = score_axe(pick(everything()), "degradation"),
         srd = 0.7 * situation + 0.3 * degradation,
         alerte = case_when(
           situation >= s_("situation", "P90") | (situation >= s_("situation", "P75") & degradation >= s_("degradation", "P50")) ~ "rouge",
           situation >= s_("situation", "P75") | (situation >= s_("situation", "P50") & degradation >= s_("degradation", "P75")) ~ "orange",
           situation >= s_("situation", "P50") | degradation >= s_("degradation", "P75") ~ "jaune",
           TRUE ~ "vert")) |>
  select(zone, annee, type, srd, alerte, situation, degradation, tap, tap_bas, tap_haut,
         any_of(c("tba", "tbs", "reussite_cep", "ips_tap", "eleves")),
         non_achevement_relatif, evol_tap_3ans, evol_eleves_3ans)

cat("\nAlertes projetées (nombre de régions) :\n")
print(projection |> count(annee, alerte) |> pivot_wider(names_from = alerte, values_from = n, values_fill = 0))
cat("\nAchèvement projeté en", max(annees_proj), ":\n")
print(projection |> filter(annee == max(annees_proj)) |> arrange(tap) |>
        select(zone, tap, tap_bas, tap_haut, alerte), n = Inf)

# ---- 5. Exports ------------------------------------------------
exporter(projection, "04_analyse", "scientist", "projection-region")
exporter(phi_retenu, "04_analyse", "scientist", "projection-amortissement-retenu")
exporter(choix, "04_analyse", "scientist", "projection-validation-passe")
