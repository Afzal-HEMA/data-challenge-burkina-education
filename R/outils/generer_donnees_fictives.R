# ============================================================
# OUTIL DE DÉVELOPPEMENT - DONNÉES FICTIVES
# Génère une table d'indicateurs au bon format (contrat de l'étape 3)
# pour tester le SRD, le modèle et l'application AVANT d'avoir
# les vraies données. Ces chiffres sont inventés : ne jamais les
# utiliser dans les résultats ni dans la documentation.
# ============================================================

source(here::here("R", "fonctions_communes.R"))
set.seed(2026)

prov <- read_csv(here("data", "referentiels", "provinces_regions.csv"), show_col_types = FALSE)
risque_base <- c(Sahel = 1.0, Est = 0.8, `Centre-Nord` = 0.7, Nord = 0.6,
                 `Boucle du Mouhoun` = 0.5)

fictif <- prov |>
  mutate(base = coalesce(risque_base[region], 0.2) + runif(n(), 0, 0.3)) |>
  crossing(annee = 2016:2025) |>
  mutate(
    choc = if_else(annee >= 2020, base * (annee - 2019) * 0.6, 0),
    taux_abandon      = pmax(0.5, 4 + 6 * base + choc + rnorm(n(), 0, 0.8)),
    taux_redoublement = pmax(0.5, 5 + 3 * base + rnorm(n(), 0, 1)),
    taux_achevement   = pmin(100, pmax(20, 70 - 25 * base - 2 * choc + rnorm(n(), 0, 4))),
    effectif          = round(30000 * exp(rnorm(n(), 0, 0.5)) * (1 - 0.03 * choc)),
    eleves_par_maitre = 40 + 15 * base + rnorm(n(), 0, 3),
    eleves_par_classe = 45 + 20 * base + rnorm(n(), 0, 4),
    indice_parite     = 1.02 - 0.12 * base + rnorm(n(), 0, 0.03),
    part_ecoles_fermees = if_else(annee >= 2019, pmax(0, choc * 4 + rnorm(n(), 0, 2)), 0),
    sexe = "T"
  ) |>
  select(all_of(c(COLONNES_CLES, INDICATEURS))) |>
  mutate(across(where(is.double), \(x) round(x, 2)))

dir.create(here("data", "fictif"), showWarnings = FALSE)
write_csv(fictif, here("data", "fictif", "indicateurs_province_fictif.csv"))
message("Jeu FICTIF écrit : ", nrow(fictif), " lignes")
