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

# ------------------------------------------------------------
# Tableau d'accueil FICTIF (contrat : voir docs/dictionnaire_variables.md)
# annee, zone, niveau_zone, dimension, modalite, indicateur, valeur
# ------------------------------------------------------------
niveaux <- c(Préscolaire = 0.06, CP1 = 0.20, CP2 = 0.18, CE1 = 0.17,
             CE2 = 0.16, CM1 = 0.15, CM2 = 0.14)

base_prov <- prov |>
  crossing(annee = 2016:2025) |>
  left_join(fictif |> select(province, annee, effectif, eleves_par_maitre, eleves_par_classe),
            by = c("province", "annee")) |>
  mutate(eleves = effectif,
         ecoles = round(eleves / runif(n(), 160, 240)),
         enseignants = round(eleves / eleves_par_maitre),
         salles_classe = round(eleves / eleves_par_classe),
         urbain = if_else(province == "Kadiogo", 0.9, if_else(province == "Houet", 0.6, runif(n(), 0.08, 0.25))),
         prive = if_else(province %in% c("Kadiogo", "Houet"), 0.55, runif(n(), 0.08, 0.2)),
         filles = runif(n(), 0.46, 0.51))

lignes_prov <- base_prov |>
  transmute(annee, zone = province, niveau_zone = "province", region,
            ensemble_ecoles = ecoles, ensemble_eleves = eleves,
            ensemble_enseignants = enseignants, ensemble_salles_classe = salles_classe,
            statut_ecoles_Public = round(ecoles * (1 - prive)), statut_ecoles_Privé = round(ecoles * prive),
            statut_eleves_Public = round(eleves * (1 - prive * 0.8)), statut_eleves_Privé = round(eleves * prive * 0.8),
            milieu_ecoles_Urbain = round(ecoles * urbain), milieu_ecoles_Rural = round(ecoles * (1 - urbain)),
            milieu_eleves_Urbain = round(eleves * urbain * 1.3), milieu_eleves_Rural = round(eleves - eleves * urbain * 1.3),
            sexe_eleves_Filles = round(eleves * filles), sexe_eleves_Garçons = round(eleves * (1 - filles)),
            !!!setNames(map(niveaux, \(p) expr(round(eleves * !!p))), paste0("niveau_eleves_", names(niveaux)))) |>
  pivot_longer(-c(annee, zone, niveau_zone, region), names_to = "cle", values_to = "valeur") |>
  separate_wider_regex(cle, c(dimension = "[a-z]+", "_", indicateur = "ecoles|eleves|enseignants|salles_classe",
                              "_?", modalite = ".*")) |>
  mutate(modalite = if_else(modalite == "", "Total", modalite))

accueil <- bind_rows(
  lignes_prov,
  lignes_prov |> group_by(annee, zone = region, dimension, modalite, indicateur) |>
    summarise(valeur = sum(valeur), .groups = "drop") |> mutate(niveau_zone = "region"),
  lignes_prov |> group_by(annee, dimension, modalite, indicateur) |>
    summarise(valeur = sum(valeur), .groups = "drop") |> mutate(zone = "National", niveau_zone = "national")
) |>
  select(annee, zone, niveau_zone, dimension, modalite, indicateur, valeur)

write_csv(accueil, here("data", "fictif", "tableau_accueil_fictif.csv"))
message("Tableau d'accueil FICTIF écrit : ", nrow(accueil), " lignes")
