# ============================================================
# OUTIL DE DÉVELOPPEMENT - DONNÉES FICTIVES
# Produit une table longue au format exact de l'étape 2
# (02_nettoyage_dgess.R) pour tester les étapes 3, 4 et l'application
# AVANT d'avoir les vraies données. Tous les chiffres sont inventés :
# ne jamais les utiliser dans les résultats ni dans la documentation.
# Usage : Sys.setenv(DONNEES_FICTIVES = "1"); source(ce script)
# ============================================================

source(here::here("R", "fonctions_communes.R"))
set.seed(2026)

risque <- c(Sahel = 1.0, Est = 0.85, `Centre-Nord` = 0.75, Nord = 0.6,
            `Boucle du Mouhoun` = 0.5, `Centre-Est` = 0.45, Cascades = 0.35,
            `Centre-Ouest` = 0.3, `Sud-Ouest` = 0.3, `Plateau Central` = 0.25,
            `Centre-Sud` = 0.2, `Hauts-Bassins` = 0.15, Centre = 0.05)
taille <- c(Centre = 420e3, `Hauts-Bassins` = 330e3, `Boucle du Mouhoun` = 260e3,
            `Centre-Est` = 230e3, `Centre-Ouest` = 240e3, Est = 210e3, Nord = 220e3,
            `Centre-Nord` = 200e3, `Plateau Central` = 140e3, `Sud-Ouest` = 120e3,
            `Centre-Sud` = 120e3, Cascades = 110e3, Sahel = 130e3)

reg <- tibble(zone = names(risque), r = risque, n0 = taille[names(risque)]) |>
  crossing(annee = 2016:2025) |>
  mutate(choc = if_else(annee >= 2019, r * (annee - 2018) / 7, 0),
         eleves = round(n0 * (1 + 0.025 * (annee - 2016)) * (1 - 0.5 * choc) * exp(rnorm(n(), 0, 0.02))),
         pf = 0.49 - 0.03 * r + rnorm(n(), 0, 0.004),
         nouveaux_cp1 = round(eleves * 0.21),
         tap = pmin(95, 72 - 25 * r - 18 * choc + rnorm(n(), 0, 2)),
         tba = pmin(120, 100 - 15 * r - 20 * choc + rnorm(n(), 0, 3)),
         tbs = pmin(110, 92 - 22 * r - 20 * choc + rnorm(n(), 0, 2.5)),
         reussite_cep = pmin(95, 72 - 8 * r - 6 * choc + rnorm(n(), 0, 3)),
         ips = 1.02 - 0.1 * r + rnorm(n(), 0, 0.02),
         ecoles = round(eleves / (180 + 40 * r)),
         prive = pmax(0.03, 0.35 - 0.3 * r + rnorm(n(), 0, 0.02)),
         presco = round(eleves * (0.06 - 0.04 * r)),
         tbs_presco = pmax(1, 9 - 6 * r + rnorm(n(), 0, 0.5)))

base <- function(d, tab, ind, cycle = "primaire", dim = "ensemble") {
  d |> mutate(tableau = tab, cycle = cycle, indicateur = ind, dimension = dim,
              niveau_zone = "region", modalite2 = NA_character_,
              modalite = if ("modalite" %in% names(d)) modalite else "Total")
}
# effectifs : G/F/T à partir du total et de la part des filles
effectif_sexe <- function(var, pf_decal = 0) {
  reg |> transmute(zone, annee, T = .data[[var]], F = round(T * (pf + pf_decal)), G = T - F) |>
    pivot_longer(c(G, F, T), names_to = "sexe", values_to = "valeur")
}
# taux : G et F de part et d'autre du total
taux_sexe <- function(var, ecart) {
  reg |> transmute(zone, annee, T = round(.data[[var]], 1),
                   G = round(T * (1 + ecart), 1), F = round(T * (1 - ecart), 1)) |>
    pivot_longer(c(G, F, T), names_to = "sexe", values_to = "valeur")
}
indice <- function(decal) reg |> transmute(zone, annee, sexe = "T", valeur = round(ips + decal, 2))

regions_long <- bind_rows(
  effectif_sexe("eleves")              |> base("P01", "eleves"),
  effectif_sexe("nouveaux_cp1")        |> base("P05", "nouveaux_cp1"),
  taux_sexe("tba", 0.03)               |> base("P06", "tba"),
  taux_sexe("tbs", 0.04)               |> base("P07", "tbs"),
  taux_sexe("tap", 0.05)               |> base("P08", "tap"),
  taux_sexe("reussite_cep", 0.01)      |> base("P09", "reussite_cep"),
  indice(0.02)                         |> base("P10", "ips_tba"),
  indice(0)                            |> base("P11", "ips_tbs"),
  indice(-0.03)                        |> base("P12", "ips_tap"),
  reg |> transmute(zone, annee, sexe = "T", Public = round(ecoles * (1 - prive)), Privé = round(ecoles * prive)) |>
    mutate(Total = Public + Privé) |>
    pivot_longer(c(Public, Privé, Total), names_to = "modalite", values_to = "valeur") |>
    base("P13", "ecoles", dim = "statut"),
  effectif_sexe("presco", 0.01)        |> base("S01", "eleves", "prescolaire"),
  taux_sexe("tbs_presco", 0.02)        |> base("S05", "tbs", "prescolaire")
)

# National : sommes pour les effectifs, moyennes pondérées pour les taux
poids_nat <- reg |> select(zone, annee, w = eleves)
national <- regions_long |>
  left_join(poids_nat, by = c("zone", "annee")) |>
  group_by(tableau, cycle, indicateur, dimension, modalite, modalite2, sexe, annee) |>
  summarise(valeur = if (first(indicateur) %in% c("eleves", "nouveaux_cp1", "ecoles"))
                       sum(valeur) else round(weighted.mean(valeur, w), 2), .groups = "drop") |>
  mutate(zone = "National", niveau_zone = "national")

# Tableaux nationaux ventilés : statut, milieu, statut x niveau
nat_el <- national |> filter(tableau == "P01") |> select(annee, sexe, total = valeur)
ventiler <- function(tab, dim, parts) {
  nat_el |> crossing(tibble(modalite = names(parts), p = parts)) |>
    mutate(valeur = round(total * p)) |>
    bind_rows(nat_el |> mutate(modalite = "Total", valeur = total)) |>
    transmute(tableau = tab, cycle = "primaire", indicateur = "eleves", dimension = dim,
              zone = "National", niveau_zone = "national", modalite, modalite2 = NA_character_,
              sexe, annee, valeur)
}
niv <- c(CP1 = 0.20, CP2 = 0.18, CE1 = 0.175, CE2 = 0.165, CM1 = 0.145, CM2 = 0.135)
statut_niveau <- nat_el |>
  crossing(tibble(modalite = c("Public", "Privé"), ps = c(0.8, 0.2))) |>
  crossing(tibble(modalite2 = names(niv), pn = niv)) |>
  mutate(valeur = round(total * ps * pn * exp(rnorm(n(), 0, 0.01)))) |>
  transmute(tableau = "P04", cycle = "primaire", indicateur = "eleves", dimension = "statut_niveau",
            zone = "National", niveau_zone = "national", modalite, modalite2, sexe, annee, valeur)

fictif <- bind_rows(regions_long, national,
                    ventiler("P02", "statut", c(Public = 0.8, Privé = 0.2)),
                    ventiler("P03", "milieu", c(Urbain = 0.27, Rural = 0.73)),
                    statut_niveau) |>
  select(tableau, cycle, indicateur, zone, niveau_zone, dimension, modalite, modalite2, sexe, annee, valeur)

dir.create(here("data", "fictif"), showWarnings = FALSE)
unlink(here("data", "fictif", c("indicateurs_province_fictif.csv", "tableau_accueil_fictif.csv")))
write_csv(fictif, here("data", "fictif", "donnees_dgess_long_fictif.csv"), na = "")
message("Jeu FICTIF écrit : ", nrow(fictif), " lignes (format de l'étape 2)")
