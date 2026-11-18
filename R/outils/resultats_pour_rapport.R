# Affiche les résultats clés (agrégés par région, aucune donnée brute) pour rédiger la documentation.
# Usage : source("R/outils/resultats_pour_rapport.R")
suppressMessages({ source("R/fonctions_communes.R") })
options(width = 200, pillar.sigfig = 4)
ind <- lire_csv(dernier_fichier("03_transformation", "indicateurs-region"))
srd <- lire_csv(dernier_fichier("04_analyse", "srd-region-annee"))
pr  <- lire_csv(dernier_fichier("04_analyse", "projection-region"))
cmp <- lire_csv(dernier_fichier("04_analyse", "prevision-comparaison-modeles"))
fac <- lire_csv(dernier_fichier("04_analyse", "facteurs-decrochage"))

cat("\n== 1. National : décrochage sur le cycle et décrochage annuel apparent ==\n")
print(ind |> filter(zone == "National", sexe %in% c("T", "F", "G")) |>
  select(annee, sexe, tba, tap, taux_decrochage, decrochage_annuel_apparent) |>
  mutate(across(where(is.numeric) & !annee, \(x) round(x, 1))) |>
  pivot_wider(names_from = sexe, values_from = c(tba, tap, taux_decrochage, decrochage_annuel_apparent)), n = 20)

cat("\n== 2. Taux de décrochage par région (observé 2016, 2019, 2022, 2025 ; prédit 2026, 2027) ==\n")
obs <- srd |> filter(annee %in% c(2016, 2019, 2022, 2025)) |> select(zone, annee, taux_decrochage)
prd <- pr |> select(zone, annee, taux_decrochage)
print(bind_rows(obs, prd) |> mutate(taux_decrochage = round(taux_decrochage, 1)) |>
  pivot_wider(names_from = annee, values_from = taux_decrochage) |> arrange(desc(`2025`)), n = 20)

cat("\n== 3. Alerte, évolution sur 3 ans et fourchette de prédiction ==\n")
print(srd |> filter(annee == 2025) |> select(zone, alerte_2025 = alerte, evol_3ans = evol_decrochage_3ans, srd_2025 = srd) |>
  left_join(pr |> filter(annee == 2026) |> select(zone, alerte_2026 = alerte, dec_bas, dec_haut, modele_dec), by = "zone") |>
  mutate(across(where(is.numeric), \(x) round(x, 1))) |> arrange(desc(srd_2025)), n = 20)

cat("\n== 4. Nombre de régions par niveau d'alerte ==\n")
print(bind_rows(srd |> select(annee, alerte), pr |> select(annee, alerte)) |> count(annee, alerte) |>
  pivot_wider(names_from = alerte, values_from = n, values_fill = 0), n = 20)

cat("\n== 5. Comparaison des modèles de prédiction du décrochage ==\n")
print(cmp |> mutate(across(where(is.numeric), \(x) round(x, 2))), n = 20)

cat("\n== 6. Facteurs associés au décrochage ==\n")
print(fac |> select(modele, variable, coefficient, ic_bas, ic_haut, p_valeur, n, r2, signe_conforme) |>
  mutate(across(where(is.numeric), \(x) signif(x, 3))), n = 20)
