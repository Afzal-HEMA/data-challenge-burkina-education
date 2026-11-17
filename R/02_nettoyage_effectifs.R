# ============================================================
# Étape 2 : nettoyage - Effectifs du primaire par sexe et région
# Entrée  : feuille Excel "Feuil1", plage A1:L44 (Tableau 1)
# Sortie  : table longue propre (région, année, sexe, effectif)
# ============================================================

library(tidyverse)
library(readxl)
library(stringi)
library(here)

# ---- 1. Paramètres ----------------------------------------
chemin  <- here("data", "raw", "DGESS_prescolaire_primaire_2016_2025.xlsx")
feuille <- "Feuil1"
plage   <- "A2:L44"     # on commence à la ligne 2 : la ligne 1 est le titre fusionné

stopifnot("Fichier DGESS introuvable : copiez-le dans data/raw/ (voir README)" = file.exists(chemin))

# ---- 2. Table de référence des régions --------------------
# La clé = nom en minuscules, sans accents, sans espaces ni tirets.
# Si une région n'est pas reconnue, le script s'arrête et vous
# l'indique : ajoutez simplement son écriture dans `alias`.
ref_regions <- tibble(
  region = c("Boucle du Mouhoun", "Cascades", "Centre", "Centre-Est",
             "Centre-Nord", "Centre-Ouest", "Centre-Sud", "Est",
             "Hauts-Bassins", "Nord", "Plateau Central", "Sahel",
             "Sud-Ouest", "National"),
  cle    = c("bouclemouhoun", "cascades", "centre", "centreest",
             "centrenord", "centreouest", "centresud", "est",
             "hautsbassins", "nord", "plateaucentral", "sahel",
             "sudouest", "national")
)

alias <- tribble(
  ~cle_alias,        ~cle,
  "bouclemouhoun",   "bouclemouhoun",
  "hautbassins",     "hautsbassins",
  "plateaucentral",  "plateaucentral",
  "ensemble",        "national",
  "burkinafaso",     "national"
)

nettoyer_cle <- function(x) {
  x |>
    str_to_lower() |>
    stri_trans_general("Latin-ASCII") |>
    str_remove_all("\\bdu\\b|\\bde\\b|\\bdes\\b") |>   # "Boucle du Mouhoun" -> "bouclemouhoun"
    str_remove_all("[^a-z]")
}

# ---- 3. Fonction de lecture ------------------------------
lire_effectifs_sexe_region <- function(chemin, feuille, plage) {

  brut <- read_excel(chemin, sheet = feuille, range = plage,
                     col_names = FALSE, col_types = "text")

  # En-têtes : on extrait uniquement "AAAA-AAAA" (ou AAAA/AAAA, AAAA/AA)
  entetes <- brut[1, ] |> unlist(use.names = FALSE)
  annees_txt <- str_extract(entetes, "\\d{4}\\s*[-/]\\s*\\d{2,4}")
  annee_fin  <- as.integer(str_extract(annees_txt, "^\\d{4}")) + 1L   # 2015-2016 -> 2016

  donnees <- brut[-1, ]
  names(donnees) <- c("region", "sexe", paste0("a", annee_fin[-(1:2)]))

  donnees |>
    # la région n'est écrite que sur la 1re ligne de chaque bloc (cellules fusionnées)
    fill(region, .direction = "down") |>
    mutate(
      cle  = nettoyer_cle(region),
      sexe = case_when(
        str_detect(str_to_upper(sexe), "GAR") ~ "G",
        str_detect(str_to_upper(sexe), "FIL") ~ "F",
        str_detect(str_to_upper(sexe), "TOT") ~ "T",
        TRUE ~ NA_character_
      )
    ) |>
    mutate(cle = coalesce(alias$cle[match(cle, alias$cle_alias)], cle)) |>
    pivot_longer(starts_with("a"), names_to = "annee", values_to = "valeur") |>
    mutate(
      annee  = as.integer(str_remove(annee, "^a")),
      valeur = valeur |>
        str_replace_all("[\\s ]", "") |>      # espaces, y compris insécables
        str_replace(",", ".") |>
        na_if("-") |> na_if("ND") |> na_if("") |>
        as.numeric()
    ) |>
    select(-region) |>
    left_join(ref_regions, by = "cle") |>
    select(region, annee, sexe, valeur)
}

effectifs <- lire_effectifs_sexe_region(chemin, feuille, plage)

# ---- 4. Vérifications -------------------------------------
# a) Régions non reconnues
stopifnot("Régions non reconnues : complétez `alias`" = !any(is.na(effectifs$region)))
stopifnot("Sexe non reconnu" = !any(is.na(effectifs$sexe)))

# b) Garçons + Filles = Total
ctrl_sexe <- effectifs |>
  pivot_wider(names_from = sexe, values_from = valeur) |>
  mutate(ecart = G + F - T) |>
  filter(abs(ecart) > 1)
cat("Écarts G + F != Total :", nrow(ctrl_sexe), "lignes\n")

# c) Somme des 13 régions = National
ctrl_nat <- effectifs |>
  filter(sexe == "T") |>
  group_by(annee) |>
  summarise(somme_regions = sum(valeur[region != "National"]),
            national      = valeur[region == "National"]) |>
  mutate(ecart = somme_regions - national)
print(ctrl_nat)

# d) Valeurs manquantes
cat("Valeurs manquantes :", sum(is.na(effectifs$valeur)), "\n")

# ---- 5. Export --------------------------------------------
dir_sortie <- here("sorties", "02_nettoyage")
dir.create(dir_sortie, showWarnings = FALSE, recursive = TRUE)
write_csv(effectifs, file.path(dir_sortie, "effectifs_primaire_sexe_region.csv"))
write_csv(ctrl_nat,  file.path(dir_sortie, "controle_somme_regions_national.csv"))
