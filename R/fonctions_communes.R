# ============================================================
# Fonctions communes à toutes les étapes de nettoyage
# Chargé par chaque script : source(here::here("R", "fonctions_communes.R"))
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(stringi)
  library(here)
})

# ---- 1. Clé de nom normalisée ------------------------------
# "Boucle du Mouhoun" -> "bouclemouhoun" ; "Kénédougou" -> "kenedougou"
nettoyer_cle <- function(x) {
  x |>
    enc2utf8() |>
    str_to_lower() |>
    stri_trans_general("Latin-ASCII") |>
    str_remove_all("\\b(du|de|des|la|le|province|region)\\b") |>
    str_remove_all("[^a-z]")
}

# ---- 2. Référentiel régions / provinces --------------------
# 13 anciennes régions et 45 provinces (découpage en vigueur 2016-2024).
charger_referentiel <- function() {
  provinces <- read_csv(here("data", "referentiels", "provinces_regions.csv"),
                        show_col_types = FALSE)
  alias <- read_csv(here("data", "referentiels", "alias_noms.csv"),
                    show_col_types = FALSE)

  ref <- bind_rows(
    provinces |> transmute(nom = province, niveau = "province", region),
    provinces |> distinct(region) |> transmute(nom = region, niveau = "region", region),
    tibble(nom = "National", niveau = "national", region = NA_character_)
  ) |>
    mutate(cle = nettoyer_cle(nom))

  stopifnot("Clés en double dans le référentiel" = !anyDuplicated(ref$cle))
  list(ref = ref, alias = alias)
}

REF <- charger_referentiel()

# Associe un nom brut à son entité officielle (nom, niveau, région)
identifier_entite <- function(x) {
  cle <- nettoyer_cle(x)
  cle <- coalesce(REF$alias$cle[match(cle, REF$alias$cle_alias)], cle)
  tibble(nom_brut = x, cle = cle) |>
    left_join(REF$ref, by = "cle")
}

# ---- 3. Conversion des valeurs ------------------------------
# "12 345" -> 12345 ; "4,5" -> 4.5 ; "4,5%" -> 4.5 ; "-", "ND", "" -> NA
nettoyer_valeur <- function(x) {
  x |>
    str_replace_all("[\\s  %]", "") |>
    str_replace(",", ".") |>
    na_if("-") |> na_if("ND") |> na_if("nd") |> na_if("") |> na_if("NA") |>
    as.numeric()
}

# ---- 4. Année scolaire -> année de fin ----------------------
# "2015-2016", "2015/2016", "2015-16" -> 2016
annee_fin <- function(x) {
  txt <- str_extract(x, "\\d{4}\\s*[-/]\\s*\\d{2,4}")
  as.integer(str_extract(txt, "^\\d{4}")) + 1L
}

# ---- 5. Lecteur générique de tableau "large" ---------------
# Forme attendue : 1re ligne de la plage = en-têtes
#   col 1 = entité (région / province / national), parfois fusionnée
#   col 2 = sexe (optionnelle : avec_sexe = TRUE)
#   colonnes suivantes = années scolaires
# Renvoie une table longue : entite, niveau, region, annee, sexe, valeur, indicateur
lire_tableau_large <- function(chemin, feuille, plage, indicateur,
                               avec_sexe = TRUE) {

  brut <- read_excel(chemin, sheet = feuille, range = plage,
                     col_names = FALSE, col_types = "text",
                     .name_repair = "minimal")

  entetes <- unlist(brut[1, ], use.names = FALSE)
  n_id    <- if (avec_sexe) 2 else 1
  annees  <- annee_fin(entetes[-(1:n_id)])
  if (any(is.na(annees))) {
    stop("En-têtes d'année non reconnus dans '", feuille, "' : ",
         paste(entetes[-(1:n_id)][is.na(annees)], collapse = " | "))
  }

  donnees <- brut[-1, ]
  names(donnees) <- c("entite_brut", if (avec_sexe) "sexe", paste0("a", annees))
  if (!avec_sexe) donnees$sexe <- "T"

  long <- donnees |>
    fill(entite_brut, .direction = "down") |>          # cellules fusionnées
    filter(!is.na(entite_brut)) |>
    mutate(sexe = case_when(
      str_detect(str_to_upper(sexe), "GAR|^G$|MASC") ~ "G",
      str_detect(str_to_upper(sexe), "FIL|^F$|FEM")  ~ "F",
      str_detect(str_to_upper(sexe), "TOT|^T$|ENS")  ~ "T",
      TRUE ~ NA_character_
    )) |>
    pivot_longer(starts_with("a"), names_to = "annee", values_to = "valeur_brut") |>
    mutate(annee  = as.integer(str_remove(annee, "^a")),
           valeur = nettoyer_valeur(valeur_brut))

  ent <- identifier_entite(unique(long$entite_brut))
  inconnus <- ent$nom_brut[is.na(ent$nom)]
  if (length(inconnus) > 0) {
    stop("Noms non reconnus dans '", feuille, "' : ",
         paste(inconnus, collapse = " | "),
         "\n -> ajoutez une ligne dans data/referentiels/alias_noms.csv")
  }
  if (any(is.na(long$sexe))) stop("Libellés de sexe non reconnus dans '", feuille, "'")

  long |>
    left_join(ent |> select(nom_brut, entite = nom, niveau, region),
              by = c("entite_brut" = "nom_brut")) |>
    transmute(indicateur, entite, niveau, region, annee, sexe, valeur)
}

# ---- 6. Contrôles de cohérence ------------------------------
# a) G + F = T (pour les effectifs, pas pour les taux)
controle_sexe <- function(df, tolerance = 1) {
  df |>
    filter(sexe %in% c("G", "F", "T")) |>
    pivot_wider(names_from = sexe, values_from = valeur) |>
    mutate(ecart = G + F - T) |>
    filter(abs(ecart) > tolerance)
}

# b) Somme des niveaux inférieurs = niveau supérieur (effectifs)
#    provinces -> région, régions -> national
controle_agregation <- function(df, tolerance = 1) {
  prov_vs_reg <- df |>
    filter(niveau == "province") |>
    group_by(indicateur, region, annee, sexe) |>
    summarise(somme = sum(valeur, na.rm = TRUE), .groups = "drop") |>
    inner_join(df |> filter(niveau == "region") |>
                 select(indicateur, region = entite, annee, sexe, publie = valeur),
               by = c("indicateur", "region", "annee", "sexe")) |>
    mutate(controle = "provinces -> region")

  reg_vs_nat <- df |>
    filter(niveau == "region") |>
    group_by(indicateur, annee, sexe) |>
    summarise(somme = sum(valeur, na.rm = TRUE), .groups = "drop") |>
    inner_join(df |> filter(niveau == "national") |>
                 select(indicateur, annee, sexe, publie = valeur),
               by = c("indicateur", "annee", "sexe")) |>
    mutate(controle = "regions -> national", region = "National")

  bind_rows(prov_vs_reg, reg_vs_nat) |>
    mutate(ecart = somme - publie,
           ok = abs(ecart) <= tolerance)
}

# ---- 7. Export avec nom conforme au challenge ---------------
# EQUIPE_ETAPE_DEFI_ROLE_description_vN.csv
EQUIPE <- "E00"   # <-- remplacer par votre numéro d'équipe
exporter <- function(df, etape, role, description, version = 1) {
  dossier <- here("sorties", etape)
  dir.create(dossier, showWarnings = FALSE, recursive = TRUE)
  etape_court <- str_replace(etape, "_", "-")
  nom <- sprintf("%s_%s_D1_%s_%s_v%d.csv", EQUIPE, etape_court, role, description, version)
  write_csv(df, file.path(dossier, nom), na = "")
  message("  -> ", file.path("sorties", etape, nom), " (", nrow(df), " lignes)")
  invisible(nom)
}

# ---- 8. Retrouver la dernière version d'un fichier produit -----
# ex. dernier_fichier("03_transformation", "indicateurs-province")
dernier_fichier <- function(etape, description) {
  f <- list.files(here("sorties", etape),
                  pattern = paste0("_", description, "_v\\d+\\.csv$"),
                  full.names = TRUE)
  if (length(f) == 0) return(NA_character_)
  v <- as.integer(str_match(basename(f), "_v(\\d+)\\.csv$")[, 2])
  f[which.max(v)]
}

# ---- 9. Charger la table d'indicateurs (contrat de l'étape 3) ---
# Variable d'environnement DONNEES_FICTIVES=1 -> utilise le jeu fictif
# (uniquement pour développer et tester, jamais pour les résultats).
charger_indicateurs <- function() {
  if (Sys.getenv("DONNEES_FICTIVES") == "1") {
    warning("*** DONNÉES FICTIVES : résultats sans valeur, pour test uniquement ***")
    return(read_csv(here("data", "fictif", "indicateurs_province_fictif.csv"),
                    col_types = TYPES_CSV, show_col_types = FALSE))
  }
  f <- dernier_fichier("03_transformation", "indicateurs-province")
  if (is.na(f)) stop("Table d'indicateurs absente : lancez d'abord l'étape 03_transformation.")
  message("Indicateurs lus dans : ", basename(f))
  read_csv(f, col_types = TYPES_CSV, show_col_types = FALSE)
}

# Attention : sans cela, read_csv lit le sexe "T" comme TRUE (logique)
TYPES_CSV <- cols(sexe = col_character(), province = col_character(),
                  region = col_character(), .default = col_guess())

# Colonnes attendues dans la table d'indicateurs (voir docs/dictionnaire_variables.md)
COLONNES_CLES <- c("province", "region", "annee", "sexe")
INDICATEURS <- c("effectif", "taux_abandon", "taux_redoublement", "taux_achevement",
                 "eleves_par_maitre", "eleves_par_classe", "indice_parite",
                 "part_ecoles_fermees")
