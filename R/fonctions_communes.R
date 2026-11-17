# ============================================================
# Fonctions communes à toutes les étapes
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
    str_remove_all("\\b(du|de|des|la|le|province|region|regions)\\b") |>
    str_remove_all("[^a-z0-9]")
}

# ---- 2. Référentiel régions / provinces --------------------
# 13 anciennes régions et 45 provinces (découpage utilisé par la DGESS 2016-2025).
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
REGIONS <- REF$ref$nom[REF$ref$niveau == "region"]

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

# ---- 4. En-tête d'année -> année de fin d'année scolaire ----
# "2015-2016", "2015/2016", "2015/16", "2015-16" -> 2016
# "2016" (année seule, déjà l'année de fin)        -> 2016
# Seule la 1re ligne de la cellule est lue (texte parasite ignoré).
annee_fin <- function(x) {
  x <- str_extract(as.character(x), "^[^\n\r]*")
  plage <- str_extract(x, "\\d{4}\\s*[-/]\\s*\\d{2,4}")
  seule <- str_extract(x, "^\\s*\\d{4}(\\.0)?\\s*$")
  case_when(
    !is.na(plage) ~ as.integer(str_extract(plage, "^\\d{4}")) + 1L,
    !is.na(seule) ~ as.integer(str_extract(seule, "\\d{4}")),
    TRUE ~ NA_integer_
  )
}

# ---- 5. Libellés normalisés ---------------------------------
normaliser_sexe <- function(x) {
  k <- nettoyer_cle(x)
  case_when(
    k %in% c("g", "garcons", "garcon", "masculin", "m") ~ "G",
    k %in% c("f", "filles", "fille", "feminin")         ~ "F",
    k %in% c("t", "total", "ensemble")                  ~ "T",
    TRUE ~ NA_character_
  )
}

# Statut, milieu, niveau... : libellé propre et stable
normaliser_modalite <- function(x) {
  k <- nettoyer_cle(x)
  dico <- c(public = "Public", prive = "Privé", communautaire = "Communautaire",
            urbain = "Urbain", rural = "Rural", indetermine = "Indéterminé",
            total = "Total", totalgeneral = "Total", national = "Total",
            ensemble = "Total", burkinafaso = "Total",
            cp1 = "CP1", cp2 = "CP2", ce1 = "CE1", ce2 = "CE2",
            cm1 = "CM1", cm2 = "CM2",
            grandesection = "Grande section", moyennesection = "Moyenne section",
            petitesection = "Petite section")
  out <- unname(dico[k])
  if_else(is.na(out), str_squish(x), out)
}

# ---- 6. Lecteurs de tableaux ---------------------------------
lire_brut <- function(chemin, feuille, plage) {
  read_excel(chemin, sheet = feuille, range = plage, col_names = FALSE,
             col_types = "text", .name_repair = "minimal")
}

verifier_annees <- function(annees, entetes, feuille) {
  if (any(is.na(annees))) {
    stop("En-têtes d'année non reconnus dans '", feuille, "' : ",
         paste(str_trunc(as.character(entetes[is.na(annees)]), 30), collapse = " | "))
  }
}

# Forme A : colonnes d'identifiants puis une colonne par année.
#   Ligne 1 = en-têtes. Les identifiants fusionnés sont remplis vers le bas.
lire_bloc <- function(chemin, feuille, plage, ids) {
  brut <- lire_brut(chemin, feuille, plage)
  n_id <- length(ids)
  entetes <- unlist(brut[1, -(1:n_id)], use.names = FALSE)
  annees <- annee_fin(entetes)
  verifier_annees(annees, entetes, feuille)

  d <- brut[-1, ]
  names(d) <- c(ids, paste0("a", annees))
  d <- fill_fusions(d, ids)
  d |>
    pivot_longer(starts_with("a"), names_to = "annee", values_to = "valeur_brut") |>
    mutate(annee = as.integer(str_remove(annee, "^a")),
           valeur = nettoyer_valeur(valeur_brut)) |>
    select(-valeur_brut)
}

# Forme B : années sur 3 colonnes fusionnées (G / F / T).
#   Ligne 1 = années, ligne 2 = G F T, puis les données.
lire_bloc_gft <- function(chemin, feuille, plage, ids) {
  brut <- lire_brut(chemin, feuille, plage)
  n_id <- length(ids)
  ligne_an <- unlist(brut[1, -(1:n_id)], use.names = FALSE)
  ligne_sx <- unlist(brut[2, -(1:n_id)], use.names = FALSE)
  # l'année n'est écrite que dans la 1re des 3 cellules fusionnées
  annees <- vctrs::vec_fill_missing(annee_fin(ligne_an), direction = "down")
  verifier_annees(annees, ligne_an, feuille)
  sexes <- normaliser_sexe(ligne_sx)
  if (any(is.na(sexes))) stop("Ligne G/F/T non reconnue dans '", feuille, "'")

  d <- brut[-(1:2), ]
  names(d) <- c(ids, paste(annees, sexes, sep = "_"))
  d <- fill_fusions(d, ids)
  d |>
    pivot_longer(-all_of(ids), names_to = "cle", values_to = "valeur_brut") |>
    separate_wider_delim(cle, "_", names = c("annee", "sexe")) |>
    mutate(annee = as.integer(annee), valeur = nettoyer_valeur(valeur_brut)) |>
    select(-valeur_brut)
}

# Remplit les cellules fusionnées vers le bas (tous les identifiants sauf
# le dernier), sans déborder d'un bloc à l'autre : une ligne « Total ... »
# dans la 1re colonne n'est pas propagée aux lignes suivantes.
fill_fusions <- function(d, ids) {
  d <- d |> filter(if_any(everything(), \(x) !is.na(x)))
  if (length(ids) > 1) {
    # quand la 1re colonne change, les colonnes de niveau inférieur repartent à zéro
    for (k in seq_along(ids)[-length(ids)]) {
      v <- ids[k]
      d[[v]] <- vctrs::vec_fill_missing(d[[v]], direction = "down")
    }
  }
  d |> filter(if_any(all_of(ids), \(x) !is.na(x)))
}

# Rattache une colonne « zone » au référentiel (région / national).
# Gère « Total BOUCLE DU MOUHOUN » (sous-total) et « Total général ».
rattacher_zone <- function(d, feuille) {
  motif_st <- regex("^\\s*total\\s+(?!g[eé]n[eé]ral)", ignore_case = TRUE)
  d <- d |>
    mutate(est_sous_total = str_detect(zone, motif_st),
           zone = str_remove(zone, motif_st))
  ent <- identifier_entite(unique(d$zone))
  inconnus <- ent$nom_brut[is.na(ent$nom)]
  if (length(inconnus)) {
    stop("Zones non reconnues dans '", feuille, "' : ", paste(inconnus, collapse = " | "),
         "\n -> ajoutez une ligne dans data/referentiels/alias_noms.csv")
  }
  d |>
    left_join(ent |> select(nom_brut, zone_nom = nom, niveau_zone = niveau),
              by = c("zone" = "nom_brut")) |>
    mutate(zone = zone_nom) |>
    select(-zone_nom)
}

# ---- 7. Contrôles de cohérence ------------------------------
# a) G + F = T (pour les effectifs, pas pour les taux)
controle_sexe <- function(df, tolerance = 1) {
  df |>
    filter(sexe %in% c("G", "F", "T")) |>
    pivot_wider(names_from = sexe, values_from = valeur) |>
    filter(!is.na(G), !is.na(F), !is.na(T)) |>
    mutate(ecart = G + F - T) |>
    filter(abs(ecart) > tolerance)
}

# b) Somme des régions = National (pour les effectifs)
controle_regions_national <- function(df, tolerance = 1) {
  df |>
    filter(niveau_zone %in% c("region", "national")) |>
    group_by(tableau, indicateur, dimension, modalite, modalite2, sexe, annee) |>
    summarise(somme_regions = sum(valeur[niveau_zone == "region"], na.rm = TRUE),
              n_regions = sum(niveau_zone == "region" & !is.na(valeur)),
              national = if (any(niveau_zone == "national")) valeur[niveau_zone == "national"][1] else NA_real_,
              .groups = "drop") |>
    filter(!is.na(national), n_regions > 0) |>
    mutate(ecart = somme_regions - national, ok = abs(ecart) <= tolerance)
}

# ---- 8. Export avec nom conforme au challenge ---------------
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

# ---- 9. Retrouver la dernière version d'un fichier produit -----
dernier_fichier <- function(etape, description) {
  f <- list.files(here("sorties", etape),
                  pattern = paste0("_", description, "_v\\d+\\.csv$"),
                  full.names = TRUE)
  if (length(f) == 0) return(NA_character_)
  v <- as.integer(str_match(basename(f), "_v(\\d+)\\.csv$")[, 2])
  f[which.max(v)]
}

# Attention : sans forcer le texte, read_csv lit le sexe "T" comme TRUE (logique)
COLONNES_TEXTE <- c("sexe", "zone", "region", "modalite", "modalite2")
lire_csv <- function(f) {
  presentes <- intersect(COLONNES_TEXTE, names(read_csv(f, n_max = 0, show_col_types = FALSE)))
  types <- do.call(cols, c(setNames(rep(list(col_character()), length(presentes)), presentes),
                           list(.default = col_guess())))
  read_csv(f, col_types = types, show_col_types = FALSE)
}

# ---- 10. Accès aux tables des étapes 2 et 3 ------------------
# Variable d'environnement DONNEES_FICTIVES=1 -> jeu inventé (démonstration)
mode_fictif <- function() Sys.getenv("DONNEES_FICTIVES") == "1"

charger_donnees_propres <- function() {
  if (mode_fictif()) {
    warning("*** DONNÉES FICTIVES : résultats sans valeur, pour test uniquement ***")
    return(lire_csv(here("data", "fictif", "donnees_dgess_long_fictif.csv")))
  }
  f <- dernier_fichier("02_nettoyage", "donnees-dgess-long")
  if (is.na(f)) stop("Données nettoyées absentes : lancez R/02_nettoyage_dgess.R")
  message("Données nettoyées lues dans : ", basename(f))
  lire_csv(f)
}

charger_indicateurs <- function() {
  f <- dernier_fichier("03_transformation", "indicateurs-region")
  if (is.na(f)) stop("Table d'indicateurs absente : lancez R/03_transformation_indicateurs.R")
  message("Indicateurs lus dans : ", basename(f))
  lire_csv(f)
}
