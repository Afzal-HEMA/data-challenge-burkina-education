# ============================================================
# Étape 3b : transformation - Intégration des données de contexte externes
# Entrée  : data/externes/*.csv (format du modèle MODELE_contexte_regions.csv :
#           zone, annee, variable, valeur, source) + indicateurs-region (étape 3)
# Sorties : indicateurs-region (même fichier, colonnes ctx_* ajoutées),
#           contexte-sources (traçabilité), contexte-correlations (analyse)
# Sans fichier externe rempli, le script ne fait rien.
# Voir docs/sources_externes.md.
# ============================================================

source(here::here("R", "fonctions_communes.R"))

fichiers <- list.files(here("data", "externes"), pattern = "\\.csv$", full.names = TRUE)
fichiers <- fichiers[!str_detect(basename(fichiers), "^MODELE")]
if (!length(fichiers)) {
  message("Aucune donnée externe dans data/externes/ : étape ignorée.")
} else {
  brut <- map_dfr(fichiers, \(f) read_csv(f, col_types = cols(.default = col_character()),
                                          show_col_types = FALSE) |> mutate(fichier = basename(f)))
  ext <- brut |>
    mutate(valeur = nettoyer_valeur(valeur), annee = as.integer(annee),
           variable = str_trim(variable)) |>
    filter(!is.na(valeur), !is.na(annee))

  ent <- identifier_entite(unique(ext$zone))
  inconnus <- ent$nom_brut[is.na(ent$nom) | ent$niveau != "region"]
  if (length(inconnus)) stop("Régions non reconnues dans data/externes : ", paste(inconnus, collapse = " | "))
  ext <- ext |> left_join(ent |> select(nom_brut, zone_nom = nom), by = c("zone" = "nom_brut")) |>
    mutate(zone = zone_nom) |> select(-zone_nom)

  doublons <- ext |> count(zone, annee, variable) |> filter(n > 1)
  if (nrow(doublons)) { print(doublons); stop("Valeurs en double (même région, année, variable)") }
  if (any(is.na(ext$source) | ext$source == "")) warning("Certaines lignes n'ont pas de source : complétez la colonne source.")
  cat("Données externes :", nrow(ext), "valeurs,", n_distinct(ext$variable), "variables,",
      "années", paste(range(ext$annee), collapse = "-"), "\n")

  # ---- Indicateurs de contexte ----------------------------------
  ind <- charger_indicateurs()
  large <- ext |> select(zone, annee, variable, valeur) |> pivot_wider(names_from = variable, values_from = valeur)
  ctx <- large |>
    left_join(ind |> filter(sexe == "T", niveau_zone == "region") |> select(zone, annee, ecoles), by = c("zone", "annee")) |>
    transmute(zone, annee,
              ctx_pdi_pour_1000 = if (all(c("pdi", "population") %in% names(large))) 1000 * pdi / population else NA_real_,
              ctx_ecoles_fermees_pct = if ("ecoles_fermees" %in% names(large)) 100 * ecoles_fermees / ecoles else NA_real_,
              ctx_pauvrete_pct = if ("pauvrete_pct" %in% names(large)) pauvrete_pct else NA_real_) |>
    select(zone, annee, where(\(x) !all(is.na(x))))

  ind2 <- ind |> select(-starts_with("ctx_")) |>
    left_join(ctx, by = c("zone", "annee"))

  # ---- Analyse : le contexte explique-t-il la baisse de l'achèvement ? ----
  vars_ctx <- setdiff(names(ctx), c("zone", "annee"))
  correl <- map_dfr(vars_ctx, \(v) {
    d <- ind2 |> filter(sexe == "T", niveau_zone == "region", !is.na(.data[[v]]))
    map_dfr(c("evol_tap_3ans", "evol_eleves_3ans", "tap"), \(y) {
      dd <- d |> filter(!is.na(.data[[y]]))
      if (nrow(dd) < 6) return(NULL)
      ct <- cor.test(dd[[v]], dd[[y]], method = "spearman", exact = FALSE)
      tibble(contexte = v, indicateur = y, n = nrow(dd), rho_spearman = unname(ct$estimate), p_valeur = ct$p.value)
    })
  })
  cat("\nLien entre contexte et indicateurs scolaires (corrélation de rangs) :\n"); print(correl)

  # enrichit le fichier courant (même version) : pas de version périmée qui
  # masquerait une nouvelle exécution de l'étape 3
  v <- as.integer(str_match(basename(dernier_fichier("03_transformation", "indicateurs-region")), "_v(\\d+)\\.csv$")[, 2])
  exporter(ind2, "03_transformation", "architect", "indicateurs-region", version = v)
  exporter(ext |> distinct(fichier, variable, source) |> arrange(variable), "03_transformation", "engineer", "contexte-sources")
  exporter(correl, "03_transformation", "analyst", "contexte-correlations")
}
