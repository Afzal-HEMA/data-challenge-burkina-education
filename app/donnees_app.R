# ============================================================
# Préparation des données de l'application (testable sans Shiny)
# Lit les dernières versions des sorties de l'étape 4.
# ============================================================

source(here::here("R", "fonctions_communes.R"))
suppressPackageStartupMessages(library(sf))

COULEURS_ALERTE <- c(vert = "#2E7D32", jaune = "#F9A825",
                     orange = "#EF6C00", rouge = "#C62828")

LIBELLES <- c(
  srd                 = "Score de risque (SRD, 0-100)",
  taux_abandon        = "Taux d'abandon (%)",
  taux_achevement     = "Taux d'achèvement (%)",
  taux_redoublement   = "Taux de redoublement (%)",
  eleves_par_maitre   = "Élèves par maître",
  eleves_par_classe   = "Élèves par classe",
  indice_parite       = "Indice de parité F/G",
  part_ecoles_fermees = "Écoles fermées (%)"
)

lire_sortie <- function(description) {
  f <- dernier_fichier("04_analyse", description)
  if (is.na(f)) stop("Fichier '", description, "' absent : lancez R/00_run_all.R d'abord.")
  read_csv(f, show_col_types = FALSE)
}

charger_donnees_app <- function() {
  srd <- lire_sortie("srd-province-annee") |>
    mutate(alerte = factor(alerte, levels = names(COULEURS_ALERTE)))

  prev_f <- list.files(here("sorties", "04_analyse"),
                       pattern = "_prevision-abandon-\\d{4}_v\\d+\\.csv$", full.names = TRUE)
  prevision <- if (length(prev_f)) read_csv(tail(sort(prev_f), 1), show_col_types = FALSE) else NULL

  fiabilite <- tryCatch(lire_sortie("modele-fiabilite-test"), error = \(e) NULL)
  poids     <- tryCatch(lire_sortie("srd-composantes-poids"), error = \(e) NULL)

  carte <- st_read(here("data", "referentiels", "provinces_bfa.geojson"), quiet = TRUE) |>
    st_transform(4326)

  manquantes <- setdiff(unique(srd$province), carte$province)
  if (length(manquantes)) warning("Provinces sans contour : ", paste(manquantes, collapse = ", "))

  list(srd = srd, prevision = prevision, fiabilite = fiabilite, poids = poids,
       carte = carte, annees = sort(unique(srd$annee)),
       fictif = Sys.getenv("DONNEES_FICTIVES") == "1")
}

# Données d'une année jointes à la carte
carte_annee <- function(d, an) {
  d$carte |> left_join(d$srd |> filter(annee == an), by = c("province", "region"))
}
