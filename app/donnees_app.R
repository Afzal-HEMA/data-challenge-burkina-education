# ============================================================
# Préparation des données de l'application (testable sans Shiny)
# Lit les dernières versions des sorties des étapes 3 et 4.
# ============================================================

source(here::here("R", "fonctions_communes.R"))
suppressPackageStartupMessages(library(sf))

COULEURS_ALERTE <- c(vert = "#2E7D32", jaune = "#F9A825",
                     orange = "#EF6C00", rouge = "#C62828")

LIBELLES <- c(
  srd                    = "Score de risque de décrochage (SRD, 0-100)",
  situation              = "Score de situation (0-100)",
  degradation            = "Score de dégradation sur 3 ans (0-100)",
  tap                    = "Taux d'achèvement du primaire (%)",
  non_achevement_relatif = "Entrants au CP1 qui n'achèvent pas (1 - TAP/TBA, %)",
  evol_tap_3ans          = "Évolution de l'achèvement sur 3 ans (points)",
  evol_eleves_3ans       = "Évolution des effectifs sur 3 ans (%)",
  tba                    = "Taux brut d'admission au CP1 (%)",
  tbs                    = "Taux brut de scolarisation (%)",
  reussite_cep           = "Taux de réussite au CEP (%)",
  ips_tap                = "Indice de parité F/G de l'achèvement",
  part_filles            = "Part des filles dans les effectifs (%)",
  ctx_eleves_par_enseignant = "Élèves par enseignant (INSD + DGESS)",
  ctx_eleves_par_classe  = "Élèves par classe (INSD + DGESS)",
  ctx_pdi_pour_1000      = "Déplacés internes pour 1 000 habitants (contexte)",
  ctx_ecoles_fermees_pct = "Écoles fermées pour insécurité (%, contexte)",
  ctx_pauvrete_pct       = "Incidence de la pauvreté (%, contexte)"
)
# indicateurs pour lesquels une valeur haute est favorable (échelle inversée)
FAVORABLES <- c("tap", "tba", "tbs", "reussite_cep", "evol_tap_3ans", "evol_eleves_3ans", "ips_tap", "part_filles")

lire_sortie <- function(etape, description) {
  f <- dernier_fichier(etape, description)
  if (is.na(f)) stop("Fichier '", description, "' absent : lancez R/00_run_all.R d'abord.")
  lire_csv(f)
}

charger_donnees_app <- function() {
  srd <- lire_sortie("04_analyse", "srd-region-annee") |>
    mutate(alerte = factor(alerte, levels = names(COULEURS_ALERTE)), type = "observé")
  derniere_obs <- max(srd$annee)

  # Projections 2026-2030 (04_analyse_projection.R) ajoutées à la suite des années observées
  proj <- tryCatch(lire_sortie("04_analyse", "projection-region"), error = \(e) NULL)
  if (!is.null(proj)) {
    srd <- bind_rows(srd, proj |> mutate(alerte = factor(alerte, levels = names(COULEURS_ALERTE)),
                                         type = "projection"))
  }

  prev_f <- list.files(here("sorties", "04_analyse"),
                       pattern = "_prevision-achevement-\\d{4}_v\\d+\\.csv$", full.names = TRUE)
  prevision <- if (length(prev_f)) lire_csv(tail(sort(prev_f), 1)) else NULL

  fiabilite <- tryCatch(lire_sortie("04_analyse", "modele-fiabilite-test"), error = \(e) NULL)
  poids     <- tryCatch(lire_sortie("04_analyse", "srd-composantes-poids"), error = \(e) NULL)
  seuils    <- tryCatch(lire_sortie("04_analyse", "srd-seuils-matrice"), error = \(e) NULL)
  leviers   <- tryCatch(lire_sortie("04_analyse", "leviers-parametres"), error = \(e) NULL)
  national  <- tryCatch(lire_sortie("03_transformation", "indicateurs-region") |>
                          filter(niveau_zone == "national", sexe == "T"), error = \(e) NULL)

  carte <- st_read(here("data", "referentiels", "regions_bfa.geojson"), quiet = TRUE) |>
    st_transform(4326)

  manquantes <- setdiff(unique(srd$zone), carte$zone)
  if (length(manquantes)) warning("Régions sans contour : ", paste(manquantes, collapse = ", "))

  list(srd = srd, prevision = prevision, fiabilite = fiabilite, poids = poids, seuils = seuils, leviers = leviers,
       national = national, carte = carte, annees = sort(unique(srd$annee)),
       derniere_obs = derniere_obs, projection = !is.null(proj))
}

# Données d'une année jointes à la carte
carte_annee <- function(d, an) {
  d$carte |> left_join(d$srd |> filter(annee == an), by = "zone")
}

fmt_num <- function(x, d = 1) ifelse(is.na(x), "n.d.", format(round(x, d), nsmall = d, decimal.mark = ",", big.mark = " "))
