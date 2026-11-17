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
  tap                    = "Taux d'achèvement du primaire (%)",
  ecart_acces_achevement = "Écart accès - achèvement (TBA - TAP, points)",
  tba                    = "Taux brut d'admission au CP1 (%)",
  tbs                    = "Taux brut de scolarisation (%)",
  reussite_cep           = "Taux de réussite au CEP (%)",
  evol_eleves            = "Évolution des effectifs sur un an (%)",
  ips_tap                = "Indice de parité F/G de l'achèvement",
  part_filles            = "Part des filles dans les effectifs (%)"
)
# indicateurs pour lesquels une valeur haute est favorable (échelle inversée)
FAVORABLES <- c("tap", "tba", "tbs", "reussite_cep", "evol_eleves", "ips_tap", "part_filles")

lire_sortie <- function(etape, description) {
  f <- dernier_fichier(etape, description)
  if (is.na(f)) stop("Fichier '", description, "' absent : lancez R/00_run_all.R d'abord.")
  lire_csv(f)
}

charger_donnees_app <- function() {
  srd <- lire_sortie("04_analyse", "srd-region-annee") |>
    mutate(alerte = factor(alerte, levels = names(COULEURS_ALERTE)))

  prev_f <- list.files(here("sorties", "04_analyse"),
                       pattern = "_prevision-achevement-\\d{4}_v\\d+\\.csv$", full.names = TRUE)
  prevision <- if (length(prev_f)) lire_csv(tail(sort(prev_f), 1)) else NULL

  fiabilite <- tryCatch(lire_sortie("04_analyse", "modele-fiabilite-test"), error = \(e) NULL)
  poids     <- tryCatch(lire_sortie("04_analyse", "srd-composantes-poids"), error = \(e) NULL)
  national  <- tryCatch(lire_sortie("03_transformation", "indicateurs-region") |>
                          filter(niveau_zone == "national", sexe == "T"), error = \(e) NULL)

  carte <- st_read(here("data", "referentiels", "regions_bfa.geojson"), quiet = TRUE) |>
    st_transform(4326)

  manquantes <- setdiff(unique(srd$zone), carte$zone)
  if (length(manquantes)) warning("Régions sans contour : ", paste(manquantes, collapse = ", "))

  list(srd = srd, prevision = prevision, fiabilite = fiabilite, poids = poids,
       national = national, carte = carte, annees = sort(unique(srd$annee)))
}

# Données d'une année jointes à la carte
carte_annee <- function(d, an) {
  d$carte |> left_join(d$srd |> filter(annee == an), by = "zone")
}

fmt_num <- function(x, d = 1) ifelse(is.na(x), "n.d.", format(round(x, d), nsmall = d, decimal.mark = ",", big.mark = " "))
