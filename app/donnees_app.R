# ============================================================
# Préparation des données de l'application (testable sans Shiny)
# Lit les dernières versions des sorties des étapes 3 et 4.
# ============================================================

source(here::here("R", "fonctions_communes.R"))
source(here::here("R", "fonctions_srd.R"))
suppressPackageStartupMessages(library(sf))

COULEURS_ALERTE <- c(vert = "#2E7D32", jaune = "#F9A825",
                     orange = "#EF6C00", rouge = "#C62828")

LIBELLES <- c(
  taux_decrochage        = "Taux de décrochage sur le cycle primaire (%)",
  decrochage_annuel_apparent = "Décrochage annuel apparent (%)",
  evol_decrochage_3ans   = "Évolution du décrochage sur 3 ans (points)",
  evol_ecoles_3ans       = "Évolution du nombre d'écoles sur 3 ans (%)",
  ecart_parite_tap       = "Écart filles / garçons à l'achèvement",
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
lire_option <- function(etape, description) tryCatch(lire_sortie(etape, description), error = \(e) NULL)

charger_donnees_app <- function() {
  srd <- lire_sortie("04_analyse", "srd-region-annee") |>
    mutate(alerte = factor(alerte, levels = names(COULEURS_ALERTE)), type = "observé")
  derniere_obs <- max(srd$annee)

  # Prédictions sur 2 ans (04_analyse_projection.R) ajoutées après les années observées
  pred <- lire_option("04_analyse", "projection-region")
  if (!is.null(pred)) {
    srd <- bind_rows(srd, pred |> mutate(alerte = factor(alerte, levels = names(COULEURS_ALERTE)),
                                         type = "prédiction"))
  }

  prev_f <- list.files(here("sorties", "04_analyse"),
                       pattern = "_prevision-achevement-\\d{4}_v\\d+\\.csv$", full.names = TRUE)
  prevision <- if (length(prev_f)) lire_csv(tail(sort(prev_f), 1)) else NULL

  ind_obs <- lire_option("03_transformation", "indicateurs-region")
  national <- if (!is.null(ind_obs)) ind_obs |> filter(niveau_zone == "national", sexe == "T") else NULL

  carte <- st_read(here("data", "referentiels", "regions_bfa.geojson"), quiet = TRUE) |>
    st_transform(4326)
  sf_use_s2(FALSE)
  centres <- suppressWarnings(st_point_on_surface(carte))
  centres <- tibble(zone = carte$zone, lng = st_coordinates(centres)[, 1], lat = st_coordinates(centres)[, 2])
  # petits décalages pour que les noms des régions voisines du centre ne se chevauchent pas
  decal <- tribble(~zone, ~dlng, ~dlat,
                   "Centre", -0.10, -0.12, "Plateau Central", 0.20, 0.30,
                   "Centre-Sud", -0.30, -0.10, "Centre-Est", 0.35, 0.05)
  centres <- centres |> left_join(decal, by = "zone") |>
    mutate(lng = lng + coalesce(dlng, 0), lat = lat + coalesce(dlat, 0)) |> select(zone, lng, lat) |>
    # noms longs sur deux lignes pour qu'ils tiennent dans la région
    mutate(nom = recode(zone, "Boucle du Mouhoun" = "Boucle du<br>Mouhoun", "Plateau Central" = "Plateau<br>Central",
                        "Hauts-Bassins" = "Hauts-<br>Bassins", "Centre-Ouest" = "Centre-<br>Ouest",
                        "Centre-Nord" = "Centre-<br>Nord"))
  # contour du pays (union des 13 régions), tracé en trait épais
  # (petite dilatation / érosion de 500 m pour boucher les interstices entre régions, puis anneau extérieur seul)
  pays <- suppressWarnings(suppressMessages({
    u <- carte |> st_make_valid() |> st_transform(32630) |> st_buffer(500) |> st_union() |> st_buffer(-500)
    pol <- st_cast(u, "POLYGON"); pol <- pol[which.max(as.numeric(st_area(pol)))]
    st_sfc(st_polygon(list(pol[[1]][[1]])), crs = 32630) |> st_transform(4326) |> st_boundary()
  }))

  manquantes <- setdiff(unique(srd$zone), carte$zone)
  if (length(manquantes)) warning("Régions sans contour : ", paste(manquantes, collapse = ", "))

  D <- list(srd = srd, prevision = prevision,
            fiabilite = lire_option("04_analyse", "modele-fiabilite-test"),
            poids = lire_option("04_analyse", "srd-composantes-poids"),
            seuils = lire_option("04_analyse", "srd-seuils-matrice"),
            leviers = lire_option("04_analyse", "leviers-parametres"),
            modeles = lire_option("04_analyse", "prevision-comparaison-modeles"),
            modele_retenu = lire_option("04_analyse", "prevision-modele-retenu"),
            importance = lire_option("04_analyse", "prevision-importance-variables"),
            facteurs = lire_option("04_analyse", "facteurs-decrochage"),
            ind_obs = if (!is.null(ind_obs)) ind_obs |> filter(niveau_zone == "region") else NULL,
            proj_sexe = lire_option("04_analyse", "projection-sexe"),
            etab = lire_option("03_transformation", "etablissements-statut-region"),
            proj_statut = lire_option("04_analyse", "projection-statut"),
            national = national, carte = carte, centres = centres, pays = pays,
            annees = sort(unique(srd$annee)), derniere_obs = derniere_obs,
            projection = !is.null(pred))
  D$carte_long <- preparer_carte(D)
  D
}

# ---- Table longue pour la carte : zone x année x niveau x indicateur x sexe x statut ----
preparer_carte <- function(D) {
  vars_prim <- c("taux_decrochage", "decrochage_annuel_apparent", "evol_decrochage_3ans",
                 "tap", "tba", "tbs", "reussite_cep", "eleves", "non_achevement_relatif",
                 "evol_tap_3ans", "evol_eleves_3ans", "evol_ecoles_3ans")
  vars_pre <- c("eleves_prescolaire", "tbs_prescolaire")
  parts <- list()

  # 1. Score de risque (toutes années, observées et prédites)
  parts$srd <- D$srd |> transmute(zone, annee, type, indicateur = "srd", sexe = "T", statut = "Tous", valeur = srd)

  # 2. Années observées, par sexe (G / F / T)
  if (!is.null(D$ind_obs)) {
    parts$obs <- D$ind_obs |>
      select(zone, annee, sexe, any_of(c(vars_prim, vars_pre)), starts_with("ctx_")) |>
      pivot_longer(-c(zone, annee, sexe), names_to = "indicateur", values_to = "valeur") |>
      filter(!is.na(valeur)) |>
      mutate(type = "observé", statut = "Tous")
  }
  # 3. Années prédites : total (T) puis filles / garçons
  pr <- D$srd |> filter(type == "prédiction")
  if (nrow(pr)) {
    parts$pred_T <- pr |>
      select(zone, annee, any_of(c(vars_prim, vars_pre))) |>
      pivot_longer(-c(zone, annee), names_to = "indicateur", values_to = "valeur") |>
      mutate(type = "prédiction", sexe = "T", statut = "Tous")
  }
  if (!is.null(D$proj_sexe)) {
    parts$pred_sexe <- D$proj_sexe |>
      transmute(zone, annee, type = "prédiction", indicateur = variable, sexe, statut = "Tous", valeur)
  }
  # 4. Établissements par statut (observés + prédits)
  if (!is.null(D$etab)) {
    parts$etab <- D$etab |> filter(niveau_zone == "region") |>
      transmute(zone, annee, type = "observé", indicateur, cycle, sexe = "T",
                statut = if_else(statut == "Total", "Tous", statut), valeur)
  }
  if (!is.null(D$proj_statut)) {
    parts$etab_pred <- D$proj_statut |>
      transmute(zone, annee, type = "prédiction", indicateur, cycle, sexe = "T",
                statut = if_else(statut == "Total", "Tous", statut), valeur)
  }
  bind_rows(parts) |>
    mutate(niveau = case_when(
      indicateur %in% c("eleves_prescolaire", "tbs_prescolaire") ~ "prescolaire",
      indicateur %in% c("structures", "salles_classe") ~ "prescolaire",
      indicateur == "ecoles" ~ "primaire",
      TRUE ~ "primaire")) |>
    select(-any_of("cycle")) |>
    distinct(zone, annee, niveau, indicateur, sexe, statut, .keep_all = TRUE)
}

# Données d'une année jointes à la carte
carte_annee <- function(d, an) {
  d$carte |> left_join(d$srd |> filter(annee == an), by = "zone")
}

fmt_num <- function(x, d = 1) ifelse(is.na(x), "n.d.", format(round(x, d), nsmall = d, decimal.mark = ",", big.mark = " "))
