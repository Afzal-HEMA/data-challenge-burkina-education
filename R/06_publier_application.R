# ============================================================
# Étape 6 : mise en ligne de l'application sur shinyapps.io
#
# Prérequis (une seule fois) :
#   1. Créer un compte gratuit sur https://www.shinyapps.io
#   2. Account > Tokens > Show > copier la commande
#      rsconnect::setAccountInfo(name = "...", token = "...", secret = "...")
#      et la lancer dans la console. NE PAS l'écrire dans ce fichier :
#      le dépôt est public et ce jeton donne accès à votre compte.
#   3. install.packages("rsconnect")
#
# Usage : source("R/06_publier_application.R") après R/00_run_all.R
#
# Le script rassemble dans publication/ uniquement ce dont l'application
# a besoin (code, dernières sorties des étapes 3 et 4, contours des régions),
# vérifie que les données se chargent, puis envoie le tout sur shinyapps.io.
# ============================================================

source(here::here("R", "fonctions_communes.R"))

NOM_APPLICATION <- "alerte-decrochage-burkina"   # devient l'adresse : https://<compte>.shinyapps.io/<nom>/

# ---- 1. Vérifier que les sorties existent ------------------------
necessaires <- list(
  c("03_transformation", "indicateurs-region"),
  c("03_transformation", "tableau-accueil"),
  c("04_analyse", "srd-region-annee"),
  c("04_analyse", "srd-composantes-poids"),
  c("04_analyse", "modele-fiabilite-test"),
  c("04_analyse", "srd-seuils-matrice"),
  c("04_analyse", "projection-region"),
  c("04_analyse", "projection-sexe"),
  c("04_analyse", "projection-statut"),
  c("04_analyse", "prevision-comparaison-modeles"),
  c("04_analyse", "prevision-modele-retenu"),
  c("04_analyse", "prevision-importance-variables"),
  c("03_transformation", "etablissements-statut-region"),
  c("04_analyse", "leviers-parametres"),
  c("04_analyse", "facteurs-decrochage")
)
fichiers <- map_chr(necessaires, \(x) dernier_fichier(x[1], x[2]))
if (anyNA(fichiers)) {
  stop("Sorties manquantes : lancez d'abord source(\"R/00_run_all.R\")\n  ",
       paste(map_chr(necessaires[is.na(fichiers)], paste, collapse = "/"), collapse = "\n  "))
}
prevision <- list.files(here("sorties", "04_analyse"),
                        pattern = "_prevision-achevement-\\d{4}_v\\d+\\.csv$", full.names = TRUE)
fichiers <- c(fichiers, tail(sort(prevision), 1))

# ---- 2. Préparer le dossier publication/ -------------------------
dest <- here("publication")
unlink(dest, recursive = TRUE)
dir.create(dest)
copier <- function(src, rel) {
  cible <- file.path(dest, rel)
  dir.create(dirname(cible), recursive = TRUE, showWarnings = FALSE)
  stopifnot(file.copy(src, cible, overwrite = TRUE))
}

invisible(file.create(file.path(dest, ".here")))                 # racine pour here::here()
copier(here("app", "app.R"), "app.R")                    # point d'entrée de shinyapps.io
copier(here("app", "accueil.R"), "app/accueil.R")
copier(here("app", "donnees_app.R"), "app/donnees_app.R")
copier(here("R", "fonctions_communes.R"), "R/fonctions_communes.R")
copier(here("R", "fonctions_srd.R"), "R/fonctions_srd.R")
copier(here("app", "simulateur.R"), "app/simulateur.R")
copier(here("app", "carte.R"), "app/carte.R")
copier(here("app", "budget.R"), "app/budget.R")
for (f in c("provinces_regions.csv", "alias_noms.csv", "regions_bfa.geojson")) {
  copier(here("data", "referentiels", f), file.path("data", "referentiels", f))
}
for (f in fichiers) {
  etape <- basename(dirname(f))
  copier(f, file.path("sorties", etape, basename(f)))
}
message("Dossier publication/ prêt : ", length(list.files(dest, recursive = TRUE)), " fichiers")

# ---- 3. Vérifier que l'application charge ses données ------------
# Dans une session R séparée, lancée depuis publication/ (comme sur le serveur)
code <- 'source("app/donnees_app.R"); d <- charger_donnees_app();
  stopifnot(!is.null(d$facteurs), max(d$annees) > d$derniere_obs);
  cat("OK :", dplyr::n_distinct(d$srd$zone), "régions, années", paste(range(d$annees), collapse = "-"), "\\n")'
sortie <- withr::with_dir(dest, system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(code)),
                                        stdout = TRUE, stderr = TRUE))
message(paste(sortie, collapse = "\n"))
if (!any(str_detect(sortie, "^OK :"))) stop("L'application ne charge pas ses données : corrigez l'erreur ci-dessus avant de publier.")

# ---- 4. Mise en ligne --------------------------------------------
if (!requireNamespace("rsconnect", quietly = TRUE)) stop("install.packages(\"rsconnect\") d'abord.")
if (nrow(rsconnect::accounts()) == 0) {
  stop("Aucun compte shinyapps.io configuré : lancez rsconnect::setAccountInfo(...) (voir en-tête).")
}
rsconnect::deployApp(appDir = dest, appName = NOM_APPLICATION,
                     appTitle = "Alerte décrochage scolaire - Burkina Faso",
                     forceUpdate = TRUE, launch.browser = TRUE)
