# ============================================================
# Lance tout le parcours des données, dans l'ordre.
# Usage : source("R/00_run_all.R") depuis la racine du projet.
#
# Mode démonstration (chiffres inventés, sans le fichier DGESS) :
#   Sys.setenv(DONNEES_FICTIVES = "1"); source("R/00_run_all.R")
# ============================================================

fictif <- Sys.getenv("DONNEES_FICTIVES") == "1"

etapes <- c(
  if (!fictif) c("R/01_inventaire_feuilles.R", "R/02_nettoyage_dgess.R")
  else "R/outils/generer_donnees_fictives.R",
  "R/03_transformation_indicateurs.R",
  "R/04_analyse_srd.R",
  "R/04_analyse_modele.R"
)

for (f in etapes) {
  message("\n==== ", f, " ====")
  source(f, encoding = "UTF-8", local = new.env())
}
message("\nTerminé. Lancer l'application : shiny::runApp(\"app\")")
