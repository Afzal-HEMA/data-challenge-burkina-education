# ============================================================
# Lance tout le parcours des données, dans l'ordre.
# Usage : source("R/00_run_all.R") depuis la racine du projet.
# Prérequis : data/raw/DGESS_prescolaire_primaire_2016_2025.xlsx
# ============================================================

etapes <- c(
  "R/01_inventaire_feuilles.R",
  "R/02_nettoyage_dgess.R",
  "R/03_transformation_indicateurs.R",
  "R/04_analyse_srd.R",
  "R/04_analyse_modele.R"
)

for (f in etapes) {
  message("\n==== ", f, " ====")
  source(f, encoding = "UTF-8", local = new.env())
}
message("\nTerminé. Lancer l'application : shiny::runApp(\"app\")")
