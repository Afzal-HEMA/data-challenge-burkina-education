# ============================================================
# Lance tout le parcours des données, dans l'ordre.
# Usage : source("R/00_run_all.R") depuis la racine du projet.
# ============================================================

etapes <- c(
  "R/01_inventaire_feuilles.R",
  "R/02_nettoyage_effectifs.R"
  # "R/02_nettoyage_abandon_redoublement.R",
  # "R/03_transformation_indicateurs.R",
  # "R/04_analyse_srd.R",
  # "R/04_analyse_modele.R",
  # "R/05_interpretation_cartes.R"
)

for (f in etapes) {
  message("\n==== ", f, " ====")
  source(f, encoding = "UTF-8", local = new.env())
}
message("\nTerminé.")
