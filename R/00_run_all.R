# ============================================================
# Lance tout le parcours des données, dans l'ordre.
# Usage : source("R/00_run_all.R") depuis la racine du projet.
# ============================================================

etapes <- c(
  "R/01_inventaire_feuilles.R",
  "R/02_nettoyage_effectifs.R"
  # "R/02_nettoyage_abandon_redoublement.R",   # à écrire (structure des feuilles)
  # "R/03_transformation_indicateurs.R",       # à écrire : produit la table d'indicateurs
  "R/04_analyse_srd.R",
  "R/04_analyse_modele.R",
  # "R/05_interpretation_cartes.R"
)

# Pour tester sans les vraies données : Sys.setenv(DONNEES_FICTIVES = "1")
# puis source("R/outils/generer_donnees_fictives.R") avant de lancer ce script.

for (f in etapes) {
  message("\n==== ", f, " ====")
  source(f, encoding = "UTF-8", local = new.env())
}
message("\nTerminé.")
