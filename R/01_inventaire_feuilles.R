# ============================================================
# Étape 1 : collecte - Inventaire du fichier DGESS
# Liste chaque feuille : dimensions, titre, libellés de la 1re colonne.
# N'affiche aucune valeur chiffrée.
# Sortie : sorties/01_collecte/..._inventaire-feuilles-dgess_v1.csv
# ============================================================

source(here::here("R", "fonctions_communes.R"))

chemin <- here("data", "raw", "DGESS_prescolaire_primaire_2016_2025.xlsx")
stopifnot("Fichier DGESS introuvable : copiez-le dans data/raw/ (voir README)" = file.exists(chemin))

inventaire <- map_dfr(excel_sheets(chemin), function(s) {
  x <- read_excel(chemin, sheet = s, col_names = FALSE, col_types = "text",
                  .name_repair = "minimal")
  col1 <- na.omit(x[[1]])
  tibble(
    feuille        = s,
    n_lignes       = nrow(x),
    n_colonnes     = ncol(x),
    titre          = paste(na.omit(unlist(x[1:min(2, nrow(x)), ])), collapse = " | "),
    libelles_col_a = paste(head(col1, 10), collapse = " ; "),
    noms_reconnus  = sum(!is.na(identifier_entite(col1)$nom))
  )
})

print(inventaire, n = Inf, width = Inf)
exporter(inventaire, "01_collecte", "engineer", "inventaire-feuilles-dgess")
