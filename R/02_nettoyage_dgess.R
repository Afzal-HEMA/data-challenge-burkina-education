# ============================================================
# Étape 2 : nettoyage - Statistiques DGESS préscolaire et primaire 2016-2025
# Entrée  : data/raw/DGESS_prescolaire_primaire_2016_2025.xlsx
#           (13 feuilles primaire + 1 feuille PRESCOLAIRE de 8 tableaux)
# Sortie  : une table longue unique et propre :
#   tableau | cycle | indicateur | zone | niveau_zone | dimension |
#   modalite | modalite2 | sexe | annee | valeur
#
# Règles appliquées (détail dans docs/journal_nettoyage.md) :
#  - cellules fusionnées remplies vers le bas (région, statut, niveau) ;
#  - année scolaire codée par son année de fin (2015-2016 -> 2016) ;
#  - noms de régions harmonisés (HAUT-BASSINS, Burkina Faso, Total général...) ;
#  - sexe : GARCONS / Garçons / G -> G, etc. ;
#  - lignes de sous-total (« Total X ») repérées et étiquetées « Total » ;
#  - valeurs : espaces, virgules et symboles retirés, « - » et « ND » -> NA.
# ============================================================

source(here::here("R", "fonctions_communes.R"))

chemin <- here("data", "raw", "DGESS_prescolaire_primaire_2016_2025.xlsx")
stopifnot("Fichier DGESS introuvable : copiez-le dans data/raw/ (voir README)" = file.exists(chemin))

# ---- 1. Catalogue des tableaux -------------------------------
# forme : "bloc" (une colonne par année) ou "gft" (G/F/T sous chaque année)
# ids   : colonnes d'identifiants, dans l'ordre de la feuille
#         zone = région/national ; modalite / modalite2 = statut, milieu, niveau
catalogue <- tribble(
  ~tableau, ~cycle,        ~feuille,      ~plage,      ~forme, ~ids,                                ~indicateur,          ~dimension,
  "P01",    "primaire",    "Feuil1",      "A2:L44",    "bloc", c("zone", "sexe"),                    "eleves",             "ensemble",
  "P02",    "primaire",    "Feuil2",      "A2:L11",    "bloc", c("modalite", "sexe"),                "eleves",             "statut",
  "P03",    "primaire",    "Feuil3",      "A2:L11",    "bloc", c("modalite", "sexe"),                "eleves",             "milieu",
  "P04",    "primaire",    "Feuil4",      "A2:M47",    "bloc", c("modalite", "modalite2", "sexe"),   "eleves",             "statut_niveau",
  "P05",    "primaire",    "Feuil5",      "A2:L44",    "bloc", c("zone", "sexe"),                    "nouveaux_cp1",       "ensemble",
  "P06",    "primaire",    "Feuil6",      "A2:AE17",   "gft",  c("zone"),                            "tba",                "ensemble",
  "P07",    "primaire",    "Feuil7",      "A2:AE17",   "gft",  c("zone"),                            "tbs",                "ensemble",
  "P08",    "primaire",    "Feuil8",      "A2:AE17",   "gft",  c("zone"),                            "tap",                "ensemble",
  "P09",    "primaire",    "Feuil9",      "A2:L44",    "bloc", c("zone", "sexe"),                    "reussite_cep",       "ensemble",
  "P10",    "primaire",    "Feuil10",     "A2:K16",    "bloc", c("zone"),                            "ips_tba",            "ensemble",
  "P11",    "primaire",    "Feuil11",     "A2:K16",    "bloc", c("zone"),                            "ips_tbs",            "ensemble",
  "P12",    "primaire",    "Feuil12",     "A2:K16",    "bloc", c("zone"),                            "ips_tap",            "ensemble",
  "P13",    "primaire",    "Feuil13",     "A2:L44",    "bloc", c("zone", "modalite"),                "ecoles",             "statut",
  "S01",    "prescolaire", "PRESCOLAIRE", "A2:AE17",   "gft",  c("zone"),                            "eleves",             "ensemble",
  "S02",    "prescolaire", "PRESCOLAIRE", "A20:AE25",  "gft",  c("modalite"),                        "eleves",             "statut",
  "S03",    "prescolaire", "PRESCOLAIRE", "A28:Y33",   "gft",  c("modalite"),                        "eleves",             "milieu",
  "S04",    "prescolaire", "PRESCOLAIRE", "A37:AF51",  "gft",  c("modalite", "modalite2"),           "eleves",             "statut_niveau",
  "S05",    "prescolaire", "PRESCOLAIRE", "A55:AE70",  "gft",  c("zone"),                            "tbs",                "ensemble",
  "S06",    "prescolaire", "PRESCOLAIRE", "A74:K88",   "bloc", c("zone"),                            "ips_tbs",            "ensemble",
  "S07",    "prescolaire", "PRESCOLAIRE", "A92:L145",  "bloc", c("zone", "modalite"),                "structures",         "statut",
  "S08",    "prescolaire", "PRESCOLAIRE", "A149:L202", "bloc", c("zone", "modalite"),                "salles_classe",      "statut"
)

# ---- 2. Lecture et harmonisation d'un tableau -----------------
lire_tableau <- function(t) {
  d <- if (t$forme == "gft") lire_bloc_gft(chemin, t$feuille, t$plage, t$ids[[1]])
       else                  lire_bloc(chemin, t$feuille, t$plage, t$ids[[1]])

  if (!"sexe" %in% names(d))      d$sexe <- "T"          # indices sans ventilation
  if (!"modalite" %in% names(d))  d$modalite <- "Total"
  if (!"modalite2" %in% names(d)) d$modalite2 <- NA_character_

  if ("zone" %in% names(d)) {
    d <- rattacher_zone(d, t$feuille)
    # « Total BOUCLE DU MOUHOUN » sans statut -> modalité Total
    d <- d |> mutate(modalite = if_else(est_sous_total | is.na(modalite), "Total", modalite)) |>
      select(-est_sous_total)
  } else {
    # Tableaux nationaux ventilés : la ligne NATIONAL/TOTAL est la modalité « Total »
    d <- d |> mutate(zone = "National", niveau_zone = "national")
  }

  d |>
    mutate(
      # « Total Public » (préscolaire) -> statut Public, niveau Total
      modalite2 = if_else(t$dimension == "statut_niveau" & is.na(modalite2) &
                            str_detect(modalite, regex("^total\\s+\\S", ignore_case = TRUE)),
                          "Total", modalite2),
      modalite  = str_remove(modalite, regex("^total\\s+(?!g[eé]n[eé]ral)", ignore_case = TRUE)),
      modalite  = normaliser_modalite(modalite),
      modalite2 = if_else(is.na(modalite2), NA_character_, normaliser_modalite(modalite2)),
      sexe      = normaliser_sexe(sexe),
      tableau = t$tableau, cycle = t$cycle, indicateur = t$indicateur,
      dimension = t$dimension
    ) |>
    select(tableau, cycle, indicateur, zone, niveau_zone, dimension,
           modalite, modalite2, sexe, annee, valeur)
}

message("Lecture de ", nrow(catalogue), " tableaux...")
donnees <- map_dfr(seq_len(nrow(catalogue)), \(i) {
  t <- catalogue[i, ]
  d <- tryCatch(lire_tableau(t), error = \(e) stop("[", t$tableau, " / ", t$feuille, "] ", conditionMessage(e)))
  message(sprintf("  %s %-11s %-14s %4d valeurs (%d manquantes)", t$tableau, t$feuille,
                  t$indicateur, nrow(d), sum(is.na(d$valeur))))
  d
})

# ---- 3. Vérifications ----------------------------------------
stopifnot("Sexe non reconnu" = !any(is.na(donnees$sexe)))
stopifnot("Année manquante" = !any(is.na(donnees$annee)))
doublons <- donnees |> count(tableau, zone, modalite, modalite2, sexe, annee) |> filter(n > 1)
if (nrow(doublons)) { print(doublons); stop("Doublons détectés") }

# Contrôles sur les EFFECTIFS seulement (pas sur les taux ni les indices)
effectifs <- donnees |> filter(indicateur %in% c("eleves", "nouveaux_cp1", "ecoles",
                                                 "structures", "salles_classe"))
ctrl_sexe <- controle_sexe(effectifs)
ctrl_reg  <- controle_regions_national(effectifs)

cat("\n== Contrôles ==\n")
cat("G + F différent de T :", nrow(ctrl_sexe), "cas\n")
cat("Somme des régions différente du national :", sum(!ctrl_reg$ok), "cas sur", nrow(ctrl_reg), "\n")
cat("Valeurs manquantes :", sum(is.na(donnees$valeur)), "sur", nrow(donnees), "\n")
manquants <- donnees |> group_by(tableau, indicateur) |>
  summarise(manquantes = sum(is.na(valeur)), total = n(), .groups = "drop") |>
  filter(manquantes > 0)
if (nrow(manquants)) print(manquants)

# ---- 4. Exports ----------------------------------------------
exporter(donnees,   "02_nettoyage", "engineer", "donnees-dgess-long")
exporter(ctrl_sexe, "02_nettoyage", "engineer", "controle-garcons-filles-total")
exporter(ctrl_reg,  "02_nettoyage", "engineer", "controle-regions-national")
exporter(catalogue |> mutate(ids = map_chr(ids, paste, collapse = " + ")),
         "02_nettoyage", "architect", "catalogue-tableaux-sources")
