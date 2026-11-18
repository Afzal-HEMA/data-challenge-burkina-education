# ============================================================
# Étape 5 : interprétation - Outil d'alerte et d'aide à la décision
# Lancer : shiny::runApp("app", launch.browser = TRUE)   (après R/00_run_all.R)
# Packages : shiny, bslib, leaflet, sf, DT, tidyverse, here
#
# Onglets : Carte (prédictions, diagnostic, recommandations) · Simulateur ·
#           Investissement · Tableau de bord · Classement · Méthode
# ============================================================

library(shiny)
library(bslib)
library(leaflet)
library(DT)
source(here::here("app", "donnees_app.R"))
source(here::here("app", "accueil.R"))
source(here::here("app", "simulateur.R"))
source(here::here("app", "carte.R"))
source(here::here("app", "budget.R"))

D <- charger_donnees_app()
A <- charger_accueil()
regions_acc <- if (!is.null(A)) sort(unique(A$zone[A$niveau_zone == "region"])) else character()
zones_acc <- c("Ensemble du pays" = "National", setNames(regions_acc, regions_acc))
n_regions <- n_distinct(D$srd$zone)
# Effets des facteurs sur le décrochage (04_analyse_leviers.R) : modèle A, et
# modèle B pour « élèves par enseignant » ; 0 si le signe contredit l'attendu
effets_dec <- if (!is.null(D$facteurs)) {
  f <- D$facteurs |> arrange(modele) |> group_by(variable) |> slice(1) |> ungroup()
  as.list(setNames(f$effet_retenu, f$variable))
} else list()
param_sim <- list(
  elasticite = if (!is.null(D$leviers)) D$leviers$valeur[D$leviers$parametre == "elasticite_eleves_ecoles"] else 0.5,
  eleves_par_enseignant = if (!is.null(D$leviers)) D$leviers$valeur[D$leviers$parametre == "eleves_par_enseignant_defaut"] else 45,
  effets = effets_dec)
# Année affichée à l'ouverture : la première année prédite (2026)
annee_defaut <- if (D$projection) D$derniere_obs + 1 else D$derniere_obs
annees_futures <- D$annees[D$annees >= D$derniere_obs]
# régions proposées dans le simulateur : les plus à risque d'abord
zones_sim <- D$srd |> filter(annee == annee_defaut) |> arrange(desc(taux_decrochage)) |> pull(zone)

NOMS_MODELES <- c(naif = "Naïf (aucun changement)", tendance = "Tendance amortie",
                  lineaire = "Régression linéaire", foret = "Forêt aléatoire")

# Barre de navigation sombre : syntaxe différente selon la version de bslib
NAV_OPTS <- if ("navbar_options" %in% getNamespaceExports("bslib")) {
  list(navbar_options = bslib::navbar_options(bg = "#2B3446", theme = "dark"))
} else list(bg = "#2B3446", inverse = TRUE)

# ---- Interface ---------------------------------------------
ui <- do.call(page_navbar, c(list(
  id = "onglets",
  title = "Alerte décrochage scolaire · Burkina Faso",
  fillable = FALSE,
  theme = bs_theme(version = 5, primary = "#F47A5B", font_scale = 0.85,
                   base_font = font_collection("system-ui", "Segoe UI", "sans-serif")) |>
    bs_add_rules(".navbar { min-height: 48px; padding-top: 2px; padding-bottom: 2px; }
                  .navbar-brand { font-size: 1.05rem; } .nav-link { font-size: 0.9rem; }
                  .card-header { font-size: 0.9rem; font-weight: 600; }
                  .shiny-notification { font-size: 0.9rem; }
                  .shiny-notification-message { background:#E8F5E9; color:#1B5E20; border-color:#2E7D32; }
                  .shiny-notification-error { background:#FFEBEE; color:#B71C1C; border-color:#C62828; }"),

  nav_panel("Carte", value = "carte", icon = icon("map-location-dot"),
    carte_ui("carte", D$annees, annee_defaut, D$derniere_obs)),

  nav_panel("Simulateur", value = "simulateur", icon = icon("sliders"),
    simulateur_ui("sim", zones_sim, annees_futures, annee_defaut, param_sim, derniere_obs = D$derniere_obs)),

  nav_panel("Investissement", value = "budget", icon = icon("coins"),
    budget_ui("bud", annees_futures, annee_defaut)),

  nav_panel("Tableau de bord", value = "accueil", icon = icon("table-cells-large"),
    if (!is.null(A)) accueil_ui("acc", zones_acc, sort(unique(A$annee))) else
      p(class = "m-4 text-muted", "Tableau de bord absent : lancez l'étape 03_transformation.")),

  nav_panel("Classement", value = "classement", icon = icon("ranking-star"),
    div(class = "d-flex align-items-center gap-3 mb-2",
        selectInput("annee_cl", NULL, width = "260px",
                    choices = setNames(D$annees, ifelse(D$annees > D$derniere_obs, paste(D$annees, "(prédiction)"), paste(D$annees, "(observé)"))),
                    selected = annee_defaut)),
    layout_columns(col_widths = c(6, 6),
      card(card_header(textOutput("titre_top")), DTOutput("top")),
      card(card_header("Matrice de risque : situation × dégradation"),
           plotOutput("matrice", height = 420),
           p(class = "small text-muted",
             "En bas à gauche : situation correcte et stable. En haut à droite : situation",
             "critique qui se dégrade encore. Lignes : seuils de la règle d'alerte."))
    ),
    card(card_header("Taux de décrochage prédit"), DTOutput("prev"))
  ),

  nav_panel("Méthode", value = "methode", icon = icon("flask"),
    layout_columns(col_widths = c(6, 6),
      card(card_header("Le taux de décrochage et le niveau d'alerte"), tableOutput("poids"),
           p(class = "small text-muted",
             strong("Taux de décrochage = 100 × (1 − taux d'achèvement / taux d'admission au CP1) : "),
             "part des enfants entrés au CP1 qui quittent l'école avant d'achever le primaire. Le fichier DGESS",
             "ne donne pas de taux d'abandon : c'est l'approximation classique du taux d'abandon cumulé sur le cycle.",
             "Contrôle croisé : le décrochage annuel apparent (bilan des flux d'élèves) donne un ordre de grandeur cohérent."),
           p(class = "small text-muted",
             "Score de risque = 70 % niveau du décrochage + 30 % hausse sur 3 ans, chacun exprimé en rang",
             "percentile sur toutes les régions et années 2016-2025."),
           p(class = "small text-muted",
             strong("Règle d'alerte. "), "Rouge : situation parmi les 10 % pires, ou parmi les 25 %",
             "pires avec une dégradation au-dessus de la médiane. Orange : situation parmi les 25 %",
             "pires, ou au-dessus de la médiane avec une forte dégradation. Jaune : situation au-dessus",
             "de la médiane ou forte dégradation. Source : DGESS, Ministère de l'Éducation nationale.")),
      card(card_header("Choix du modèle de prédiction du décrochage"), tableOutput("modeles"),
           p(class = "small text-muted",
             "Validation « comme en vrai » : pour chaque année passée, chaque modèle n'apprend que sur ce qui",
             "était connu à cette date, prédit 1 an ou 2 ans plus tard, puis est comparé à la réalité.",
             "Le modèle retenu pour chaque horizon est celui dont l'erreur moyenne (MAE, en points) est la plus faible.",
             "« Sens correct » : part des cas où le modèle a prévu la bonne direction (hausse ou baisse)."),
           plotOutput("importance", height = 230))
    ),
    card(card_header("Facteurs associés au décrochage (effets estimés, utilisés par le diagnostic et le simulateur)"),
         tableOutput("facteurs"),
         p(class = "small text-muted",
           "Régression sur toutes les régions et années avec un effet propre à chaque année. Lecture : variation du taux",
           "de décrochage (en points) pour +1 unité du facteur. Ce sont des associations, pas des preuves de causalité ;",
           "un effet de signe inattendu n'est pas utilisé."))
  )
), NAV_OPTS))

# ---- Serveur -----------------------------------------------
server <- function(input, output, session) {

  carte <- carte_server("carte", D)
  simulateur_server("sim", D, param_sim, aller_a = carte$vers_simulateur)
  budget_server("bud", D, param_sim)
  if (!is.null(A)) accueil_server("acc", A)

  # depuis la fiche d'une région : ouvrir le simulateur sur cette région
  observeEvent(carte$vers_simulateur(), nav_select("onglets", "simulateur"))

  # ---- Classement ----
  an_cl <- reactive(as.integer(input$annee_cl))
  output$titre_top <- renderText(sprintf("Classement des régions en %d%s", an_cl(),
                                         if (an_cl() > D$derniere_obs) " (prédiction)" else ""))
  output$top <- renderDT({
    D$srd |> filter(annee == an_cl()) |> arrange(desc(srd)) |>
      transmute(`Région` = zone, `Décrochage (%)` = round(taux_decrochage, 1),
                `Évolution 3 ans (pts)` = round(evol_decrochage_3ans, 1),
                Alerte = as.character(alerte), `Score de risque` = round(srd, 1),
                `Achèvement (%)` = round(tap, 1)) |>
      datatable(rownames = FALSE, options = list(dom = "t", pageLength = 13)) |>
      formatStyle("Alerte", backgroundColor = styleEqual(names(COULEURS_ALERTE), COULEURS_ALERTE),
                  color = "white", fontWeight = "bold")
  })

  output$matrice <- renderPlot({
    d <- D$srd |> filter(annee == an_cl())
    s <- D$seuils
    g <- ggplot(d, aes(situation, degradation))
    if (!is.null(s)) {
      g <- g +
        geom_vline(xintercept = s$seuil[s$axe == "situation"], linetype = "dashed", colour = "grey75") +
        geom_hline(yintercept = s$seuil[s$axe == "degradation"], linetype = "dotted", colour = "grey75")
    }
    g + geom_point(aes(colour = alerte), size = 4) +
      geom_text(aes(label = zone), size = 3.3, vjust = -1.1, colour = "#2D3436") +
      scale_colour_manual(values = COULEURS_ALERTE, drop = FALSE, name = "Alerte") +
      scale_x_continuous(limits = c(0, 100)) + scale_y_continuous(limits = c(0, 105)) +
      labs(x = "Situation (0 = la meilleure, 100 = la pire)", y = "Dégradation sur 3 ans (100 = la plus forte)") +
      theme_minimal(base_size = 12) + theme(legend.position = "bottom")
  }, res = 96)

  output$prev <- renderDT({
    pr <- D$srd |> filter(type == "prédiction")
    req(nrow(pr))
    obs <- D$srd |> filter(annee == D$derniere_obs) |> select(zone, dec_obs = taux_decrochage, alerte_obs = alerte)
    pr |> left_join(obs, by = "zone") |> arrange(annee, desc(taux_decrochage)) |>
      transmute(`Région` = zone, `Année` = annee,
                !!sprintf("Décrochage %d (%%)", D$derniere_obs) := round(dec_obs, 1),
                `Décrochage prédit (%)` = round(taux_decrochage, 1),
                `Fourchette` = sprintf("%s – %s", fmt_num(dec_bas), fmt_num(dec_haut)),
                `Modèle` = NOMS_MODELES[modele_dec],
                `Alerte prédite` = as.character(alerte)) |>
      datatable(rownames = FALSE, options = list(dom = "tp", pageLength = 13)) |>
      formatStyle("Alerte prédite", backgroundColor = styleEqual(names(COULEURS_ALERTE), COULEURS_ALERTE),
                  color = "white", fontWeight = "bold")
  })

  # ---- Méthode ----
  output$poids <- renderTable({
    req(D$poids)
    D$poids |> transmute(Axe = if_else(axe == "situation", "Niveau (70 %)", "Dégradation (30 %)"),
                         Composante = coalesce(LIBELLES[variable], variable),
                         `Poids dans l'axe` = sprintf("%.0f %%", 100 * poids_dans_axe),
                         `Sens` = if_else(sens > 0, "↑ risque", "↓ risque"),
                         Justification = justification)
  })
  output$facteurs <- renderTable({
    req(D$facteurs)
    D$facteurs |> transmute(`Modèle` = modele, Facteur = coalesce(LIBELLES[variable], variable),
                            `Effet (pts de décrochage par unité)` = round(coefficient, 3),
                            `IC 95 %` = sprintf("[%s ; %s]", round(ic_bas, 3), round(ic_haut, 3)),
                            `p` = signif(p_valeur, 2), `Signe attendu` = ifelse(signe_conforme, "oui", "non (non utilisé)"),
                            n = as.integer(n), `R²` = round(r2, 2))
  })

  output$modeles <- renderTable({
    req(D$modeles)
    ret <- if (!is.null(D$modele_retenu)) D$modele_retenu |> select(h, retenu = modele) else NULL
    D$modeles |> left_join(ret, by = "h") |>
      transmute(Horizon = paste(h, "an(s)"), `Modèle` = NOMS_MODELES[modele],
                `MAE (pts)` = round(MAE, 2), `RMSE (pts)` = round(RMSE, 2),
                `Sens correct` = ifelse(is.na(sens_correct), "–", sprintf("%.0f %%", 100 * sens_correct)),
                `Retenu` = ifelse(modele == retenu, "✔", ""))
  })
  output$importance <- renderPlot({
    req(D$importance)
    noms <- c(LIBELLES, evol_decrochage = "Évolution du décrochage sur 1 an", evol_tap = "Évolution de l'achèvement sur 1 an",
              evol_nouveaux_cp1 = "Évolution des nouveaux inscrits CP1")
    d <- D$importance |> slice_max(importance, n = 8) |>
      mutate(nom = coalesce(noms[variable], variable), nom = reorder(nom, importance))
    ggplot(d, aes(importance, nom)) + geom_col(fill = "#F47A5B", width = 0.6) +
      labs(x = "Importance (forêt aléatoire)", y = NULL, title = "Variables qui expliquent le plus l'évolution du décrochage") +
      theme_minimal(base_size = 10) + theme(plot.title = element_text(size = 10, face = "bold"))
  }, res = 96)
}

shinyApp(ui, server)
