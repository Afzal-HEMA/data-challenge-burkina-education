# ============================================================
# Étape 5 : interprétation - Tableau de bord d'alerte au décrochage
# Lancer : shiny::runApp("app")   (après R/00_run_all.R)
# Packages : shiny, bslib, leaflet, sf, DT, tidyverse, here
# ============================================================

library(shiny)
library(bslib)
library(leaflet)
library(DT)
source(here::here("app", "donnees_app.R"))
source(here::here("app", "accueil.R"))

D <- charger_donnees_app()
annee_max <- max(D$annees)
A <- charger_accueil()
regions_acc <- if (!is.null(A)) sort(unique(A$zone[A$niveau_zone == "region"])) else character()
zones_acc <- c("Ensemble du pays" = "National", setNames(regions_acc, regions_acc))
n_regions <- n_distinct(D$srd$zone)

# Barre de navigation sombre : syntaxe différente selon la version de bslib
NAV_OPTS <- if ("navbar_options" %in% getNamespaceExports("bslib")) {
  list(navbar_options = bslib::navbar_options(bg = "#2B3446", theme = "dark"))
} else list(bg = "#2B3446", inverse = TRUE)

# ---- Interface ---------------------------------------------
ui <- do.call(page_navbar, c(list(
  title = "Alerte décrochage scolaire · Burkina Faso",
  theme = bs_theme(version = 5, primary = "#F47A5B", font_scale = 0.85,
                   base_font = font_collection("system-ui", "Segoe UI", "sans-serif")) |>
    bs_add_rules(".navbar { min-height: 48px; padding-top: 2px; padding-bottom: 2px; }
                  .navbar-brand { font-size: 1.05rem; } .nav-link { font-size: 0.9rem; }
                  .card-header { font-size: 0.9rem; font-weight: 600; }"),

  nav_panel("Accueil", icon = icon("house"),
    if (!is.null(A)) accueil_ui("acc", zones_acc, sort(unique(A$annee))) else
      p(class = "m-4 text-muted", "Tableau d'accueil absent : lancez l'étape 03_transformation.")),

  nav_panel("Carte d'alerte", icon = icon("map-location-dot"),
    layout_sidebar(
      sidebar = sidebar(width = 300,
        sliderInput("annee", "Année (année de fin d'année scolaire)", min = min(D$annees), max = annee_max,
                    value = D$derniere_obs, step = 1, sep = "", ticks = FALSE),
        uiOutput("info_projection"),
        selectInput("indic", "Indicateur affiché",
                    choices = setNames(names(LIBELLES), LIBELLES)[names(LIBELLES) %in% names(D$srd)],
                    selected = "srd"),
        helpText("Cliquez sur une région pour afficher sa fiche."),
        hr(),
        uiOutput("resume_national")
      ),
      layout_columns(col_widths = c(7, 5),
        card(full_screen = TRUE, card_header(textOutput("titre_carte")),
             leafletOutput("carte", height = 560)),
        card(card_header(textOutput("titre_fiche")), uiOutput("fiche"),
             plotOutput("courbes", height = 260))
      )
    )
  ),

  nav_panel("Classement", icon = icon("ranking-star"),
    layout_columns(col_widths = c(6, 6),
      card(card_header(textOutput("titre_top")), DTOutput("top")),
      card(card_header("Matrice de risque : situation × dégradation"),
           plotOutput("matrice", height = 420),
           p(class = "small text-muted",
             "En bas à gauche : situation correcte et stable. En haut à droite : situation",
             "critique qui se dégrade encore. Lignes : seuils de la règle d'alerte."))
    ),
    card(card_header("Prévision du taux d'achèvement l'an prochain"), DTOutput("prev"))
  ),

  nav_panel("Méthode et fiabilité", icon = icon("flask"),
    layout_columns(col_widths = c(6, 6),
      card(card_header("Composantes du SRD et poids"), tableOutput("poids"),
           p(class = "small text-muted",
             "Le fichier DGESS ne contient pas de taux d'abandon : le SRD combine des signaux",
             "indirects de décrochage sur deux axes. Situation : où en est la région. Dégradation :",
             "vers où elle va (3 ans). Chaque composante est convertie en rang percentile sur toutes",
             "les régions et toutes les années. SRD = 70 % situation + 30 % dégradation."),
           p(class = "small text-muted",
             strong("Règle d'alerte. "), "Rouge : situation parmi les 10 % pires, ou parmi les 25 %",
             "pires avec une dégradation au-dessus de la médiane. Orange : situation parmi les 25 %",
             "pires, ou au-dessus de la médiane avec une forte dégradation. Jaune : situation au-dessus",
             "de la médiane ou forte dégradation. Source : DGESS, Ministère de l'Éducation nationale.")),
      card(card_header("Fiabilité du modèle (années de test)"), tableOutput("fiab"),
           p(class = "small text-muted",
             "Cible : taux d'achèvement de l'année suivante (les modèles prédisent sa variation).",
             "MAE : erreur moyenne en points. « tendance » = la moitié de la tendance des 3 dernières années.",
             "Rappel alerte : part des 3 régions au plus faible achèvement réel que le modèle a",
             "repérées. « naif » = on suppose que l'an prochain sera identique à cette année."))
    )
  )
), NAV_OPTS))

# ---- Serveur -----------------------------------------------
server <- function(input, output, session) {

  if (!is.null(A)) accueil_server("acc", A)

  zone_sel <- reactiveVal(NULL)
  donnees_an <- reactive(carte_annee(D, input$annee))

  est_projection <- reactive(input$annee > D$derniere_obs)

  output$titre_carte <- renderText(paste0(LIBELLES[input$indic], " · ", input$annee,
                                          if (est_projection()) " · PROJECTION" else " · observé"))

  output$info_projection <- renderUI({
    if (!D$projection) return(NULL)
    if (est_projection()) {
      div(class = "alert alert-warning p-2 small mb-2",
          strong(sprintf("Projection %d", input$annee)), br(),
          sprintf("Prolongement de la tendance %d-%d, amorti selon ce qui a le mieux marché par le passé.",
                  D$derniere_obs - 3, D$derniere_obs),
          "Ce n'est pas une certitude : un choc (réouvertures d'écoles, insécurité) peut changer la trajectoire.")
    } else {
      p(class = "small text-muted mb-2",
        sprintf("Données observées. Faites glisser au-delà de %d pour voir les projections jusqu'en %d.",
                D$derniere_obs, annee_max))
    }
  })

  output$carte <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 5)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -1.6, lat = 12.3, zoom = 6)
  })

  observe({
    g <- donnees_an()
    v <- g[[input$indic]]
    if (input$indic == "srd") {
      fill <- unname(COULEURS_ALERTE[as.character(g$alerte)])
      fill[is.na(fill)] <- "#BDBDBD"
    } else {
      pal <- colorNumeric("RdYlGn", domain = D$srd[[input$indic]],
                          reverse = !(input$indic %in% FAVORABLES), na.color = "#BDBDBD")
      fill <- pal(v)
    }
    etiquette <- sprintf("<b>%s</b><br>%s : %s<br>Alerte : %s",
                         g$zone, LIBELLES[input$indic], fmt_num(v),
                         ifelse(is.na(g$alerte), "n.d.", as.character(g$alerte))) |>
      lapply(HTML)

    p <- leafletProxy("carte", data = g) |>
      clearShapes() |> clearControls() |>
      addPolygons(layerId = ~zone, fillColor = fill, fillOpacity = 0.8,
                  color = "white", weight = 1.5, label = etiquette,
                  highlightOptions = highlightOptions(weight = 3, color = "#212121",
                                                      bringToFront = TRUE))
    if (input$indic == "srd") {
      p |> addLegend("bottomright", colors = unname(COULEURS_ALERTE),
                     labels = c("Vert", "Jaune", "Orange", "Rouge"),
                     title = "Niveau d'alerte", opacity = 0.9)
    } else {
      p |> addLegend("bottomright", pal = pal, values = D$srd[[input$indic]],
                     title = LIBELLES[input$indic], opacity = 0.9)
    }
  })

  observeEvent(input$carte_shape_click, zone_sel(input$carte_shape_click$id))

  output$titre_fiche <- renderText({
    if (is.null(zone_sel())) "Fiche région" else paste("Fiche ·", zone_sel())
  })

  output$fiche <- renderUI({
    z <- zone_sel()
    if (is.null(z)) return(p(class = "text-muted", "Sélectionnez une région sur la carte."))
    l <- D$srd |> filter(zone == z, annee == input$annee)
    if (!nrow(l)) return(p("Pas de données pour cette année."))
    nat <- if (!is.null(D$national)) D$national |> filter(annee == input$annee) else NULL
    pr <- if (!is.null(D$prevision)) D$prevision |> filter(zone == z) else NULL
    rang <- D$srd |> filter(annee == input$annee) |> mutate(r = min_rank(desc(srd))) |>
      filter(zone == z) |> pull(r)
    comparer <- function(v, lib, unite = " %") {
      if (!v %in% names(l)) return(NULL)
      n <- if (!is.null(nat) && nrow(nat) && v %in% names(nat)) paste0(" (national : ", fmt_num(nat[[v]]), unite, ")") else ""
      tags$li(sprintf("%s : %s%s%s", lib, fmt_num(l[[v]]), unite, n))
    }
    tagList(
      div(style = sprintf("border-left: 6px solid %s; padding-left: 10px;", COULEURS_ALERTE[as.character(l$alerte)]),
          h4(sprintf("SRD %s / 100", fmt_num(l$srd))),
          p(sprintf("Alerte %s · %de région la plus à risque sur %d",
                    toupper(as.character(l$alerte)), rang, n_regions)),
          p(class = "small", sprintf("Situation %s / 100 · Dégradation %s / 100",
                                     fmt_num(l$situation, 0), fmt_num(l$degradation, 0)))),
      tags$ul(
        comparer("tap", "Taux d'achèvement"),
        comparer("non_achevement_relatif", "Entrants au CP1 qui n'achèvent pas"),
        comparer("evol_tap_3ans", "Évolution de l'achèvement sur 3 ans", " pts"),
        comparer("tbs", "Taux brut de scolarisation"),
        comparer("reussite_cep", "Réussite au CEP"),
        comparer("evol_eleves_3ans", "Évolution des effectifs sur 3 ans"),
        if (est_projection() && "tap_bas" %in% names(l))
          tags$li(strong(sprintf("Achèvement projeté : %s %% (fourchette %s – %s %%)",
                                 fmt_num(l$tap), fmt_num(l$tap_bas), fmt_num(l$tap_haut)))),
        if (!est_projection() && !is.null(pr) && nrow(pr)) tags$li(strong(sprintf("Achèvement prévu en %d : %s %% (± %s) · %s",
                       pr$annee_prevue, fmt_num(pr$prevu_retenu), fmt_num(pr$erreur_moyenne_attendue),
                       pr$direction_prevue)))
      )
    )
  })

  output$courbes <- renderPlot({
    z <- zone_sel(); req(z)
    vars <- intersect(c("tap", "tbs", "reussite_cep"), names(D$srd))
    noms <- c(tap = "Achèvement (%)", tbs = "Scolarisation (%)", reussite_cep = "Réussite CEP (%)")
    dz <- D$srd |> filter(zone == z)
    d <- dz |>
      select(annee, type, all_of(vars)) |>
      pivot_longer(-c(annee, type), names_to = "indicateur", values_to = "valeur") |>
      mutate(indicateur = noms[indicateur])
    # la projection part du dernier point observé (ligne continue entre les deux)
    jonction <- d |> filter(annee == D$derniere_obs) |> mutate(type = "projection")
    d <- bind_rows(d, jonction)
    g <- ggplot(d, aes(annee, valeur, colour = indicateur))
    if ("tap_bas" %in% names(dz)) {
      ruban <- dz |> filter(type == "projection") |>
        transmute(annee, bas = tap_bas, haut = tap_haut, indicateur = noms[["tap"]])
      g <- g + geom_ribbon(data = ruban, aes(x = annee, ymin = bas, ymax = haut), inherit.aes = FALSE,
                           fill = "#C62828", alpha = 0.12)
    }
    g + geom_line(aes(linetype = type), linewidth = 0.9) +
      geom_point(data = filter(d, type == "observé"), size = 1.5) +
      geom_vline(xintercept = input$annee, linetype = "dotted", colour = "grey50") +
      facet_wrap(~indicateur, scales = "free_y", ncol = 3) +
      scale_colour_manual(values = c("#C62828", "#EF6C00", "#1565C0"), guide = "none") +
      scale_linetype_manual(values = c("observé" = "solid", "projection" = "22"), guide = "none") +
      scale_x_continuous(breaks = scales::breaks_pretty(4)) +
      labs(x = NULL, y = NULL, caption = "Trait plein : observé · pointillés : projection (zone colorée : fourchette)") +
      theme_minimal(base_size = 12) + theme(strip.text = element_text(face = "bold"))
  }, res = 96)

  output$resume_national <- renderUI({
    d <- D$srd |> filter(annee == input$annee)
    n <- table(factor(d$alerte, levels = names(COULEURS_ALERTE)))
    tagList(
      strong(sprintf("Situation en %d", input$annee)),
      tags$ul(lapply(names(n), \(k) tags$li(
        span(style = sprintf("color:%s;font-weight:bold;", COULEURS_ALERTE[k]), "■ "),
        sprintf("%s : %d région(s)", tools::toTitleCase(k), n[[k]]))))
    )
  })

  output$titre_top <- renderText(sprintf("Classement des régions en %d", input$annee))
  output$top <- renderDT({
    D$srd |> filter(annee == input$annee) |> arrange(desc(srd)) |>
      transmute(`Région` = zone, SRD = round(srd, 1), Alerte = as.character(alerte),
                Situation = round(situation), `Dégradation` = round(degradation),
                `Achèvement (%)` = round(tap, 1),
                `Évol. effectifs 3 ans (%)` = round(evol_eleves_3ans, 1)) |>
      datatable(rownames = FALSE, options = list(dom = "t", pageLength = 13)) |>
      formatStyle("Alerte", backgroundColor = styleEqual(names(COULEURS_ALERTE), COULEURS_ALERTE),
                  color = "white", fontWeight = "bold")
  })

  output$prev <- renderDT({
    req(D$prevision)
    D$prevision |>
      transmute(`Région` = zone,
                `Achèvement actuel (%)` = round(tap_actuel, 1),
                `Achèvement prévu (%)` = round(prevu_retenu, 1),
                `Marge d'erreur (± pts)` = round(erreur_moyenne_attendue, 1),
                `Direction prévue` = direction_prevue,
                `Fiabilité de la direction` = sprintf("%.0f %%", 100 * fiabilite_direction)) |>
      datatable(rownames = FALSE, options = list(dom = "t", pageLength = 13))
  })

  output$matrice <- renderPlot({
    d <- D$srd |> filter(annee == input$annee)
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
      labs(x = "Situation (0 = la meilleure, 100 = la pire)",
           y = "Dégradation sur 3 ans (100 = la plus forte)") +
      theme_minimal(base_size = 12) + theme(legend.position = "bottom")
  })

  output$poids <- renderTable({
    req(D$poids)
    D$poids |> transmute(Axe = axe, Composante = variable,
                         `Poids dans l'axe` = sprintf("%.0f %%", 100 * poids_dans_axe),
                         `Sens` = if_else(sens > 0, "↑ risque", "↓ risque"),
                         Justification = justification)
  })

  output$fiab <- renderTable({
    req(D$fiabilite)
    D$fiabilite |> transmute(`Modèle` = modele, `Année test` = as.integer(annee_cible),
                             MAE = round(MAE, 2), RMSE = round(RMSE, 2),
                             `Rappel alerte` = sprintf("%.0f %%", 100 * rappel_alerte),
                             `Sens de variation correct` = ifelse(is.na(sens_correct), "–",
                                                                  sprintf("%.0f %%", 100 * sens_correct)))
  })
}

shinyApp(ui, server)
