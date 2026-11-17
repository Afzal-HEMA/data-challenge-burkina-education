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

# ---- Interface ---------------------------------------------
ui <- page_navbar(
  title = "Alerte décrochage scolaire · Burkina Faso",
  bg = "#2B3446", inverse = TRUE,
  theme = bs_theme(version = 5, primary = "#F47A5B", base_font = font_collection("system-ui", "sans-serif")),
  header = if (D$fictif) div(class = "alert alert-danger text-center m-0 rounded-0",
                              strong("DONNÉES FICTIVES : démonstration uniquement")),

  nav_panel("Accueil", icon = icon("house"),
    if (!is.null(A)) accueil_ui("acc", zones_acc, sort(unique(A$annee))) else
      p(class = "m-4 text-muted", "Tableau d'accueil absent : lancez l'étape 03_transformation.")),

  nav_panel("Carte d'alerte", icon = icon("map-location-dot"),
    layout_sidebar(
      sidebar = sidebar(width = 300,
        sliderInput("annee", "Année scolaire (année de fin)", min = min(D$annees), max = annee_max,
                    value = annee_max, step = 1, sep = ""),
        selectInput("indic", "Indicateur affiché",
                    choices = setNames(names(LIBELLES), LIBELLES)[names(LIBELLES) %in% names(D$srd)],
                    selected = "srd"),
        helpText("Cliquez sur une province pour afficher sa fiche."),
        hr(),
        uiOutput("resume_national")
      ),
      layout_columns(col_widths = c(7, 5),
        card(full_screen = TRUE, card_header(textOutput("titre_carte")),
             leafletOutput("carte", height = 560)),
        card(card_header(textOutput("titre_fiche")), uiOutput("fiche"),
             plotOutput("courbes", height = 230))
      )
    )
  ),

  nav_panel("Classement", icon = icon("ranking-star"),
    layout_columns(col_widths = c(6, 6),
      card(card_header(textOutput("titre_top")), DTOutput("top")),
      card(card_header("Prévision du taux d'abandon l'an prochain"), DTOutput("prev"))
    )
  ),

  nav_panel("Méthode et fiabilité", icon = icon("flask"),
    layout_columns(col_widths = c(6, 6),
      card(card_header("Composantes du SRD et poids"), tableOutput("poids"),
           p(class = "small text-muted",
             "Chaque composante est convertie en rang percentile sur toutes les provinces et",
             "toutes les années, puis pondérée. Niveaux d'alerte : médiane, 75e et 90e",
             "percentiles du SRD. Source : DGESS, Ministère de l'Éducation nationale.")),
      card(card_header("Fiabilité du modèle (années de test)"), tableOutput("fiab"),
           p(class = "small text-muted",
             "MAE : erreur moyenne en points de pourcentage. Rappel top 10 : part des 10",
             "provinces réellement les plus touchées que le modèle a repérées. « naif » = on",
             "suppose que l'an prochain sera identique à cette année."))
    )
  )
)

# ---- Serveur -----------------------------------------------
server <- function(input, output, session) {

  if (!is.null(A)) accueil_server("acc", A)

  province_sel <- reactiveVal(NULL)
  donnees_an <- reactive(carte_annee(D, input$annee))

  output$titre_carte <- renderText(paste0(LIBELLES[input$indic], " · ", input$annee))

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
      legende <- list(colors = unname(COULEURS_ALERTE), labels = c("Vert", "Jaune", "Orange", "Rouge"))
    } else {
      inverse <- input$indic %in% c("taux_achevement", "indice_parite")
      pal <- colorNumeric(if (inverse) "RdYlGn" else "YlOrRd", domain = D$srd[[input$indic]],
                          reverse = FALSE, na.color = "#BDBDBD")
      fill <- pal(v)
    }
    etiquette <- sprintf("<b>%s</b> (%s)<br>%s : %s<br>Alerte : %s",
                         g$province, g$region, LIBELLES[input$indic],
                         ifelse(is.na(v), "n.d.", format(round(v, 1), decimal.mark = ",")),
                         ifelse(is.na(g$alerte), "n.d.", as.character(g$alerte))) |>
      lapply(HTML)

    p <- leafletProxy("carte", data = g) |>
      clearShapes() |> clearControls() |>
      addPolygons(layerId = ~province, fillColor = fill, fillOpacity = 0.8,
                  color = "white", weight = 1, label = etiquette,
                  highlightOptions = highlightOptions(weight = 3, color = "#212121",
                                                      bringToFront = TRUE))
    if (input$indic == "srd") {
      p |> addLegend("bottomright", colors = legende$colors, labels = legende$labels,
                     title = "Niveau d'alerte", opacity = 0.9)
    } else {
      p |> addLegend("bottomright", pal = pal, values = D$srd[[input$indic]],
                     title = LIBELLES[input$indic], opacity = 0.9)
    }
  })

  observeEvent(input$carte_shape_click, province_sel(input$carte_shape_click$id))

  output$titre_fiche <- renderText({
    if (is.null(province_sel())) "Fiche province" else paste("Fiche ·", province_sel())
  })

  output$fiche <- renderUI({
    p <- province_sel()
    if (is.null(p)) return(p(class = "text-muted", "Sélectionnez une province sur la carte."))
    l <- D$srd |> filter(province == p, annee == input$annee)
    if (!nrow(l)) return(p("Pas de données pour cette année."))
    pr <- if (!is.null(D$prevision)) D$prevision |> filter(province == p) else NULL
    rang <- D$srd |> filter(annee == input$annee) |> mutate(r = min_rank(desc(srd))) |>
      filter(province == p) |> pull(r)
    couleur <- COULEURS_ALERTE[as.character(l$alerte)]
    tagList(
      div(style = sprintf("border-left: 6px solid %s; padding-left: 10px;", couleur),
          h4(sprintf("SRD %s / 100", format(round(l$srd, 1), decimal.mark = ","))),
          p(sprintf("Alerte %s · %de province la plus à risque sur 45 · Région %s",
                    toupper(as.character(l$alerte)), rang, l$region))),
      tags$ul(
        if ("taux_abandon" %in% names(l)) tags$li(sprintf("Taux d'abandon : %s %%", format(round(l$taux_abandon, 1), decimal.mark = ","))),
        if ("taux_achevement" %in% names(l)) tags$li(sprintf("Taux d'achèvement : %s %%", format(round(l$taux_achevement, 1), decimal.mark = ","))),
        if (!is.null(pr) && nrow(pr)) tags$li(strong(sprintf("Abandon prévu en %d : %s %% (± %s)",
                       pr$annee_prevue, format(round(pr$prevu_retenu, 1), decimal.mark = ","),
                       format(round(pr$erreur_moyenne_attendue, 1), decimal.mark = ","))))
      )
    )
  })

  output$courbes <- renderPlot({
    p <- province_sel(); req(p)
    vars <- intersect(c("taux_abandon", "taux_redoublement", "taux_achevement"), names(D$srd))
    d <- D$srd |> filter(province == p) |>
      select(annee, all_of(vars)) |>
      pivot_longer(-annee, names_to = "indicateur", values_to = "valeur") |>
      mutate(indicateur = LIBELLES[indicateur])
    ggplot(d, aes(annee, valeur, colour = indicateur)) +
      geom_line(linewidth = 1) + geom_point(size = 1.8) +
      geom_vline(xintercept = input$annee, linetype = "dashed", colour = "grey50") +
      facet_wrap(~indicateur, scales = "free_y", ncol = 3) +
      scale_colour_manual(values = c("#C62828", "#EF6C00", "#1565C0"), guide = "none") +
      labs(x = NULL, y = NULL) + theme_minimal(base_size = 11)
  })

  output$resume_national <- renderUI({
    d <- D$srd |> filter(annee == input$annee)
    n <- table(factor(d$alerte, levels = names(COULEURS_ALERTE)))
    tagList(
      strong(sprintf("Situation en %d", input$annee)),
      tags$ul(lapply(names(n), \(k) tags$li(
        span(style = sprintf("color:%s;font-weight:bold;", COULEURS_ALERTE[k]), "■ "),
        sprintf("%s : %d province(s)", tools::toTitleCase(k), n[[k]]))))
    )
  })

  output$titre_top <- renderText(sprintf("Les 10 provinces les plus à risque en %d", input$annee))
  output$top <- renderDT({
    D$srd |> filter(annee == input$annee) |> slice_max(srd, n = 10) |>
      transmute(Province = province, `Région` = region, SRD = round(srd, 1),
                Alerte = as.character(alerte), `Évolution SRD` = round(evol_srd, 1),
                `Abandon (%)` = round(taux_abandon, 1)) |>
      datatable(rownames = FALSE, options = list(dom = "t", pageLength = 10)) |>
      formatStyle("Alerte", backgroundColor = styleEqual(names(COULEURS_ALERTE), COULEURS_ALERTE),
                  color = "white", fontWeight = "bold")
  })

  output$prev <- renderDT({
    req(D$prevision)
    D$prevision |>
      transmute(Province = province, `Région` = region,
                `Abandon actuel (%)` = round(taux_abandon_actuel, 1),
                `Abandon prévu (%)` = round(prevu_retenu, 1),
                `Écart` = round(prevu_retenu - taux_abandon_actuel, 1)) |>
      datatable(rownames = FALSE, options = list(pageLength = 10))
  })

  output$poids <- renderTable({
    req(D$poids)
    D$poids |> transmute(Composante = variable,
                         `Poids` = sprintf("%.0f %%", 100 * poids_effectif),
                         `Sens` = if_else(sens > 0, "↑ risque", "↓ risque"),
                         Justification = justification)
  })

  output$fiab <- renderTable({
    req(D$fiabilite)
    D$fiabilite |> transmute(`Modèle` = modele, `Année test` = as.integer(annee_cible),
                             MAE = round(MAE, 2), RMSE = round(RMSE, 2),
                             `Rappel top 10` = sprintf("%.0f %%", 100 * rappel_top10))
  })
}

shinyApp(ui, server)
