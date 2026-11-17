# ============================================================
# Onglet Simulateur : aide à la décision
#  1. « Que se passe-t-il si... ? » : curseurs de leviers -> indicateurs,
#     SRD et alerte recalculés (avant / après).
#  2. « Que faut-il pour passer au vert ? » : effort minimal calculé pour
#     chaque couleur, traduit en enfants à scolariser, enseignants, écoles.
#  3. Vue d'ensemble : effort pour amener chaque région au vert.
#
# Logique « à terme » : on compare la situation de la région une fois les
# mesures pleinement effectives (quand les cohortes concernées ont achevé
# le primaire), toutes choses égales par ailleurs.
# ============================================================

source(here::here("R", "fonctions_srd.R"))

ORDRE_ALERTE <- c("vert", "jaune", "orange", "rouge")
MAX_ENS <- 15000
MAX_ECOLES <- 1500

# ---- Moteur de simulation ------------------------------------
# base  : une ligne (région, année) avec tap, tba, tbs, reussite_cep, ips_tap,
#         eleves, ecoles, non_achevement_relatif, tap_3, eleves_3 (valeurs 3 ans avant)
# lev   : list(ecoles, enseignants, maintien, cep, parite, eleves_direct)
# param : list(elasticite, eleves_par_enseignant)
simuler <- function(base, lev, param) {
  pop_6_11 <- base$eleves / (base$tbs / 100)            # population d'âge scolaire déduite du TBS
  hors_ecole <- max(0, pop_6_11 - base$eleves)          # enfants non scolarisés (TBS plafonné à 100)
  extra_ecoles <- if (isTRUE(base$ecoles > 0) && lev$ecoles > 0)
    base$eleves * (((base$ecoles + lev$ecoles) / base$ecoles)^param$elasticite - 1) else 0
  extra_ens <- lev$enseignants * param$eleves_par_enseignant
  extra <- min(extra_ecoles + extra_ens + coalesce(lev$eleves_direct, 0), hors_ecole)
  facteur <- (base$eleves + extra) / base$eleves

  out <- base
  out$eleves <- base$eleves + extra
  out$tbs <- base$tbs * facteur
  out$tba <- base$tba * facteur                         # l'accès au CP1 suit la capacité d'accueil
  out$non_achevement_relatif <- base$non_achevement_relatif * (1 - lev$maintien / 100)
  out$tap <- out$tba * (1 - out$non_achevement_relatif / 100)
  out$reussite_cep <- min(100, base$reussite_cep + lev$cep)
  out$ips_tap <- 1 + (base$ips_tap - 1) * (1 - lev$parite / 100)
  out$ecart_parite_tap <- abs(1 - out$ips_tap)
  out$evol_tap_3ans <- out$tap - base$tap_3
  out$evol_eleves_3ans <- 100 * (out$eleves / base$eleves_3 - 1)
  out$eleves_supplementaires <- extra
  out$hors_ecole <- hors_ecole
  out
}

# Effort minimal pour atteindre une couleur : on rapproche ensemble toutes les
# composantes de la situation d'un « bon niveau » (25 % des meilleures valeurs
# observées) et on cherche le plus petit pas qui suffit.
plan_vers <- function(base, ref, poids, seuils, param, couleur_cible) {
  cible_tbs <- max(base$tbs, quantile(ref$tbs, 0.75, na.rm = TRUE))
  cible_na  <- min(base$non_achevement_relatif, quantile(ref$non_achevement_relatif, 0.25, na.rm = TRUE))
  cible_cep <- max(base$reussite_cep, quantile(ref$reussite_cep, 0.75, na.rm = TRUE))
  rang_cible <- match(couleur_cible, ORDRE_ALERTE)
  for (t in seq(0, 1, by = 0.02)) {
    tbs_t <- base$tbs + t * (cible_tbs - base$tbs)
    lev <- list(ecoles = 0, enseignants = 0,
                maintien = 100 * (1 - (base$non_achevement_relatif + t * (cible_na - base$non_achevement_relatif)) /
                                    base$non_achevement_relatif),
                cep = t * (cible_cep - base$reussite_cep),
                parite = 100 * t,
                eleves_direct = (tbs_t / base$tbs - 1) * base$eleves)
    sim <- scorer_srd(simuler(base, lev, param), ref, poids, seuils)
    if (match(sim$alerte, ORDRE_ALERTE) <= rang_cible) {
      extra <- sim$eleves_supplementaires
      return(tibble(
        objectif = couleur_cible, atteignable = TRUE, effort = t,
        eleves_en_plus = extra,
        enseignants = extra / param$eleves_par_enseignant,
        ecoles = if (isTRUE(base$ecoles > 0)) extra / (base$eleves / base$ecoles) else NA_real_,
        reduction_non_achevement = lev$maintien,
        points_cep = lev$cep, reduction_ecart_filles = lev$parite,
        tap_atteint = sim$tap, srd_atteint = sim$srd))
    }
  }
  tibble(objectif = couleur_cible, atteignable = FALSE, effort = NA_real_)
}

fmt_ent <- function(x) ifelse(is.na(x), "–", format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE))
pastille <- function(alerte) span(class = "badge", style = sprintf("background:%s;font-size:0.85rem;", COULEURS_ALERTE[[alerte]]),
                                  toupper(alerte))

# ---- Interface -----------------------------------------------
simulateur_ui <- function(id, zones, annees, defaut_annee, param_defaut) {
  ns <- NS(id)
  layout_sidebar(fillable = FALSE,
    sidebar = sidebar(width = 320,
      selectInput(ns("zone"), "Région", choices = zones, selected = zones[1]),
      selectInput(ns("annee"), "Situation de départ",
                  choices = setNames(annees, ifelse(annees > defaut_annee, paste(annees, "(projection)"), paste(annees, "(observé)"))),
                  selected = defaut_annee),
      hr(),
      strong("Leviers à tester"),
      sliderInput(ns("ecoles"), "Écoles supplémentaires", 0, MAX_ECOLES, 0, step = 10),
      sliderInput(ns("enseignants"), "Enseignants supplémentaires", 0, MAX_ENS, 0, step = 50),
      sliderInput(ns("maintien"), "Programme de maintien à l'école (cantines, kits, bourses) : baisse du non-achèvement (%)",
                  0, 60, 0, step = 5, post = " %"),
      sliderInput(ns("cep"), "Remédiation scolaire : réussite au CEP (+ points)", 0, 25, 0, step = 1),
      sliderInput(ns("parite"), "Actions pour les filles : réduction de l'écart filles/garçons (%)", 0, 100, 0, step = 10, post = " %"),
      actionButton(ns("raz"), "Remettre à zéro", class = "btn-sm btn-outline-secondary"),
      accordion(open = FALSE,
        accordion_panel("Hypothèses (modifiables)",
          numericInput(ns("ratio"), "Élèves par enseignant supplémentaire", param_defaut$eleves_par_enseignant, 20, 90, 5),
          numericInput(ns("elasticite"), "Élasticité élèves / écoles (estimée)", round(param_defaut$elasticite, 2), 0, 1.5, 0.05),
          numericInput(ns("cout_ens"), "Coût annuel d'un enseignant (FCFA, facultatif)", 0, 0, step = 100000),
          numericInput(ns("cout_ecole"), "Coût de construction d'une école (FCFA, facultatif)", 0, 0, step = 1e6)))
    ),
    layout_columns(col_widths = c(6, 6),
      card(min_height = 330, card_header("Avant / après les mesures testées"), uiOutput(ns("avant_apres"))),
      card(min_height = 330, card_header("Position dans la matrice de risque"), plotOutput(ns("matrice"), height = 300))
    ),
    layout_columns(col_widths = c(5, 7),
      card(card_header("Indicateurs (à terme)"), tableOutput(ns("indicateurs"))),
      card(card_header(textOutput(ns("titre_plan"))), uiOutput(ns("plan")),
           p(class = "small text-muted mt-2",
             "Effort minimal : toutes les composantes de la situation sont rapprochées ensemble d'un « bon niveau »",
             "(les 25 % meilleures valeurs observées en 2016-2025) jusqu'à atteindre la couleur visée."))
    ),
    card(card_header(textOutput(ns("titre_ensemble")),
                     span(class = "small text-muted ms-2", "à terme, avec les hypothèses du panneau de gauche")),
         DTOutput(ns("ensemble")))
  )
}

# ---- Serveur -------------------------------------------------
simulateur_server <- function(id, D, param_defaut) {
  moduleServer(id, function(input, output, session) {
    ref <- D$srd |> filter(type == "observé")
    ecoles_dernieres <- ref |> filter(!is.na(ecoles)) |> group_by(zone) |>
      slice_max(annee, n = 1) |> select(zone, ecoles_ref = ecoles)

    base_de <- function(z, an) {
      b <- D$srd |> filter(zone == z, annee == an)
      h <- D$srd |> filter(zone == z, annee == an - 3) |> select(tap_3 = tap, eleves_3 = eleves)
      b <- bind_cols(b, h)
      if (!"ecoles" %in% names(b) || is.na(b$ecoles)) b$ecoles <- ecoles_dernieres$ecoles_ref[ecoles_dernieres$zone == z]
      b
    }
    base <- reactive(base_de(input$zone, as.integer(input$annee)))
    param <- reactive(list(elasticite = input$elasticite, eleves_par_enseignant = input$ratio))
    leviers <- reactive(list(ecoles = input$ecoles, enseignants = input$enseignants, maintien = input$maintien,
                             cep = input$cep, parite = input$parite, eleves_direct = 0))

    avant <- reactive(scorer_srd(base(), ref, D$poids, D$seuils))
    apres <- reactive(scorer_srd(simuler(base(), leviers(), param()), ref, D$poids, D$seuils))

    observeEvent(input$raz, {
      for (k in c("ecoles", "enseignants", "maintien", "cep", "parite")) updateSliderInput(session, k, value = 0)
    })

    output$avant_apres <- renderUI({
      a <- avant(); b <- apres()
      bloc <- function(titre, x) div(class = "text-center p-2",
        div(class = "text-muted small", titre), pastille(x$alerte),
        h3(class = "mt-2 mb-0", fmt_num(x$srd)), div(class = "small text-muted", "SRD / 100"),
        div(class = "small mt-1", sprintf("Situation %s · Dégradation %s", fmt_num(x$situation, 0), fmt_num(x$degradation, 0))))
      cout <- input$enseignants * input$cout_ens + input$ecoles * input$cout_ecole
      tagList(
        layout_columns(col_widths = c(5, 2, 5), bloc("Avant", a),
                       div(class = "text-center", style = "font-size:2rem;padding-top:2.5rem;", icon("arrow-right")),
                       bloc("Après", b)),
        tags$ul(class = "small mb-0",
          tags$li(sprintf("Enfants scolarisés en plus : %s (sur %s enfants de 6-11 ans hors de l'école)",
                          fmt_ent(b$eleves_supplementaires), fmt_ent(b$hors_ecole))),
          if (b$hors_ecole < 1 && (input$ecoles + input$enseignants) > 0)
            tags$li(class = "text-warning", "Scolarisation déjà complète (TBS ≥ 100 %) : écoles et enseignants n'augmentent pas les effectifs."),
          if (cout > 0) tags$li(strong(sprintf("Coût indicatif : %s FCFA", fmt_ent(cout)))))
      )
    })

    output$indicateurs <- renderTable({
      a <- avant(); b <- apres()
      lignes <- c(tap = "Achèvement (%)", tbs = "Scolarisation TBS (%)", tba = "Admission TBA (%)",
                  non_achevement_relatif = "Entrants qui n'achèvent pas (%)",
                  reussite_cep = "Réussite au CEP (%)", ips_tap = "Parité F/G de l'achèvement",
                  eleves = "Élèves du primaire")
      tibble(Indicateur = unname(lignes),
             Avant = map_chr(names(lignes), \(v) if (v == "eleves") fmt_ent(a[[v]]) else fmt_num(a[[v]], if (v == "ips_tap") 2 else 1)),
             `Après` = map_chr(names(lignes), \(v) if (v == "eleves") fmt_ent(b[[v]]) else fmt_num(b[[v]], if (v == "ips_tap") 2 else 1)))
    }, striped = TRUE, spacing = "s", align = "lrr")

    output$matrice <- renderPlot({
      a <- avant(); b <- apres()
      autres <- D$srd |> filter(annee == as.integer(input$annee), zone != input$zone)
      s <- D$seuils
      ggplot() +
        geom_vline(xintercept = s$seuil[s$axe == "situation"], linetype = "dashed", colour = "grey80") +
        geom_hline(yintercept = s$seuil[s$axe == "degradation"], linetype = "dotted", colour = "grey80") +
        geom_point(data = autres, aes(situation, degradation), colour = "grey70", size = 2.5) +
        geom_text(data = autres, aes(situation, degradation, label = zone), colour = "grey60", size = 2.8, vjust = -1) +
        geom_segment(aes(x = a$situation, y = a$degradation, xend = b$situation, yend = b$degradation),
                     arrow = arrow(length = unit(0.25, "cm")), linewidth = 0.9, colour = "#2D3436") +
        geom_point(aes(a$situation, a$degradation), colour = COULEURS_ALERTE[[a$alerte]], size = 5) +
        geom_point(aes(b$situation, b$degradation), colour = COULEURS_ALERTE[[b$alerte]], size = 5, shape = 21,
                   fill = COULEURS_ALERTE[[b$alerte]], stroke = 1.5) +
        annotate("text", x = a$situation, y = a$degradation, label = input$zone, vjust = 2, fontface = "bold", size = 3.5) +
        scale_x_continuous(limits = c(0, 100)) + scale_y_continuous(limits = c(0, 105)) +
        labs(x = "Situation (100 = la pire)", y = "Dégradation sur 3 ans") +
        theme_minimal(base_size = 11)
    }, res = 96)

    plans <- reactive({
      a <- avant()
      cibles <- ORDRE_ALERTE[seq_len(match(a$alerte, ORDRE_ALERTE) - 1)]
      if (!length(cibles)) return(NULL)
      map_dfr(rev(cibles), \(k) plan_vers(base(), ref, D$poids, D$seuils, param(), k))
    })

    output$titre_plan <- renderText(sprintf("Que faut-il pour que %s change de couleur ?", input$zone))
    output$plan <- renderUI({
      a <- avant(); p <- plans()
      if (is.null(p)) return(p(strong(input$zone), "est déjà au vert pour cette année."))
      lignes <- lapply(seq_len(nrow(p)), \(i) {
        x <- p[i, ]
        if (!x$atteignable) return(tags$tr(tags$td(pastille(x$objectif)),
          tags$td(colspan = 6, class = "small", "Non atteignable avec ces leviers seuls : la dégradation des 3 dernières années pèse trop.")))
        tags$tr(tags$td(pastille(x$objectif)),
                tags$td(fmt_ent(x$eleves_en_plus)), tags$td(fmt_ent(x$enseignants)), tags$td(fmt_ent(x$ecoles)),
                tags$td(sprintf("−%s %%", fmt_num(x$reduction_non_achevement, 0))),
                tags$td(sprintf("+%s pts", fmt_num(x$points_cep, 1))),
                tags$td(sprintf("%s → %s %%", fmt_num(a$tap), fmt_num(x$tap_atteint))))
      })
      tagList(
        tags$table(class = "table table-sm small mb-2",
          tags$thead(tags$tr(tags$th("Objectif"), tags$th("Enfants à scolariser"), tags$th("Enseignants"),
                             tags$th("Écoles"), tags$th("Non-achèvement"), tags$th("CEP"), tags$th("Achèvement"))),
          tags$tbody(lignes)),
        p(class = "small text-muted mb-2",
          "Enseignants et écoles : moyens nécessaires pour accueillir ces enfants aux ratios actuels de la région",
          "(ce ne sont pas deux leviers à additionner)."),
        if (any(p$atteignable)) actionButton(session$ns("appliquer"), "Tester le plan le plus ambitieux atteignable dans les curseurs",
                                            class = "btn-sm btn-primary")
      )
    })

    observeEvent(input$appliquer, {
      p <- plans(); p <- p[p$atteignable, ]
      if (!nrow(p)) return()
      x <- p[nrow(p), ]
      updateSliderInput(session, "ecoles", value = 0)
      # arrondi vers le haut au pas du curseur, pour que le plan reste suffisant
      updateSliderInput(session, "enseignants", value = min(MAX_ENS, ceiling(x$enseignants / 50) * 50))
      updateSliderInput(session, "maintien", value = min(60, ceiling(x$reduction_non_achevement / 5) * 5))
      updateSliderInput(session, "cep", value = min(25, ceiling(x$points_cep)))
      updateSliderInput(session, "parite", value = min(100, ceiling(x$reduction_ecart_filles / 10) * 10))
    })

    output$titre_ensemble <- renderText(sprintf("Vue d'ensemble %s : effort pour amener chaque région au vert", input$annee))
    output$ensemble <- renderDT({
      an <- as.integer(input$annee)
      zones <- D$srd |> filter(annee == an) |> arrange(desc(srd)) |> pull(zone)
      tab <- map_dfr(zones, \(z) {
        b <- base_de(z, an); a <- scorer_srd(b, ref, D$poids, D$seuils)
        p <- if (a$alerte == "vert") tibble(atteignable = TRUE, eleves_en_plus = 0, enseignants = 0, ecoles = 0,
                                            reduction_non_achevement = 0, points_cep = 0)
             else plan_vers(b, ref, D$poids, D$seuils, param(), "vert")
        tibble(`Région` = z, Alerte = a$alerte, SRD = round(a$srd, 1),
               `Au vert ?` = ifelse(a$alerte == "vert", "déjà", ifelse(p$atteignable, "oui", "non (dégradation)")),
               `Enfants à scolariser` = if (p$atteignable) round(p$eleves_en_plus) else NA,
               Enseignants = if (p$atteignable) round(p$enseignants) else NA,
               `Écoles` = if (p$atteignable) round(p$ecoles) else NA,
               `Baisse non-achèvement (%)` = if (p$atteignable) round(p$reduction_non_achevement) else NA,
               `CEP (+pts)` = if (p$atteignable) round(p$points_cep, 1) else NA)
      })
      datatable(tab, rownames = FALSE, options = list(dom = "t", pageLength = 20)) |>
        formatStyle("Alerte", backgroundColor = styleEqual(names(COULEURS_ALERTE), COULEURS_ALERTE),
                    color = "white", fontWeight = "bold") |>
        formatRound(c("Enfants à scolariser", "Enseignants", "Écoles"), digits = 0, mark = " ")
    })
  })
}
