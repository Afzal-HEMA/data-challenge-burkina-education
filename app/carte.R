# ============================================================
# Onglet Carte (premier onglet) : alerte et prédictions par région
#  - grande carte, noms des régions, fond sans écritures
#  - filtres : niveau, indicateur, année (observé + 2 ans prédits), sexe, statut
#  - clic sur une région : ce qui pèse sur son résultat, recommandations
#  - flash info et notifications : régions qui passent au rouge / sortent du rouge
# ============================================================

CATALOGUE <- tribble(
  ~niveau,       ~indicateur,                 ~libelle,                                        ~sens, ~par_sexe, ~par_statut,
  "primaire",    "srd",                       "Niveau d'alerte (score de risque de décrochage)", -1,  FALSE,     FALSE,
  "primaire",    "tap",                       "Taux d'achèvement du primaire (%)",              1,    TRUE,      FALSE,
  "primaire",    "non_achevement_relatif",    "Entrants au CP1 qui n'achèvent pas (%)",         -1,   FALSE,     FALSE,
  "primaire",    "tbs",                       "Taux brut de scolarisation (%)",                 1,    TRUE,      FALSE,
  "primaire",    "tba",                       "Taux brut d'admission au CP1 (%)",               1,    TRUE,      FALSE,
  "primaire",    "reussite_cep",              "Réussite au CEP (%)",                            1,    TRUE,      FALSE,
  "primaire",    "evol_tap_3ans",             "Évolution de l'achèvement sur 3 ans (points)",   1,    FALSE,     FALSE,
  "primaire",    "evol_eleves_3ans",          "Évolution des effectifs sur 3 ans (%)",          1,    FALSE,     FALSE,
  "primaire",    "eleves",                    "Élèves du primaire",                             0,    TRUE,      FALSE,
  "primaire",    "ecoles",                    "Écoles primaires",                               0,    FALSE,     TRUE,
  "primaire",    "ctx_eleves_par_enseignant", "Élèves par enseignant (INSD, jusqu'en 2024)",    -1,   FALSE,     FALSE,
  "primaire",    "ctx_eleves_par_classe",     "Élèves par classe (INSD, jusqu'en 2024)",        -1,   FALSE,     FALSE,
  "prescolaire", "tbs_prescolaire",           "Taux brut de préscolarisation (%)",              1,    TRUE,      FALSE,
  "prescolaire", "eleves_prescolaire",        "Enfants au préscolaire",                         0,    TRUE,      FALSE,
  "prescolaire", "structures",                "Structures préscolaires",                        0,    FALSE,     TRUE,
  "prescolaire", "salles_classe",             "Salles du préscolaire",                          0,    FALSE,     TRUE
)

NOMS_COMPOSANTES <- c(
  tap = "Taux d'achèvement", non_achevement_relatif = "Entrants qui n'achèvent pas",
  tbs = "Scolarisation (TBS)", reussite_cep = "Réussite au CEP", ecart_parite_tap = "Écart filles / garçons",
  evol_tap_3ans = "Évolution de l'achèvement (3 ans)", evol_eleves_3ans = "Évolution des effectifs (3 ans)")
UNITES <- c(tap = " %", non_achevement_relatif = " %", tbs = " %", reussite_cep = " %",
            ecart_parite_tap = "", evol_tap_3ans = " pts", evol_eleves_3ans = " %")

CSS_CARTE <- "
.carte-plein { height: calc(100vh - 175px); min-height: 460px; }
.etiquette-region { font-weight: 700; font-size: 11.5px; color: #1F2937; background: transparent; border: none;
  box-shadow: none; text-shadow: 0 0 3px #fff, 0 0 3px #fff, 0 0 4px #fff; white-space: nowrap; }
.flash { display:flex; align-items:center; gap:10px; background:#1F2937; color:#fff; border-radius:8px;
  padding:6px 10px; overflow:hidden; margin-bottom:8px; font-size:13px; }
.flash-titre { background:#C62828; color:#fff; font-weight:700; padding:2px 8px; border-radius:4px; flex:none; }
.flash-defile { white-space:nowrap; overflow:hidden; flex:1; }
.flash-defile span.contenu { display:inline-block; padding-left:100%; animation: defile 28s linear infinite; }
.flash-defile:hover span.contenu { animation-play-state: paused; }
@keyframes defile { from { transform: translateX(0); } to { transform: translateX(-100%); } }
.flash .rouge { color:#FF8A80; font-weight:700; } .flash .vert { color:#B9F6CA; font-weight:700; }
.flash .orange { color:#FFD180; font-weight:700; }
.diag-ligne { margin-bottom:7px; } .diag-ligne .barre { height:7px; background:#EEF0F4; border-radius:4px; }
.diag-ligne .barre div { height:100%; border-radius:4px; }
.reco { border-left:4px solid #F47A5B; padding:4px 8px; margin-bottom:6px; background:#FFF7F4; border-radius:4px; font-size:12.5px; }
.badge-type { font-size:11px; padding:2px 6px; border-radius:4px; background:#EEF0F4; color:#4A5568; }
.badge-type.pred { background:#FFF3CD; color:#7A5B00; }
.leaflet-container { background:#F3F5F8; }
"

# ---- Diagnostic : contribution de chaque composante du SRD -----------------
diagnostic_region <- function(ligne, ref, poids, ligne_annee) {
  p <- poids
  map_dfr(seq_len(nrow(p)), \(i) {
    v <- p$variable[i]
    if (!v %in% names(ligne) || is.na(ligne[[v]])) return(NULL)
    tibble(variable = v, axe = p$axe[i], poids = p$poids[i] / sum(p$poids[p$axe == p$axe[i]]) *
             if (p$axe[i] == "situation") 0.7 else 0.3,
           valeur = ligne[[v]],
           mediane = median(ligne_annee[[v]], na.rm = TRUE),
           risque = rang_hist(ref[[v]], p$sens[i], ligne[[v]]))
  }) |>
    mutate(contribution = poids * risque) |>
    arrange(desc(contribution))
}

# ---- Recommandations à partir des signaux les plus forts -------------------
recommandations <- function(diag, extra) {
  r <- list()
  f <- function(v) { x <- diag$risque[diag$variable == v]; if (length(x)) x else 0 }
  val <- function(v) diag$valeur[diag$variable == v]
  if (max(f("tap"), f("non_achevement_relatif")) >= 0.6)
    r[[length(r) + 1]] <- list(p = max(f("tap"), f("non_achevement_relatif")), levier = "maintien",
      txt = sprintf("Trop d'enfants quittent l'école avant le CM2 (%s %% des entrants n'achèvent pas) : cantines scolaires, kits, suivi des absences, priorité aux classes de CE2 à CM2.",
                    fmt_num(val("non_achevement_relatif"), 0)))
  if (f("evol_eleves_3ans") >= 0.6)
    r[[length(r) + 1]] <- list(p = f("evol_eleves_3ans"), levier = "ecoles",
      txt = sprintf("Les effectifs ont baissé de %s %% en 3 ans : rouvrir ou remplacer les écoles fermées (écoles temporaires, éducation en situation d'urgence), accueillir les élèves déplacés.",
                    fmt_num(abs(val("evol_eleves_3ans")), 0)))
  if (f("tbs") >= 0.6)
    r[[length(r) + 1]] <- list(p = f("tbs"), levier = "capacite",
      txt = sprintf("Beaucoup d'enfants sont hors de l'école (TBS %s %%) : ouvrir des classes (salles + enseignants), campagnes d'inscription au CP1.", fmt_num(val("tbs"), 0)))
  if (f("evol_tap_3ans") >= 0.6)
    r[[length(r) + 1]] <- list(p = f("evol_tap_3ans"), levier = "cibler",
      txt = sprintf("L'achèvement recule (%s points en 3 ans) : identifier les communes les plus touchées (insécurité, déplacements) et y concentrer l'aide.",
                    fmt_num(val("evol_tap_3ans"), 1)))
  if (f("reussite_cep") >= 0.6)
    r[[length(r) + 1]] <- list(p = f("reussite_cep"), levier = "cep",
      txt = sprintf("Réussite au CEP faible (%s %%) : remédiation en CM1-CM2 et préparation à l'examen.", fmt_num(val("reussite_cep"), 0)))
  if (f("ecart_parite_tap") >= 0.6)
    r[[length(r) + 1]] <- list(p = f("ecart_parite_tap"), levier = "filles",
      txt = "Écart important entre filles et garçons à l'achèvement : bourses et kits, sensibilisation des familles, latrines séparées.")
  if (!is.null(extra$ratio) && !is.na(extra$ratio) && extra$ratio > 50)
    r[[length(r) + 1]] <- list(p = 0.65, levier = "enseignants",
      txt = sprintf("Classes chargées : %s élèves par enseignant (%s) : recruter ou redéployer des enseignants.", fmt_num(extra$ratio, 0), extra$ratio_annee))
  if (!is.null(extra$evol_ecoles) && !is.na(extra$evol_ecoles) && extra$evol_ecoles < -10)
    r[[length(r) + 1]] <- list(p = 0.7, levier = "ecoles",
      txt = sprintf("Le nombre d'écoles a baissé de %s %% en 3 ans : fermetures probables liées à l'insécurité.", fmt_num(abs(extra$evol_ecoles), 0)))
  if (!length(r)) return(list(list(p = 0, levier = NA, txt = "Aucun signal fort : maintenir l'effort actuel et surveiller l'évolution.")))
  r[order(-sapply(r, `[[`, "p"))]
}

# ---- Transitions d'alerte entre deux années (flash info, notifications) ----
transitions <- function(srd, an) {
  a <- srd |> filter(annee == an - 1) |> select(zone, avant = alerte)
  b <- srd |> filter(annee == an) |> select(zone, apres = alerte, type)
  inner_join(a, b, by = "zone") |>
    mutate(avant = as.character(avant), apres = as.character(apres),
           rang_av = match(avant, names(COULEURS_ALERTE)), rang_ap = match(apres, names(COULEURS_ALERTE))) |>
    filter(rang_av != rang_ap) |>
    mutate(nature = case_when(apres == "rouge" ~ "rouge", avant == "rouge" ~ "sortie_rouge",
                              apres == "vert" ~ "vert", rang_ap > rang_av ~ "aggravation", TRUE ~ "amelioration"))
}

# ---- Interface ---------------------------------------------------------------
carte_ui <- function(id, annees, annee_defaut, derniere_obs) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(CSS_CARTE)),
    layout_sidebar(fillable = FALSE,
      sidebar = sidebar(width = 270, open = TRUE,
        radioButtons(ns("niveau"), "Niveau",
                     choices = c("Primaire" = "primaire", "Préscolaire" = "prescolaire",
                                 "Post-primaire" = "post", "Secondaire" = "secondaire"), selected = "primaire"),
        tags$div(class = "small text-muted mb-2", style = "margin-top:-8px;",
                 "Post-primaire et secondaire : absents des données DGESS fournies."),
        tags$script(HTML(sprintf(
          "$(function(){ $('input[name=\"%s\"][value=\"post\"], input[name=\"%s\"][value=\"secondaire\"]').prop('disabled', true).parent().css('opacity', 0.45); });",
          ns("niveau"), ns("niveau")))),
        selectInput(ns("indic"), "Indicateur", choices = NULL),
        sliderInput(ns("annee"), "Année (fin d'année scolaire)", min = min(annees), max = max(annees),
                    value = annee_defaut, step = 1, sep = "", ticks = FALSE),
        div(class = "small text-muted", style = "margin-top:-10px;",
            sprintf("%d-%d : observé · %d-%d : prédit", min(annees), derniere_obs, derniere_obs + 1, max(annees))),
        hr(class = "my-2"),
        radioButtons(ns("sexe"), "Sexe", inline = TRUE,
                     choices = c("Total" = "T", "Filles" = "F", "Garçons" = "G")),
        uiOutput(ns("note_sexe")),
        radioButtons(ns("statut"), "Statut de l'école", inline = TRUE,
                     choices = c("Tous", "Public", "Privé", "Communautaire")),
        uiOutput(ns("note_statut")),
        hr(class = "my-2"),
        uiOutput(ns("resume"))
      ),
      uiOutput(ns("flash")),
      layout_columns(col_widths = c(8, 4),
        card(full_screen = TRUE, class = "mb-0",
             card_header(uiOutput(ns("titre"))),
             div(class = "carte-plein", leafletOutput(ns("carte"), height = "100%"))),
        card(class = "mb-0", style = "max-height: calc(100vh - 120px); overflow-y: auto;",
             card_header(uiOutput(ns("titre_fiche"))),
             uiOutput(ns("fiche")))
      )
    )
  )
}

# ---- Serveur -----------------------------------------------------------------
carte_server <- function(id, D) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ref <- D$srd |> filter(type == "observé")
    zone_sel <- reactiveVal(NULL)
    vers_simulateur <- reactiveVal(NULL)

    # Indicateurs proposés selon le niveau
    observeEvent(input$niveau, {
      if (input$niveau %in% c("post", "secondaire")) return()
      cat_n <- CATALOGUE |> filter(niveau == input$niveau, indicateur %in% c(unique(D$carte_long$indicateur)))
      updateSelectInput(session, "indic", choices = setNames(cat_n$indicateur, cat_n$libelle),
                        selected = cat_n$indicateur[1])
      statuts <- if (input$niveau == "prescolaire") c("Tous", "Public", "Privé", "Communautaire") else c("Tous", "Public", "Privé")
      updateRadioButtons(session, "statut", choices = statuts, selected = "Tous", inline = TRUE)
    }, ignoreInit = FALSE)

    info <- reactive({
      req(input$indic)
      CATALOGUE |> filter(indicateur == input$indic) |> slice(1)
    })
    est_pred <- reactive(input$annee > D$derniere_obs)

    output$note_sexe <- renderUI(if (!isTRUE(info()$par_sexe) && input$sexe != "T")
      div(class = "small text-warning", style = "margin-top:-8px;", "Cet indicateur n'est pas ventilé par sexe : total affiché."))
    output$note_statut <- renderUI(if (!isTRUE(info()$par_statut) && input$statut != "Tous")
      div(class = "small text-warning", style = "margin-top:-8px;",
          "Le statut ne s'applique qu'aux nombres d'écoles (les taux ne sont pas ventilés par statut)."))

    donnees <- reactive({
      req(input$indic)
      i <- info()
      sx <- if (isTRUE(i$par_sexe)) input$sexe else "T"
      st <- if (isTRUE(i$par_statut)) input$statut else "Tous"
      v <- D$carte_long |> filter(indicateur == input$indic, annee == input$annee, sexe == sx, statut == st) |>
        select(zone, valeur, type)
      al <- D$srd |> filter(annee == input$annee) |> select(zone, alerte, srd)
      D$carte |> left_join(v, by = "zone") |> left_join(al, by = "zone")
    })

    output$titre <- renderUI({
      i <- info()
      tagList(strong(i$libelle), " · ", input$annee, " ",
              span(class = paste("badge-type", if (est_pred()) "pred"),
                   if (est_pred()) "PRÉDICTION" else "observé"),
              if (isTRUE(i$par_sexe) && input$sexe != "T") span(class = "badge-type ms-1", c(F = "Filles", G = "Garçons")[[input$sexe]]),
              if (isTRUE(i$par_statut) && input$statut != "Tous") span(class = "badge-type ms-1", input$statut))
    })

    # ---- Carte ----
    output$carte <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 5, zoomControl = TRUE)) |>
        addProviderTiles(providers$CartoDB.PositronNoLabels) |>
        setView(lng = -1.55, lat = 12.25, zoom = 7)
    })

    observe({
      g <- donnees(); i <- info()
      req(nrow(g))
      if (input$indic == "srd") {
        fill <- unname(COULEURS_ALERTE[as.character(g$alerte)]); fill[is.na(fill)] <- "#BDBDBD"
      } else {
        dom <- D$carte_long |> filter(indicateur == input$indic) |> pull(valeur)
        pal <- if (i$sens == 0) colorNumeric("Blues", domain = dom, na.color = "#BDBDBD")
               else colorNumeric("RdYlGn", domain = dom, reverse = i$sens < 0, na.color = "#BDBDBD")
        fill <- pal(g$valeur)
      }
      etiquette <- sprintf("<b>%s</b><br>%s : %s<br>Alerte : %s", g$zone, i$libelle,
                           if (input$indic == "srd") fmt_num(g$srd) else fmt_num(g$valeur, if (i$sens == 0) 0 else 1),
                           ifelse(is.na(g$alerte), "n.d.", toupper(as.character(g$alerte)))) |> lapply(HTML)
      bord <- ifelse(!is.null(zone_sel()) & g$zone %in% zone_sel(), 3.5, 1.2)
      p <- leafletProxy("carte", data = g) |>
        clearShapes() |> clearControls() |> clearMarkers() |>
        addPolygons(layerId = ~zone, fillColor = fill, fillOpacity = 0.85, color = ifelse(bord > 2, "#111827", "white"),
                    weight = bord, label = etiquette,
                    highlightOptions = highlightOptions(weight = 3, color = "#111827", bringToFront = TRUE)) |>
        addLabelOnlyMarkers(data = D$centres, lng = ~lng, lat = ~lat, label = ~zone,
                            labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE,
                                                        className = "etiquette-region"))
      if (input$indic == "srd") {
        p |> addLegend("bottomright", colors = unname(COULEURS_ALERTE), labels = c("Vert", "Jaune", "Orange", "Rouge"),
                       title = "Niveau d'alerte", opacity = 0.9)
      } else {
        p |> addLegend("bottomright", pal = pal, values = dom, title = NULL, opacity = 0.9)
      }
    })

    observeEvent(input$carte_shape_click, zone_sel(input$carte_shape_click$id))

    # ---- Flash info et notifications ----
    trans <- reactive(transitions(D$srd, input$annee))
    output$flash <- renderUI({
      t <- trans()
      msg <- if (!nrow(t)) sprintf("Aucun changement de niveau d'alerte entre %d et %d.", input$annee - 1, input$annee) else
        paste(sapply(seq_len(nrow(t)), \(k) {
          x <- t[k, ]
          cl <- switch(x$nature, rouge = "rouge", sortie_rouge = "vert", vert = "vert", aggravation = "orange", amelioration = "vert")
          ic <- switch(x$nature, rouge = "▲ PASSE AU ROUGE", sortie_rouge = "▼ SORT DU ROUGE", vert = "● REVIENT AU VERT",
                       aggravation = "▲ s'aggrave", amelioration = "▼ s'améliore")
          sprintf("<span class='%s'>%s %s</span> (%s → %s)", cl, x$zone, ic, x$avant, x$apres)
        }), collapse = " &nbsp;&nbsp;•&nbsp;&nbsp; ")
      div(class = "flash", span(class = "flash-titre", icon("bolt"), sprintf(" FLASH %d", input$annee)),
          div(class = "flash-defile", span(class = "contenu", HTML(msg))))
    })
    observeEvent(input$annee, {
      t <- trans()
      t <- t |> filter(nature %in% c("rouge", "sortie_rouge", "vert"))
      for (k in seq_len(min(nrow(t), 5))) {
        x <- t[k, ]
        rouge <- x$nature == "rouge"
        showNotification(
          div(style = sprintf("border-left:5px solid %s; padding-left:8px;", if (rouge) "#C62828" else "#2E7D32"),
              strong(if (rouge) "Alerte rouge" else "Bonne nouvelle"), br(),
              sprintf("%s : %s → %s en %d%s", x$zone, x$avant, x$apres, input$annee,
                      if (input$annee > D$derniere_obs) " (prédiction)" else "")),
          type = if (rouge) "error" else "message", duration = 8)
      }
    }, ignoreInit = FALSE)

    output$resume <- renderUI({
      d <- D$srd |> filter(annee == input$annee)
      n <- table(factor(d$alerte, levels = names(COULEURS_ALERTE)))
      tagList(strong(sprintf("Alerte en %d", input$annee)),
              tags$ul(class = "list-unstyled small mb-0",
                      lapply(rev(names(n)), \(k) tags$li(span(style = sprintf("color:%s;font-size:1.1em;", COULEURS_ALERTE[k]), "■ "),
                                                          sprintf("%s : %d région(s)", tools::toTitleCase(k), n[[k]])))))
    })

    # ---- Fiche : ce qui ne va pas et recommandations ----
    output$titre_fiche <- renderUI(if (is.null(zone_sel())) strong("Cliquez sur une région") else
      tagList(strong(zone_sel()), " · ", input$annee))

    output$fiche <- renderUI({
      z <- zone_sel()
      if (is.null(z)) return(div(class = "text-muted small",
        p("Cliquez sur une région de la carte pour voir :"),
        tags$ul(tags$li("son niveau d'alerte et sa prédiction ;"), tags$li("les variables qui expliquent ce résultat ;"),
                tags$li("les actions recommandées.")),
        if (!is.null(D$modele_retenu)) p(class = "mt-3", sprintf(
          "Prédictions : modèle retenu %s (1 an) et %s (2 ans), choisis parce qu'ils se sont le moins trompés sur les années passées.",
          D$modele_retenu$modele[D$modele_retenu$h == 1], D$modele_retenu$modele[D$modele_retenu$h == 2]))))
      l <- D$srd |> filter(zone == z, annee == input$annee)
      if (!nrow(l)) return(p("Pas de données pour cette année."))
      la <- D$srd |> filter(annee == input$annee)
      diag <- diagnostic_region(l, ref, D$poids, la)

      # signaux complémentaires : écoles, enseignants
      ec <- D$carte_long |> filter(zone == z, indicateur == "ecoles", statut == "Tous", sexe == "T", annee %in% c(input$annee, input$annee - 3))
      evol_ec <- if (nrow(ec) == 2) 100 * (ec$valeur[ec$annee == input$annee] / ec$valeur[ec$annee == input$annee - 3] - 1) else NA
      rt <- D$carte_long |> filter(zone == z, indicateur == "ctx_eleves_par_enseignant", sexe == "T", annee <= input$annee) |>
        slice_max(annee, n = 1, with_ties = FALSE)
      extra <- list(evol_ecoles = evol_ec, ratio = if (nrow(rt)) rt$valeur else NA,
                    ratio_annee = if (nrow(rt)) paste0(rt$annee - 1, "-", rt$annee) else "")
      recos <- recommandations(diag, extra)
      rang <- la |> mutate(r = min_rank(desc(srd))) |> filter(zone == z) |> pull(r)

      tagList(
        div(style = sprintf("border-left:6px solid %s; padding-left:10px;", COULEURS_ALERTE[[as.character(l$alerte)]]),
            span(class = "badge", style = sprintf("background:%s;", COULEURS_ALERTE[[as.character(l$alerte)]]), toupper(as.character(l$alerte))),
            span(class = paste("badge-type ms-1", if (l$type == "prédiction") "pred"), if (l$type == "prédiction") "prédiction" else "observé"),
            h4(class = "mt-1 mb-0", sprintf("SRD %s / 100", fmt_num(l$srd))),
            div(class = "small text-muted", sprintf("%de région la plus à risque sur %d", rang, nrow(la))),
            if (l$type == "prédiction" && "tap_bas" %in% names(l)) div(class = "small mt-1", sprintf(
              "Achèvement prédit : %s %% (fourchette %s – %s %%) · modèle : %s, erreur moyenne passée %s pts",
              fmt_num(l$tap), fmt_num(l$tap_bas), fmt_num(l$tap_haut), l$modele_tap, fmt_num(l$erreur_moyenne_tap))),
            if (l$type == "observé" && "tap" %in% names(l)) div(class = "small mt-1", sprintf("Achèvement : %s %%", fmt_num(l$tap)))),
        h6(class = "mt-3 mb-2", "Ce qui pèse le plus sur ce résultat"),
        lapply(seq_len(min(5, nrow(diag))), \(k) {
          x <- diag[k, ]
          coul <- if (x$risque >= 0.75) "#C62828" else if (x$risque >= 0.5) "#EF6C00" else "#2E7D32"
          div(class = "diag-ligne",
              div(class = "d-flex justify-content-between small",
                  span(strong(NOMS_COMPOSANTES[[x$variable]]), sprintf(" : %s%s", fmt_num(x$valeur, if (x$variable == "ecart_parite_tap") 2 else 1), UNITES[[x$variable]])),
                  span(class = "text-muted", sprintf("médiane des régions %s", fmt_num(x$mediane, if (x$variable == "ecart_parite_tap") 2 else 1)))),
              div(class = "barre", div(style = sprintf("width:%.0f%%;background:%s;", 100 * x$risque, coul))),
              div(class = "small text-muted", sprintf("plus risqué que %.0f %% des situations observées 2016-%d", 100 * x$risque, D$derniere_obs)))
        }),
        if (!is.na(evol_ec)) div(class = "small mb-1", sprintf("Écoles primaires : %s %% en 3 ans", fmt_num(evol_ec, 0))),
        if (!is.na(extra$ratio)) div(class = "small mb-1", sprintf("Élèves par enseignant : %s (%s, INSD)", fmt_num(extra$ratio, 0), extra$ratio_annee)),
        h6(class = "mt-3 mb-2", "Recommandations"),
        lapply(recos, \(r) div(class = "reco", r$txt)),
        actionButton(ns("vers_sim"), "Tester des mesures pour cette région dans le simulateur",
                     class = "btn-sm btn-primary mt-2", icon = icon("sliders")),
        plotOutput(ns("courbe"), height = 170)
      )
    })

    observeEvent(input$vers_sim, vers_simulateur(list(zone = zone_sel(), t = Sys.time())))

    output$courbe <- renderPlot({
      z <- zone_sel(); req(z)
      d <- D$srd |> filter(zone == z) |> select(annee, type, tap, tap_bas, tap_haut)
      jonction <- d |> filter(annee == D$derniere_obs) |> mutate(type = "prédiction")
      d2 <- bind_rows(d, jonction)
      ggplot(d2, aes(annee, tap)) +
        { if ("tap_bas" %in% names(d) && any(!is.na(d$tap_bas)))
          geom_ribbon(data = filter(d, type == "prédiction"), aes(ymin = tap_bas, ymax = tap_haut), fill = "#C62828", alpha = 0.12) } +
        geom_line(aes(linetype = type), colour = "#C62828", linewidth = 0.9) +
        geom_point(data = filter(d, type == "observé"), colour = "#C62828", size = 1.4) +
        geom_vline(xintercept = input$annee, linetype = "dotted", colour = "grey50") +
        scale_linetype_manual(values = c("observé" = "solid", "prédiction" = "22"), guide = "none") +
        labs(x = NULL, y = NULL, title = "Taux d'achèvement (%) : observé et prédit") +
        theme_minimal(base_size = 10) + theme(plot.title = element_text(size = 10, face = "bold"))
    }, res = 96)

    list(vers_simulateur = vers_simulateur)
  })
}
