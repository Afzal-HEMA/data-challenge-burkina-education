# ============================================================
# Onglet Accueil : vue d'ensemble du système (préscolaire et primaire)
# Données : table « tableau-accueil » (contrat dans docs/dictionnaire_variables.md)
# Chaque bloc s'affiche seulement si la donnée existe.
# ============================================================

RES <- 110   # résolution des graphiques (taille du texte)

PAL <- list(orange = "#F47A5B", vert = "#7FCFAE", bleu = "#86AEE6",
            gris = "#EEF0F4", texte = "#2D3436", discret = "#8A94A6")

fmt_nb  <- function(x) ifelse(is.na(x), "n.d.", format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE))
fmt_pct <- function(x, d = 0) ifelse(is.na(x), "n.d.", paste0(format(round(100 * x, d), decimal.mark = ","), " %"))

charger_accueil <- function() {
  if (Sys.getenv("DONNEES_FICTIVES") == "1") {
    return(read_csv(here("data", "fictif", "tableau_accueil_fictif.csv"), show_col_types = FALSE))
  }
  f <- dernier_fichier("03_transformation", "tableau-accueil")
  if (is.na(f)) return(NULL)
  read_csv(f, show_col_types = FALSE)
}

# ---- Composants visuels ------------------------------------
carte_kpi <- function(icone, couleur, libelle, valeur, libelle2 = NULL, valeur2 = NULL) {
  div(class = "acc-card acc-kpi",
      div(class = "acc-kpi-icone", style = sprintf("color:%s;", couleur), icon(icone)),
      div(div(class = "acc-label", libelle), div(class = "acc-valeur", valeur)),
      if (!is.null(libelle2)) tagList(
        div(class = "acc-sep"),
        div(div(class = "acc-label", libelle2), div(class = "acc-valeur", valeur2))
      ))
}

titre_bloc <- function(txt, aide = NULL) {
  div(class = "acc-titre", txt,
      if (!is.null(aide)) span(class = "acc-aide", title = aide, icon("circle-info")))
}

graphe_barres <- function(d, couleur, max_x = NULL) {
  if (is.null(d) || !nrow(d)) return(NULL)
  d <- d |> mutate(modalite = factor(modalite, levels = rev(unique(modalite))))
  m <- max_x %||% max(d$valeur, na.rm = TRUE) * 1.25
  ggplot(d, aes(y = modalite)) +
    geom_col(aes(x = m), fill = PAL$gris, width = 0.42) +
    geom_col(aes(x = valeur), fill = couleur, width = 0.42) +
    geom_text(aes(x = valeur, label = fmt_nb(valeur)), hjust = -0.12, size = 3.1, colour = PAL$discret) +
    scale_x_continuous(limits = c(0, m), expand = c(0, 0)) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank(), axis.text.x = element_blank(),
          axis.text.y = element_text(colour = PAL$texte, hjust = 0, size = 10),
          plot.background = element_rect(fill = "transparent", colour = NA),
          plot.margin = margin(2, 4, 2, 0))
}

graphe_anneau <- function(d, couleurs) {
  if (is.null(d) || !nrow(d)) return(NULL)
  d <- d |> mutate(part = valeur / sum(valeur), ymax = cumsum(part), ymin = lag(ymax, default = 0))
  ggplot(d) +
    geom_rect(aes(ymin = ymin, ymax = ymax, xmin = 3, xmax = 4.3, fill = modalite), colour = "white", linewidth = 1.2) +
    coord_polar(theta = "y") + xlim(c(1.6, 4.3)) +
    scale_fill_manual(values = couleurs) +
    theme_void() + theme(legend.position = "none",
                         plot.background = element_rect(fill = "transparent", colour = NA))
}

legende_anneau <- function(d, couleurs) {
  if (is.null(d) || !nrow(d)) return(NULL)
  d <- d |> mutate(part = valeur / sum(valeur))
  div(class = "acc-legende",
      lapply(seq_len(nrow(d)), \(i) div(class = "acc-legende-ligne",
        span(class = "acc-point", style = sprintf("background:%s;", couleurs[[d$modalite[i]]])),
        span(class = "acc-legende-lib", d$modalite[i]),
        span(class = "acc-legende-val", fmt_pct(d$part[i])))))
}

pas_de_donnee <- function() div(class = "acc-vide", "Donnée non disponible")

# ---- Interface ---------------------------------------------
accueil_ui <- function(id, zones, annees) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(CSS_ACCUEIL)),
    div(class = "acc-page",
      div(class = "acc-entete",
          h2("Tableau de bord"),
          span(class = "acc-sous-titre", "Préscolaire et primaire · Source : DGESS / Ministère de l'Éducation nationale")),
      div(class = "acc-card acc-filtres",
          selectInput(ns("zone"), NULL, choices = zones, selected = "National", width = "240px"),
          selectInput(ns("annee"), NULL, choices = setNames(annees, paste0("Année scolaire ", annees - 1, "-", annees)),
                      selected = max(annees), width = "240px")),
      uiOutput(ns("kpis")),
      div(class = "acc-grille",
        div(class = "acc-card acc-g-a1", uiOutput(ns("filles"))),
        div(class = "acc-card acc-g-a2", uiOutput(ns("evolution"))),
        div(class = "acc-card acc-g-b1", titre_bloc("Élèves par niveau", "Préscolaire et classes du primaire"),
            plotOutput(ns("g_niveau"), height = "200px")),
        div(class = "acc-card acc-g-b2", titre_bloc("Élèves par statut de l'école"),
            plotOutput(ns("g_statut_eleves"), height = "105px"),
            titre_bloc("Élèves par milieu"),
            plotOutput(ns("g_milieu_eleves"), height = "105px")),
        div(class = "acc-card acc-g-c1", titre_bloc("Écoles par milieu"),
            div(class = "acc-anneau", plotOutput(ns("g_milieu"), height = "150px", width = "150px"),
                uiOutput(ns("l_milieu")))),
        div(class = "acc-card acc-g-d1", titre_bloc("Écoles par statut"),
            div(class = "acc-anneau", plotOutput(ns("g_statut"), height = "150px", width = "150px"),
                uiOutput(ns("l_statut")))),
        div(class = "acc-card acc-g-c2", titre_bloc(textOutput(ns("titre_lieux"), inline = TRUE)),
            plotOutput(ns("g_lieux"), height = "290px"))
      )
    )
  )
}

# ---- Serveur -----------------------------------------------
accueil_server <- function(id, A) {
  moduleServer(id, function(input, output, session) {

    val <- function(an, dim, ind, mod = NULL) {
      d <- A |> filter(annee == an, zone == input$zone, dimension == dim, indicateur == ind)
      if (!is.null(mod)) d <- d |> filter(modalite == mod)
      d
    }
    un <- function(an, dim, ind, mod = "Total") {
      v <- val(an, dim, ind, mod)$valeur
      if (length(v)) v[1] else NA_real_
    }
    an <- reactive(as.integer(input$annee))

    output$kpis <- renderUI({
      e  <- un(an(), "ensemble", "ecoles");      el <- un(an(), "ensemble", "eleves")
      en <- un(an(), "ensemble", "enseignants"); s  <- un(an(), "ensemble", "salles_classe")
      div(class = "acc-ligne-kpi",
          carte_kpi("school", PAL$orange, "Écoles", fmt_nb(e)),
          carte_kpi("children", PAL$orange, "Élèves", fmt_nb(el)),
          carte_kpi("chalkboard-user", PAL$orange, "Enseignants", fmt_nb(en),
                    "Élèves par enseignant", fmt_nb(el / en)),
          carte_kpi("door-open", PAL$orange, "Salles de classe", fmt_nb(s),
                    "Élèves par classe", fmt_nb(el / s)))
    })

    output$filles <- renderUI({
      f <- un(an(), "sexe", "eleves", "Filles"); g <- un(an(), "sexe", "eleves", "Garçons")
      if (is.na(f)) return(tagList(titre_bloc("Filles scolarisées"), pas_de_donnee()))
      part <- f / (f + g)
      tagList(div(class = "acc-label", "Filles scolarisées"),
              div(class = "acc-valeur", fmt_nb(f)),
              div(class = "acc-ligne-barre", span("Part des filles"), strong(fmt_pct(part))),
              div(class = "acc-progress", div(style = sprintf("width:%.1f%%; background:%s;", 100 * part, PAL$bleu))),
              div(class = "acc-ligne-barre", span("Indice de parité F/G"),
                  strong(format(round(f / g, 2), nsmall = 2, decimal.mark = ","))))
    })

    output$evolution <- renderUI({
      el <- un(an(), "ensemble", "eleves"); prec <- un(an() - 1L, "ensemble", "eleves")
      ev <- if (!is.na(prec)) el / prec - 1 else NA
      fl <- if (is.na(ev)) NULL else if (ev < 0)
        span(class = "acc-baisse", icon("arrow-down"), fmt_pct(abs(ev), 1)) else
        span(class = "acc-hausse", icon("arrow-up"), fmt_pct(ev, 1))
      pre <- un(an(), "niveau", "eleves", "Préscolaire")
      tagList(div(class = "acc-label", "Élèves du primaire"),
              div(class = "acc-valeur", fmt_nb(el), fl),
              div(class = "acc-label-petit", sprintf("par rapport à %d-%d", an() - 2L, an() - 1L)),
              div(class = "acc-ligne-barre", span("Enfants au préscolaire"), strong(fmt_nb(pre))))
    })

    niv <- reactive(val(an(), "niveau", "eleves") |>
                      mutate(modalite = factor(modalite, levels = c("Préscolaire", "CP1", "CP2", "CE1", "CE2", "CM1", "CM2"))) |>
                      arrange(modalite) |> mutate(modalite = as.character(modalite)))
    output$g_niveau <- renderPlot(graphe_barres(niv(), PAL$orange), bg = "transparent", res = RES)
    output$g_statut_eleves <- renderPlot(graphe_barres(val(an(), "statut", "eleves"), PAL$bleu), bg = "transparent", res = RES)
    output$g_milieu_eleves <- renderPlot(graphe_barres(val(an(), "milieu", "eleves"), PAL$vert), bg = "transparent", res = RES)

    coul_milieu <- c(Urbain = PAL$orange, Rural = PAL$vert)
    coul_statut <- c(Public = PAL$bleu, Privé = PAL$orange, Communautaire = PAL$vert)
    output$g_milieu <- renderPlot(graphe_anneau(val(an(), "milieu", "ecoles"), coul_milieu), bg = "transparent", res = RES)
    output$l_milieu <- renderUI(legende_anneau(val(an(), "milieu", "ecoles"), coul_milieu) %||% pas_de_donnee())
    output$g_statut <- renderPlot(graphe_anneau(val(an(), "statut", "ecoles"), coul_statut), bg = "transparent", res = RES)
    output$l_statut <- renderUI(legende_anneau(val(an(), "statut", "ecoles"), coul_statut) %||% pas_de_donnee())

    output$titre_lieux <- renderText(if (input$zone == "National") "Élèves par région" else
                                       paste("Élèves par province ·", input$zone))
    output$g_lieux <- renderPlot({
      niveau_enfant <- if (input$zone == "National") "region" else "province"
      membres <- if (input$zone == "National") NULL else
        REF$ref$nom[REF$ref$niveau == "province" & REF$ref$region == input$zone]
      d <- A |> filter(annee == an(), niveau_zone == niveau_enfant, dimension == "ensemble",
                       indicateur == "eleves", is.null(membres) | zone %in% membres) |>
        arrange(desc(valeur)) |> transmute(modalite = zone, valeur)
      graphe_barres(d, PAL$vert)
    }, bg = "transparent", res = RES)
  })
}

# ---- Style (inspiré d'un tableau de bord à cartes blanches) ----
CSS_ACCUEIL <- "
.acc-page { background:#F5F6FA; padding:18px 22px; margin:-1rem; min-height:100vh; font-family: system-ui, -apple-system, 'Segoe UI', sans-serif; color:#2D3436; }
.acc-entete h2 { font-weight:700; font-size:1.6rem; margin:0; }
.acc-sous-titre { color:#8A94A6; font-size:.85rem; }
.acc-card { background:#fff; border-radius:12px; box-shadow:0 1px 3px rgba(20,30,60,.06); padding:14px 16px; }
.acc-filtres { display:flex; gap:12px; margin:14px 0; padding:10px 14px; }
.acc-filtres .form-group { margin:0; }
.acc-filtres select, .acc-filtres .selectize-input { border-radius:8px !important; border:1px solid #E3E6EE !important; box-shadow:none !important; }
.acc-ligne-kpi { display:grid; grid-template-columns:1fr 1fr 1.7fr 1.7fr; gap:14px; margin-bottom:14px; }
.acc-kpi { display:flex; align-items:center; gap:16px; min-height:96px; }
.acc-kpi-icone { font-size:2rem; width:44px; text-align:center; }
.acc-label { color:#4A5568; font-size:.9rem; }
.acc-label-petit { color:#8A94A6; font-size:.78rem; margin-bottom:10px; }
.acc-valeur { font-size:1.65rem; font-weight:700; letter-spacing:.5px; }
.acc-sep { width:1px; align-self:stretch; background:#EEF0F4; margin:0 8px; }
.acc-grille { display:grid; gap:14px; grid-template-columns:1fr 1.6fr 1fr 1fr; grid-template-areas: 'a1 b1 c1 d1' 'a2 b2 c2 c2'; }
.acc-g-a1{grid-area:a1;} .acc-g-a2{grid-area:a2;} .acc-g-b1{grid-area:b1;} .acc-g-b2{grid-area:b2;}
.acc-g-c1{grid-area:c1;} .acc-g-d1{grid-area:d1;} .acc-g-c2{grid-area:c2;}
.acc-titre { font-weight:600; font-size:.95rem; margin-bottom:6px; }
.acc-aide { color:#B8BFCC; margin-left:6px; font-size:.8rem; }
.acc-ligne-barre { display:flex; justify-content:space-between; color:#8A94A6; font-size:.85rem; margin-top:14px; }
.acc-ligne-barre strong { color:#2D3436; }
.acc-progress { height:10px; background:#EEF0F4; border-radius:6px; overflow:hidden; margin-top:6px; }
.acc-progress div { height:100%; border-radius:6px; }
.acc-baisse { color:#E05A47; font-size:.85rem; font-weight:600; margin-left:10px; }
.acc-hausse { color:#2E9E6A; font-size:.85rem; font-weight:600; margin-left:10px; }
.acc-anneau { display:flex; align-items:center; gap:8px; }
.acc-legende { flex:1; font-size:.85rem; }
.acc-legende-ligne { display:flex; align-items:center; gap:8px; margin:8px 0; }
.acc-point { width:10px; height:10px; border-radius:50%; display:inline-block; }
.acc-legende-lib { flex:1; color:#4A5568; }
.acc-legende-val { font-weight:600; }
.acc-vide { color:#B8BFCC; font-style:italic; padding:20px 0; text-align:center; }
@media (max-width: 1100px) {
  .acc-ligne-kpi { grid-template-columns:1fr 1fr; }
  .acc-grille { grid-template-columns:1fr 1fr; grid-template-areas: 'a1 a2' 'b1 b1' 'b2 b2' 'c1 d1' 'c2 c2'; }
}
"
