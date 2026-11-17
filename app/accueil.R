# ============================================================
# Onglet Accueil : vue d'ensemble du système (préscolaire et primaire)
# Données : table « tableau-accueil » (contrat dans docs/dictionnaire_variables.md)
# Chaque bloc s'affiche seulement si la donnée existe.
# ============================================================

RES <- 90    # résolution des graphiques (taille du texte)

PAL <- list(orange = "#F47A5B", vert = "#7FCFAE", bleu = "#86AEE6",
            gris = "#EEF0F4", texte = "#2D3436", discret = "#8A94A6")

fmt_nb  <- function(x) ifelse(is.na(x), "n.d.", format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE))
fmt_pct <- function(x, d = 0) ifelse(is.na(x), "n.d.", paste0(format(round(100 * x, d), decimal.mark = ","), " %"))

fmt_taux <- function(x) ifelse(is.na(x), "n.d.", paste0(format(round(x, 1), nsmall = 1, decimal.mark = ","), " %"))

charger_accueil <- function() {
  f <- dernier_fichier("03_transformation", "tableau-accueil")
  if (is.na(f)) return(NULL)
  lire_csv(f)
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
    geom_col(aes(x = valeur), fill = rep_len(couleur, nrow(d)), width = 0.42) +
    geom_text(aes(x = valeur, label = fmt_nb(valeur)), hjust = -0.12, size = 2.6, colour = PAL$discret) +
    scale_x_continuous(limits = c(0, m), expand = c(0, 0)) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank(), axis.text.x = element_blank(),
          axis.text.y = element_text(colour = PAL$texte, hjust = 0, size = 8.5),
          plot.background = element_rect(fill = "transparent", colour = NA),
          plot.margin = margin(0, 2, 0, 0))
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
  graphe <- function(id_plot) div(class = "acc-plot", plotOutput(ns(id_plot), height = "100%"))
  tagList(
    tags$style(HTML(CSS_ACCUEIL)),
    div(class = "acc-page",
      # Titre et filtres sur une seule ligne
      div(class = "acc-entete",
          div(h2("Tableau de bord"),
              span(class = "acc-sous-titre", "Préscolaire et primaire · Source : DGESS / Ministère de l'Éducation nationale")),
          div(class = "acc-filtres",
              selectInput(ns("zone"), NULL, choices = zones, selected = "National", width = "210px"),
              selectInput(ns("annee"), NULL, choices = setNames(annees, paste0("Année scolaire ", annees - 1, "-", annees)),
                          selected = max(annees), width = "210px"))),
      uiOutput(ns("kpis")),
      # Grille 4 colonnes x 2 lignes qui occupe toute la hauteur restante
      div(class = "acc-grille",
        div(class = "acc-card acc-g-a1", uiOutput(ns("filles"))),
        div(class = "acc-card acc-g-a2", uiOutput(ns("evolution"))),
        div(class = "acc-card acc-g-b1", uiOutput(ns("t_niveau")), graphe("g_niveau")),
        div(class = "acc-card acc-g-b2", uiOutput(ns("t_statut_eleves")), graphe("g_statut_eleves"),
            titre_bloc("Élèves par sexe"), graphe("g_sexe_eleves")),
        div(class = "acc-card acc-g-c1", uiOutput(ns("t_milieu")),
            div(class = "acc-anneau", graphe("g_milieu"), uiOutput(ns("l_milieu")))),
        div(class = "acc-card acc-g-d1", titre_bloc("Écoles par statut"),
            div(class = "acc-anneau", graphe("g_statut"), uiOutput(ns("l_statut")))),
        div(class = "acc-card acc-g-c2", titre_bloc(textOutput(ns("titre_lieux"), inline = TRUE)),
            graphe("g_lieux"))
      )
    )
  )
}

# ---- Serveur -----------------------------------------------
accueil_server <- function(id, A) {
  moduleServer(id, function(input, output, session) {

    val <- function(an, dim, ind, mod = NULL, zone = input$zone) {
      d <- A |> filter(annee == an, .data$zone == .env$zone, dimension == dim, indicateur == ind)
      if (!is.null(mod)) d <- d |> filter(modalite == mod)
      d
    }
    # Certains tableaux DGESS n'existent qu'au niveau national :
    # on affiche alors le national, en le signalant dans le titre.
    val_ou_nat <- function(an, dim, ind) {
      d <- val(an, dim, ind)
      if (nrow(d) >= 2 || input$zone == "National") return(list(d = d, nat = FALSE))
      list(d = val(an, dim, ind, zone = "National"), nat = TRUE)
    }
    titre_nat <- function(txt, r) titre_bloc(if (isTRUE(r$nat)) paste(txt, "· national") else txt,
                                             if (isTRUE(r$nat)) "Ce tableau DGESS n'existe qu'au niveau national")
    un <- function(an, dim, ind, mod = "Total") {
      v <- val(an, dim, ind, mod)$valeur
      if (length(v)) v[1] else NA_real_
    }
    an <- reactive(as.integer(input$annee))

    output$kpis <- renderUI({
      e  <- un(an(), "ensemble", "ecoles");  el <- un(an(), "ensemble", "eleves")
      cp <- un(an(), "ensemble", "nouveaux_cp1")
      div(class = "acc-ligne-kpi",
          carte_kpi("school", PAL$orange, "Écoles primaires", fmt_nb(e)),
          carte_kpi("children", PAL$orange, "Élèves du primaire", fmt_nb(el)),
          carte_kpi("user-plus", PAL$orange, "Nouveaux inscrits CP1", fmt_nb(cp),
                    "Admission (TBA)", fmt_taux(un(an(), "ensemble", "tba"))),
          carte_kpi("graduation-cap", PAL$orange, "Scolarisation (TBS)", fmt_taux(un(an(), "ensemble", "tbs")),
                    "Achèvement (TAP)", fmt_taux(un(an(), "ensemble", "tap"))))
    })

    output$filles <- renderUI({
      f <- un(an(), "sexe", "eleves", "Filles"); g <- un(an(), "sexe", "eleves", "Garçons")
      if (is.na(f)) return(tagList(titre_bloc("Filles scolarisées"), pas_de_donnee()))
      part <- f / (f + g)
      tagList(div(class = "acc-label", "Filles scolarisées"),
              div(class = "acc-valeur", fmt_nb(f)),
              div(class = "acc-ligne-barre", span("Part des filles"), strong(fmt_pct(part))),
              div(class = "acc-progress", div(style = sprintf("width:%.1f%%; background:%s;", 100 * part, PAL$bleu))),
              div(class = "acc-ligne-barre", span("Indice de parité F/G (scolarisation)"),
                  strong({ ips <- un(an(), "ensemble", "ips_tbs")
                           format(round(if (is.na(ips)) f / g else ips, 2), nsmall = 2, decimal.mark = ",") })),
              div(class = "acc-ligne-barre", span("Réussite au CEP"),
                  strong(fmt_taux(un(an(), "ensemble", "reussite_cep")))))
    })

    output$evolution <- renderUI({
      el <- un(an(), "ensemble", "eleves"); prec <- un(an() - 1L, "ensemble", "eleves")
      ev <- if (!is.na(prec)) el / prec - 1 else NA
      fl <- if (is.na(ev)) NULL else if (ev < 0)
        span(class = "acc-baisse", icon("arrow-down"), fmt_pct(abs(ev), 1)) else
        span(class = "acc-hausse", icon("arrow-up"), fmt_pct(ev, 1))
      pre <- un(an(), "ensemble", "eleves_prescolaire")
      tagList(div(class = "acc-label", "Élèves du primaire"),
              div(class = "acc-valeur", fmt_nb(el), fl),
              div(class = "acc-label-petit", sprintf("par rapport à %d-%d", an() - 2L, an() - 1L)),
              div(class = "acc-ligne-barre", span("Enfants au préscolaire"), strong(fmt_nb(pre))),
              div(class = "acc-ligne-barre", span("Scolarisation au préscolaire"),
                  strong(fmt_taux(un(an(), "ensemble", "tbs_prescolaire")))))
    })

    niv <- reactive({
      r <- val_ou_nat(an(), "niveau", "eleves")
      r$d <- r$d |> mutate(modalite = factor(modalite, levels = c("Préscolaire", "CP1", "CP2", "CE1", "CE2", "CM1", "CM2"))) |>
        arrange(modalite) |> mutate(modalite = as.character(modalite))
      r
    })
    statut_el <- reactive(val_ou_nat(an(), "statut", "eleves"))
    milieu_el <- reactive(val_ou_nat(an(), "milieu", "eleves"))
    output$t_niveau <- renderUI(titre_nat("Élèves par niveau", niv()))
    output$t_statut_eleves <- renderUI(titre_nat("Élèves par statut de l'école", statut_el()))
    output$t_milieu <- renderUI(titre_nat("Élèves par milieu", milieu_el()))
    output$g_niveau <- renderPlot(graphe_barres(niv()$d, PAL$orange), bg = "transparent", res = RES)
    output$g_statut_eleves <- renderPlot(graphe_barres(statut_el()$d |> arrange(desc(modalite)), PAL$bleu), bg = "transparent", res = RES)
    output$g_sexe_eleves <- renderPlot(graphe_barres(val(an(), "sexe", "eleves") |> arrange(modalite), PAL$vert), bg = "transparent", res = RES)

    coul_milieu <- c(Urbain = PAL$orange, Rural = PAL$vert)
    coul_statut <- c(Public = PAL$bleu, Privé = PAL$orange, Communautaire = PAL$vert)
    output$g_milieu <- renderPlot(graphe_anneau(milieu_el()$d, coul_milieu), bg = "transparent", res = RES)
    output$l_milieu <- renderUI(legende_anneau(milieu_el()$d, coul_milieu) %||% pas_de_donnee())
    output$g_statut <- renderPlot(graphe_anneau(val(an(), "statut", "ecoles"), coul_statut), bg = "transparent", res = RES)
    output$l_statut <- renderUI(legende_anneau(val(an(), "statut", "ecoles"), coul_statut) %||% pas_de_donnee())

    output$titre_lieux <- renderText("Élèves du primaire par région")
    output$g_lieux <- renderPlot({
      d <- A |> filter(annee == an(), niveau_zone == "region", dimension == "ensemble",
                       indicateur == "eleves") |>
        arrange(desc(valeur)) |> transmute(modalite = zone, valeur)
      # la région choisie est mise en évidence
      graphe_barres(d, if (input$zone == "National") PAL$vert else
                      if_else(d$modalite == input$zone, PAL$orange, PAL$vert))
    }, bg = "transparent", res = RES)
  })
}

# ---- Style (inspiré d'un tableau de bord à cartes blanches) ----
CSS_ACCUEIL <- "
/* La page occupe exactement la hauteur de l'écran sous la barre de navigation */
.acc-page { background:#F5F6FA; margin:-1rem; padding:8px 14px 10px; height:calc(100vh - 52px); min-height:520px;
  display:flex; flex-direction:column; gap:8px; overflow:auto;
  font-family: system-ui, 'Segoe UI', sans-serif; color:#2D3436; font-size:12px; }
.acc-entete { display:flex; align-items:center; justify-content:space-between; gap:12px; flex:none; }
.acc-entete h2 { font-weight:700; font-size:17px; margin:0; line-height:1.2; }
.acc-sous-titre { color:#8A94A6; font-size:11px; }
.acc-filtres { display:flex; gap:8px; }
.acc-filtres .form-group { margin:0; }
.acc-filtres .selectize-input, .acc-filtres select { font-size:12px !important; min-height:28px !important; padding:4px 10px !important;
  border-radius:8px !important; border:1px solid #E3E6EE !important; box-shadow:none !important; background:#fff !important; }
.acc-filtres .selectize-dropdown { font-size:12px; }
.acc-card { background:#fff; border-radius:10px; box-shadow:0 1px 3px rgba(20,30,60,.06); padding:8px 12px;
  min-width:0; min-height:0; display:flex; flex-direction:column; overflow:hidden; }
.acc-ligne-kpi { display:grid; grid-template-columns:1fr 1fr 1.7fr 1.7fr; gap:8px; flex:none; }
.acc-kpi { flex-direction:row; align-items:center; gap:10px; min-height:58px; padding:6px 12px; }
.acc-kpi-icone { font-size:20px; width:26px; text-align:center; flex:none; }
.acc-label { color:#4A5568; font-size:11px; white-space:nowrap; overflow:hidden; text-overflow:ellipsis; }
.acc-label-petit { color:#8A94A6; font-size:10px; margin-bottom:4px; }
.acc-valeur { font-size:19px; font-weight:700; letter-spacing:.2px; white-space:nowrap; line-height:1.2; }
.acc-sep { width:1px; align-self:stretch; background:#EEF0F4; margin:0 4px; flex:none; }
.acc-grille { flex:1 1 auto; min-height:340px; display:grid; gap:8px;
  grid-template-columns:1fr 1.6fr 1fr 1fr; grid-template-rows:1fr 1fr;
  grid-template-areas: 'a1 b1 c1 d1' 'a2 b2 c2 c2'; }
.acc-g-a1{grid-area:a1;} .acc-g-a2{grid-area:a2;} .acc-g-b1{grid-area:b1;} .acc-g-b2{grid-area:b2;}
.acc-g-c1{grid-area:c1;} .acc-g-d1{grid-area:d1;} .acc-g-c2{grid-area:c2;}
/* Les graphiques prennent toute la place libre de leur carte */
.acc-plot { flex:1 1 0; min-height:40px; position:relative; }
.acc-plot > .shiny-plot-output { position:absolute; inset:0; height:100% !important; width:100% !important; }
.acc-titre { font-weight:600; font-size:12px; margin-bottom:2px; flex:none; }
.acc-aide { color:#B8BFCC; margin-left:4px; font-size:10px; }
.acc-ligne-barre { display:flex; justify-content:space-between; gap:8px; color:#8A94A6; font-size:11px; margin-top:6px; }
.acc-ligne-barre strong { color:#2D3436; white-space:nowrap; }
.acc-progress { height:7px; background:#EEF0F4; border-radius:6px; overflow:hidden; margin-top:4px; flex:none; }
.acc-progress div { height:100%; border-radius:6px; }
.acc-baisse { color:#E05A47; font-size:11px; font-weight:600; margin-left:6px; }
.acc-hausse { color:#2E9E6A; font-size:11px; font-weight:600; margin-left:6px; }
.acc-anneau { flex:1 1 0; min-height:0; display:flex; align-items:center; gap:4px; }
.acc-anneau > .acc-plot { flex:0 0 50%; align-self:stretch; }
.acc-legende { flex:1; font-size:11px; min-width:0; }
.acc-legende-ligne { display:flex; align-items:center; gap:5px; margin:5px 0; }
.acc-point { width:8px; height:8px; border-radius:50%; display:inline-block; flex:none; }
.acc-legende-lib { flex:1; color:#4A5568; white-space:nowrap; }
.acc-legende-val { font-weight:600; white-space:nowrap; }
.acc-vide { color:#B8BFCC; font-style:italic; padding:12px 0; text-align:center; }
@media (max-width: 760px) {
  .acc-page { height:auto; }
  .acc-ligne-kpi { grid-template-columns:1fr 1fr; }
  .acc-grille { grid-template-columns:1fr 1fr; grid-template-rows:none; grid-auto-rows:220px;
    grid-template-areas: 'a1 a2' 'b1 b1' 'b2 b2' 'c1 d1' 'c2 c2'; }
}
"
