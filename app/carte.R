# ============================================================
# Onglet Carte (premier onglet) : alerte et prédictions par région
#  - grande carte plein écran sur fond OpenStreetMap, noms des régions
#  - filtres : niveau, indicateur, année (observé + 2 ans prédits), sexe, statut
#  - clic sur une région : ce qui pèse sur son résultat, recommandations
#  - flash info et notifications : régions qui passent au rouge / sortent du rouge
# ============================================================

CATALOGUE <- tribble(
  ~niveau,       ~indicateur,                 ~libelle,                                        ~sens, ~par_sexe, ~par_statut,
  "primaire",    "taux_decrochage",           "Taux de décrochage sur le cycle primaire (%)",  -1,   TRUE,      FALSE,
  "primaire",    "srd",                       "Niveau d'alerte décrochage",                     -1,   FALSE,     FALSE,
  "primaire",    "evol_decrochage_3ans",      "Évolution du décrochage sur 3 ans (points)",     -1,   FALSE,     FALSE,
  "primaire",    "decrochage_annuel_apparent","Décrochage annuel apparent (%, observé)",        -1,   TRUE,      FALSE,
  "primaire",    "tbs",                       "Facteur : taux brut de scolarisation (%)",       1,    TRUE,      FALSE,
  "primaire",    "tba",                       "Facteur : taux brut d'admission au CP1 (%)",     1,    TRUE,      FALSE,
  "primaire",    "reussite_cep",              "Facteur : réussite au CEP (%)",                  1,    TRUE,      FALSE,
  "primaire",    "evol_ecoles_3ans",          "Facteur : évolution des écoles sur 3 ans (%)",   1,    FALSE,     FALSE,
  "primaire",    "evol_eleves_3ans",          "Facteur : évolution des effectifs sur 3 ans (%)", 1,   FALSE,     FALSE,
  "primaire",    "ctx_eleves_par_enseignant", "Facteur : élèves par enseignant (INSD)",         -1,   FALSE,     FALSE,
  "primaire",    "ctx_eleves_par_classe",     "Facteur : élèves par classe (INSD)",             -1,   FALSE,     FALSE,
  "primaire",    "tap",                       "Taux d'achèvement du primaire (%)",              1,    TRUE,      FALSE,
  "primaire",    "eleves",                    "Élèves du primaire",                             0,    TRUE,      FALSE,
  "primaire",    "ecoles",                    "Écoles primaires",                               0,    FALSE,     TRUE,
  "prescolaire", "tbs_prescolaire",           "Taux brut de préscolarisation (%)",              1,    TRUE,      FALSE,
  "prescolaire", "eleves_prescolaire",        "Enfants au préscolaire",                         0,    TRUE,      FALSE,
  "prescolaire", "structures",                "Structures préscolaires",                        0,    FALSE,     TRUE,
  "prescolaire", "salles_classe",             "Salles du préscolaire",                          0,    FALSE,     TRUE
)

# Facteurs explicatifs du décrochage (effets estimés dans 04_analyse_leviers.R)
FACTEURS <- tribble(
  ~variable,                   ~nom,                                   ~unite, ~dec,
  "tbs",                       "Scolarisation (TBS)",                  " %",   0,
  "reussite_cep",              "Réussite au CEP",                      " %",   0,
  "ecart_parite_tap",          "Écart filles / garçons",               "",     2,
  "evol_ecoles_3ans",          "Évolution des écoles (3 ans)",         " %",   0,
  "evol_eleves_3ans",          "Évolution des effectifs (3 ans)",      " %",   0,
  "ctx_eleves_par_enseignant", "Élèves par enseignant",                "",     0
)

CSS_CARTE <- "
.zone-carte { position: relative; margin: -8px -4px 0 -4px; }
.carte-plein { height: calc(100vh - 66px); min-height: 480px; border-radius: 8px; overflow: hidden; border: 1px solid #D1D5DB; }
.leaflet-container { background: #E8ECEF !important; }
/* barre du haut : bouton des filtres + titre */
.barre-haut { position: absolute; top: 8px; left: 8px; z-index: 850; display: flex; gap: 8px; align-items: center;
  max-width: calc(100% - 230px); }
.fiche-ouverte .barre-haut { max-width: calc(100% - 440px); }
.bouton-filtres { border: none; background: #1F2937; color: #fff; border-radius: 8px; padding: 5px 11px; font-size: 13px;
  box-shadow: 0 1px 4px rgba(0,0,0,.25); white-space: nowrap; }
.titre-carte { background: rgba(255,255,255,.94); padding: 4px 10px; border-radius: 8px; box-shadow: 0 1px 4px rgba(0,0,0,.2);
  font-size: 14px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
/* légende compacte en bas à gauche */
.boite-legende { position: absolute; left: 8px; bottom: 40px; z-index: 845; background: rgba(255,255,255,.94);
  padding: 5px 8px; border-radius: 8px; box-shadow: 0 1px 4px rgba(0,0,0,.2); }
.legende { display: flex; gap: 2px; }
.legende .classe { display: flex; flex-direction: column; align-items: center; min-width: 40px; }
.legende .classe .pave { width: 100%; height: 10px; border-radius: 2px; }
.legende .classe .lib { font-size: 11px; font-weight: 600; color: #1F2937; white-space: nowrap; }
.legende-unite { font-size: 11px; color: #4B5563; margin-bottom: 2px; }
/* filtres repliables, à gauche */
.panneau-filtres { position: absolute; top: 46px; left: 8px; bottom: 112px; width: 250px; z-index: 840; overflow-y: auto;
  background: rgba(255,255,255,.97); border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,.2); padding: 10px 14px; font-size: 14px; }
.panneau-filtres .form-group, .panneau-filtres .shiny-input-container { margin-bottom: 8px; width: 100%; }
.filtres-replies .panneau-filtres { display: none; }
/* flash info : bandeau fin en bas de la carte */
.zone-carte .flash { position: absolute; left: 0; right: 0; bottom: 0; z-index: 860; margin: 0; border-radius: 0 0 8px 8px;
  padding: 3px 10px; font-size: 12.5px; background: rgba(31,41,55,.92); }
.zone-carte .flash-titre { font-size: 11.5px; padding: 1px 6px; }
.leaflet-bottom.leaflet-right { bottom: 30px; }
.fiche-flottante { position: absolute; top: 8px; right: 8px; bottom: 38px; width: 400px; z-index: 1100;
  background: #fff; border-radius: 10px; box-shadow: 0 4px 18px rgba(0,0,0,.22); display: flex; flex-direction: column; }
.fiche-flottante .entete { display: flex; justify-content: space-between; align-items: center;
  padding: 8px 14px; border-bottom: 1px solid #EEF0F4; font-size: 16px; }
.fiche-flottante .corps { overflow-y: auto; padding: 10px 14px; flex: 1; }
.fiche-flottante .fermer { font-size: 22px; line-height: 1; color: #6B7280; text-decoration: none; }
.aide-carte { position: absolute; top: 8px; right: 8px; z-index: 800; background: rgba(255,255,255,.94);
  padding: 4px 10px; border-radius: 8px; box-shadow: 0 1px 4px rgba(0,0,0,.2); font-size: 12.5px; color: #374151; max-width: 210px; }
@media (max-width: 1100px) { .fiche-flottante { width: 330px; } .fiche-ouverte .barre-haut { max-width: calc(100% - 370px); } }
.leaflet-tooltip.etiquette-region { font-size: 11.5px !important; line-height: 1.1; text-align: center; padding: 0; }
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

# ---- Diagnostic : contribution estimée de chaque facteur au décrochage -----
# contribution = effet estimé (points de décrochage par unité) x (valeur de la
# région - médiane des régions la même année). Positif = le facteur pousse le
# décrochage de la région AU-DESSUS de la médiane.
diagnostic_region <- function(ligne, ligne_annee, facteurs) {
  if (is.null(facteurs)) return(tibble())
  eff <- facteurs |> filter(effet_retenu != 0) |> group_by(variable) |> slice(1) |> ungroup() |>
    select(variable, effet = effet_retenu, p_valeur)
  FACTEURS |> inner_join(eff, by = "variable") |>
    mutate(valeur = map_dbl(variable, \(v) if (v %in% names(ligne)) ligne[[v]] else NA_real_),
           mediane = map_dbl(variable, \(v) if (v %in% names(ligne_annee)) median(ligne_annee[[v]], na.rm = TRUE) else NA_real_),
           contribution = effet * (valeur - mediane)) |>
    filter(!is.na(contribution)) |>
    arrange(desc(contribution))
}

recommandations <- function(l, diag, extra) {
  r <- list()
  ctr <- function(v) { x <- diag$contribution[diag$variable == v]; if (length(x)) x else 0 }
  dec <- l$taux_decrochage
  if (!is.na(dec) && dec >= 40)
    r[[length(r) + 1]] <- list(p = dec / 10, txt = sprintf(
      "%s %% des enfants entrés au CP1 décrochent avant la fin du primaire : programme de maintien (cantines, kits, suivi des absences et des abandons), en priorité dans les classes de CE2 à CM2.", fmt_num(dec, 0)))
  if (!is.na(l$evol_decrochage_3ans) && l$evol_decrochage_3ans > 2)
    r[[length(r) + 1]] <- list(p = 3 + l$evol_decrochage_3ans / 2, txt = sprintf(
      "Le décrochage a augmenté de %s points en 3 ans : identifier les communes touchées (insécurité, déplacements, fermetures) et y concentrer l'aide.", fmt_num(l$evol_decrochage_3ans, 1)))
  if (ctr("evol_ecoles_3ans") > 0.5)
    r[[length(r) + 1]] <- list(p = ctr("evol_ecoles_3ans"), txt = sprintf(
      "La baisse du nombre d'écoles pèse sur le décrochage (+%s pts estimés) : rouvrir ou remplacer les écoles fermées (écoles temporaires, éducation en situation d'urgence).", fmt_num(ctr("evol_ecoles_3ans"), 1)))
  if (ctr("tbs") > 0.5)
    r[[length(r) + 1]] <- list(p = ctr("tbs"), txt = sprintf(
      "La faible scolarisation pèse sur le décrochage (+%s pts estimés) : ouvrir des classes (salles + enseignants) et mener des campagnes d'inscription.", fmt_num(ctr("tbs"), 1)))
  if (ctr("ctx_eleves_par_enseignant") > 0.5)
    r[[length(r) + 1]] <- list(p = ctr("ctx_eleves_par_enseignant"), txt = sprintf(
      "Classes surchargées (%s élèves par enseignant, +%s pts estimés) : recruter ou redéployer des enseignants.", fmt_num(extra$ratio, 0), fmt_num(ctr("ctx_eleves_par_enseignant"), 1)))
  if (ctr("reussite_cep") > 0.5)
    r[[length(r) + 1]] <- list(p = ctr("reussite_cep"), txt = "Faible réussite au CEP : remédiation en CM1-CM2 pour éviter les sorties sans diplôme.")
  if (ctr("ecart_parite_tap") > 0.5)
    r[[length(r) + 1]] <- list(p = ctr("ecart_parite_tap"), txt = sprintf(
      "L'écart filles / garçons pèse sur le décrochage (+%s pts estimés) : bourses et kits pour les filles, sensibilisation des familles, latrines séparées.", fmt_num(ctr("ecart_parite_tap"), 1)))
  if (ctr("evol_eleves_3ans") > 0.5)
    r[[length(r) + 1]] <- list(p = ctr("evol_eleves_3ans"), txt = "Effectifs en baisse : accueillir les élèves déplacés et ramener à l'école ceux qui l'ont quittée.")
  if (!length(r)) return(list(list(p = 0, txt = "Décrochage contenu : maintenir l'effort actuel et surveiller l'évolution.")))
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
NOMS_MODELES_C <- c(naif = "naïf (aucun changement)", tendance = "tendance amortie",
                    lineaire = "régression linéaire", foret = "forêt aléatoire")

carte_ui <- function(id, annees, annee_defaut, derniere_obs) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(CSS_CARTE)),
    tags$script(HTML("
      // Cadre la carte sur le Burkina Faso dans la place laissée libre par les panneaux
      // (filtres à gauche, titre en haut, fiche de la région à droite).
      window.cadrerCarte = function(id) {
        var w = HTMLWidgets.find('#' + id); if (!w || !w.getMap) return;
        var m = w.getMap(); if (!m) return;
        var z = $('#' + id).closest('.zone-carte');
        var g = z.find('.panneau-filtres').is(':visible') ? z.find('.panneau-filtres').outerWidth() + 20 : 10;
        var h = (z.find('.barre-haut').outerHeight() || 0) + 16;
        var b = (z.find('.boite-legende').outerHeight() || 0) + 46;
        var ouverte = z.find('.fiche-flottante').is(':visible');
        z.toggleClass('fiche-ouverte', ouverte);
        var d = ouverte ? z.find('.fiche-flottante').outerWidth() + 20 : 10;
        m.invalidateSize();
        m.fitBounds([[9.40, -5.55], [15.10, 2.42]], {paddingTopLeft: [g, h], paddingBottomRight: [d, b], animate: false});
      };
      window.cadrerToutes = function() {
        setTimeout(function() { $('.zone-carte .leaflet').each(function() { window.cadrerCarte(this.id); }); }, 60);
      };
      Shiny.addCustomMessageHandler('cadrer_carte', function(msg) { window.cadrerToutes(); });
      $(window).on('resize', window.cadrerToutes);
      $(document).on('shown.bs.tab', window.cadrerToutes);
      // sur les écrans étroits, les filtres sont repliés au départ pour laisser la place à la carte
      $(function() { if (window.innerWidth < 1500) $('.zone-carte').addClass('filtres-replies'); });
      $(document).on('click', '.bouton-filtres', function() {
        $(this).closest('.zone-carte').toggleClass('filtres-replies'); window.cadrerToutes();
      });
    ")),
    div(class = "zone-carte",
      div(class = "carte-plein", leafletOutput(ns("carte"), height = "100%")),
      # en haut à gauche : bouton des filtres et titre ; en bas : légende et flash info
      div(class = "barre-haut",
          tags$button(class = "bouton-filtres", type = "button", title = "Afficher / masquer les filtres", "☰ Filtres"),
          div(class = "titre-carte", uiOutput(ns("titre"), inline = TRUE))),
      div(class = "boite-legende", uiOutput(ns("legende"))),
      uiOutput(ns("flash")),
      # filtres, à gauche (repliables)
      div(class = "panneau-filtres",
        radioButtons(ns("niveau"), "Niveau",
                     choices = c("Primaire" = "primaire", "Préscolaire" = "prescolaire",
                                 "Post-primaire" = "post", "Secondaire" = "secondaire"), selected = "primaire"),
        tags$div(class = "small text-muted mb-2", style = "margin-top:-8px;",
                 "Post-primaire et secondaire : absents des données DGESS fournies."),
        tags$script(HTML(sprintf(
          "$(function(){ $('input[name=\"%s\"][value=\"post\"], input[name=\"%s\"][value=\"secondaire\"]').prop('disabled', true).parent().css('opacity', 0.45); });",
          ns("niveau"), ns("niveau")))),
        # indicateur fixé (taux de décrochage pour le primaire) : liste masquée
        div(style = "display:none;", selectInput(ns("indic"), "Indicateur", choices = NULL)),
        sliderInput(ns("annee"), "Année", min = min(annees), max = max(annees),
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
      # aide ou fiche de la région, à droite
      conditionalPanel("!output.zone_ouverte", ns = ns,
        div(class = "aide-carte", "👆 Cliquez sur une région pour voir son diagnostic")),
      conditionalPanel("output.zone_ouverte", ns = ns,
        div(class = "fiche-flottante",
            div(class = "entete", uiOutput(ns("titre_fiche"), inline = TRUE),
                actionLink(ns("fermer"), HTML("&times;"), class = "fermer", title = "Fermer")),
            div(class = "corps", uiOutput(ns("fiche")))))
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
      # pas de fond de carte (aucune écriture, aucune clé d'API) ; zoom ajusté au pays
      # fond OpenStreetMap (gratuit, sans clé) : pays voisins, villes, routes
      m <- leaflet(options = leafletOptions(zoomSnap = 0.05, zoomDelta = 0.5, zoomControl = TRUE)) |>
        addProviderTiles(providers$OpenStreetMap.Mapnik) |>
        fitBounds(-5.55, 9.40, 2.42, 15.10, options = list(padding = c(10, 10)))
      # zoom en bas à droite ; recadrage une fois la page affichée (taille réelle du conteneur)
      if (inherits(m, "htmlwidget")) m <- htmlwidgets::onRender(m, sprintf(
        "function(el, x) { if (this.zoomControl) this.zoomControl.setPosition('bottomright');
           setTimeout(function() { window.cadrerCarte && window.cadrerCarte('%s'); }, 400); }", ns("carte")))
      m
    })
    output$zone_ouverte <- reactive(!is.null(zone_sel()))
    outputOptions(output, "zone_ouverte", suspendWhenHidden = FALSE)
    observeEvent(input$fermer, zone_sel(NULL))
    # recadre quand la fiche s'ouvre ou se ferme (elle occupe la droite)
    observeEvent(zone_sel(), ignoreNULL = FALSE, {
      session$onFlushed(function() session$sendCustomMessage("cadrer_carte", list()), once = TRUE)
    })

    # Couleurs par classes (plus lisibles qu'un dégradé) : bornes « rondes » couvrant toutes les années
    unite_de <- function(lib) if (grepl("\\(%", lib)) " %" else if (grepl("points", lib)) " pts" else ""
    classes <- reactive({
      i <- info()
      if (input$indic == "srd")
        return(list(fill = function(g) { f <- unname(COULEURS_ALERTE[as.character(g$alerte)]); f[is.na(f)] <- "#BDBDBD"; f },
                    legende = tibble(couleur = unname(COULEURS_ALERTE), lib = c("Vert", "Jaune", "Orange", "Rouge")),
                    unite = "rouge = risque de décrochage le plus fort"))
      dom <- D$carte_long |> filter(indicateur == input$indic) |> pull(valeur)
      dom <- dom[is.finite(dom)]
      req(length(dom) > 1)
      br <- pretty(dom, n = 6)
      pal_nom <- if (i$sens == 0) "Blues" else "RdYlGn"
      pal <- colorBin(pal_nom, domain = range(br), bins = br, reverse = i$sens < 0, na.color = "#BDBDBD")
      milieux <- head(br, -1) + diff(br) / 2
      u <- unite_de(i$libelle)
      list(fill = function(g) pal(g$valeur),
           legende = tibble(couleur = pal(milieux),
                            lib = sprintf("%s–%s", format(head(br, -1), big.mark = " ", trim = TRUE),
                                          format(br[-1], big.mark = " ", trim = TRUE))),
           unite = if (i$sens < 0) "rouge = plus de décrochage" else if (i$sens > 0) "rouge = situation défavorable" else "")
    })
    output$legende <- renderUI({
      cl <- classes()
      tagList(div(class = "legende-unite", strong(info()$libelle), if (nzchar(cl$unite)) paste0(" · ", cl$unite)),
              div(class = "legende",
                  lapply(seq_len(nrow(cl$legende)), \(k)
                    div(class = "classe", div(class = "pave", style = sprintf("background:%s;", cl$legende$couleur[k])),
                        div(class = "lib", cl$legende$lib[k])))))
    })

    observe({
      g <- donnees(); i <- info(); cl <- classes()
      req(nrow(g))
      fill <- cl$fill(g)
      etiquette <- sprintf("<b>%s</b><br>%s : %s<br>Alerte : %s", g$zone, i$libelle,
                           if (input$indic == "srd") fmt_num(g$srd) else fmt_num(g$valeur, if (i$sens == 0) 0 else 1),
                           ifelse(is.na(g$alerte), "n.d.", toupper(as.character(g$alerte)))) |> lapply(HTML)
      bord <- ifelse(!is.null(zone_sel()) & g$zone %in% zone_sel(), 3.5, 1.2)
      leafletProxy("carte", data = g) |>
        clearShapes() |> clearControls() |> clearMarkers() |>
        addPolygons(layerId = ~zone, fillColor = fill, fillOpacity = 1, color = ifelse(bord > 2, "#111827", "white"),
                    weight = bord, label = etiquette,
                    highlightOptions = highlightOptions(weight = 3, color = "#111827", bringToFront = TRUE)) |>
        # frontière du pays bien marquée
        addPolylines(data = D$pays, color = "#111827", weight = 3, opacity = 0.95, options = pathOptions(interactive = FALSE)) |>
        addLabelOnlyMarkers(data = D$centres, lng = ~lng, lat = ~lat, label = ~lapply(nom, HTML),
                            labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE,
                                                        className = "etiquette-region"))
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
    # (pas de fenêtres de notification sur la carte : les changements d'alerte sont dans le bandeau flash)

    output$resume <- renderUI({
      d <- D$srd |> filter(annee == input$annee)
      n <- table(factor(d$alerte, levels = names(COULEURS_ALERTE)))
      tagList(strong(sprintf("Alerte en %d", input$annee)),
              tags$ul(class = "list-unstyled small mb-0",
                      lapply(rev(names(n)), \(k) tags$li(span(style = sprintf("color:%s;font-size:1.1em;", COULEURS_ALERTE[k]), "■ "),
                                                          sprintf("%s : %d région(s)", tools::toTitleCase(k), n[[k]])))))
    })

    # ---- Fiche : ce qui ne va pas et recommandations ----
    output$titre_fiche <- renderUI(if (!is.null(zone_sel())) tagList(strong(zone_sel()), " · ", input$annee))

    output$fiche <- renderUI({
      z <- zone_sel()
      if (is.null(z)) return(div(class = "text-muted small",
        p("Cliquez sur une région de la carte pour voir :"),
        tags$ul(tags$li("son taux de décrochage, observé ou prédit ;"), tags$li("les facteurs qui expliquent ce niveau ;"),
                tags$li("les actions recommandées.")),
        if (!is.null(D$modele_retenu)) p(class = "mt-3", sprintf(
          "Prédictions du décrochage : modèle %s (1 an) et %s (2 ans), retenus parce qu'ils se sont le moins trompés sur les années passées.",
          NOMS_MODELES_C[[D$modele_retenu$modele[D$modele_retenu$h == 1]]], NOMS_MODELES_C[[D$modele_retenu$modele[D$modele_retenu$h == 2]]]))))
      l <- D$srd |> filter(zone == z, annee == input$annee)
      if (!nrow(l)) return(p("Pas de données pour cette année."))
      la <- D$srd |> filter(annee == input$annee)
      diag <- diagnostic_region(l, la, D$facteurs)
      rt <- D$carte_long |> filter(zone == z, indicateur == "ctx_eleves_par_enseignant", sexe == "T", annee <= input$annee) |>
        slice_max(annee, n = 1, with_ties = FALSE)
      extra <- list(ratio = if (nrow(rt)) rt$valeur else NA)
      recos <- recommandations(l, diag, extra)
      rang <- la |> mutate(r = min_rank(desc(taux_decrochage))) |> filter(zone == z) |> pull(r)
      med <- median(la$taux_decrochage, na.rm = TRUE)
      coul <- COULEURS_ALERTE[[as.character(l$alerte)]]

      # Bloc 1 : l'indicateur affiché sur la carte (s'il n'est pas déjà le décrochage)
      i <- info()
      bloc_indic <- NULL
      if (!input$indic %in% c("taux_decrochage", "srd")) {
        g <- donnees(); vz <- g$valeur[g$zone == z]
        u <- unite_de(i$libelle)
        dec <- if (i$sens == 0 && u == "") 0 else 1
        if (length(vz) && !is.na(vz)) {
          rg <- sum(g$valeur > vz, na.rm = TRUE) + 1
          coul_i <- classes()$fill(g[g$zone == z, ])
          bloc_indic <- div(style = sprintf("border-left:6px solid %s; padding-left:10px;", coul_i), class = "mb-3",
            div(class = "small text-muted", "Indicateur affiché sur la carte"),
            div(strong(sub("^Facteur : ", "", i$libelle))),
            h3(class = "mt-1 mb-0", paste0(fmt_num(vz, dec), u)),
            div(class = "small text-muted", sprintf("%s valeur la plus élevée sur %d · médiane des régions : %s%s",
                                                    if (rg == 1) "1re" else paste0(rg, "e"), sum(!is.na(g$valeur)),
                                                    fmt_num(median(g$valeur, na.rm = TRUE), dec), u)))
        } else {
          bloc_indic <- div(class = "small text-muted mb-3", sprintf("%s : pas de valeur pour cette région en %d.", i$libelle, input$annee))
        }
      }

      tagList(
        bloc_indic,
        if (!is.null(bloc_indic)) h6(class = "mb-1", "Risque de décrochage de la région"),
        div(style = sprintf("border-left:6px solid %s; padding-left:10px;", coul),
            span(class = "badge", style = sprintf("background:%s;", coul), paste("ALERTE", toupper(as.character(l$alerte)))),
            span(class = paste("badge-type ms-1", if (l$type == "prédiction") "pred"), if (l$type == "prédiction") "prédiction" else "observé"),
            h3(class = "mt-1 mb-0", sprintf("%s %%", fmt_num(l$taux_decrochage))),
            div(class = "small", "des enfants entrés au CP1 quittent l'école avant d'achever le primaire"),
            div(class = "small text-muted", sprintf("%s taux le plus élevé sur %d · médiane des régions : %s %% · %s %s pts en 3 ans",
                                                    if (rang == 1) "1er" else paste0(rang, "e"), nrow(la), fmt_num(med),
                                                    if (isTRUE(l$evol_decrochage_3ans >= 0)) "▲" else "▼", fmt_num(l$evol_decrochage_3ans, 1))),
            if (l$type == "prédiction" && "dec_bas" %in% names(l)) div(class = "small mt-1", sprintf(
              "Fourchette : %s – %s %% · modèle : %s (erreur moyenne passée : %s pts)",
              fmt_num(l$dec_bas), fmt_num(l$dec_haut), (NOMS_MODELES_C[l$modele_dec] %||% l$modele_dec), fmt_num(l$erreur_moyenne_dec)))),
        h6(class = "mt-3 mb-1", "Pourquoi ce niveau ? Facteurs associés"),
        if (!nrow(diag)) div(class = "small text-muted", "Effets des facteurs non disponibles (lancez 04_analyse_leviers.R).") else
        div(class = "small text-muted mb-2", "Effet estimé de chaque facteur par rapport à la médiane des régions (en points de décrochage)."),
        lapply(seq_len(nrow(diag)), \(k) {
          x <- diag[k, ]
          pousse <- x$contribution > 0
          larg <- min(100, 100 * abs(x$contribution) / max(abs(diag$contribution), 1e-9))
          div(class = "diag-ligne",
              div(class = "d-flex justify-content-between small",
                  span(strong(x$nom), sprintf(" : %s%s", fmt_num(x$valeur, if (x$dec == 2) 2 else 1), x$unite)),
                  span(style = sprintf("color:%s;font-weight:600;", if (pousse) "#C62828" else "#2E7D32"),
                       sprintf("%+.1f pts", x$contribution))),
              div(class = "barre", div(style = sprintf("width:%.0f%%;background:%s;", larg, if (pousse) "#C62828" else "#2E7D32"))),
              div(class = "small text-muted", sprintf("médiane des régions : %s%s", fmt_num(x$mediane, if (x$dec == 2) 2 else 1), x$unite)))
        }),
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
      if (!input$indic %in% c("taux_decrochage", "srd")) {
        i <- info()
        sx <- if (isTRUE(i$par_sexe)) input$sexe else "T"; st <- if (isTRUE(i$par_statut)) input$statut else "Tous"
        d <- D$carte_long |> filter(zone == z, indicateur == input$indic, sexe == sx, statut == st, !is.na(valeur))
        req(nrow(d))
        jonction <- d |> filter(annee == D$derniere_obs) |> mutate(type = "prédiction")
        return(ggplot(bind_rows(d, jonction), aes(annee, valeur)) +
          geom_line(aes(linetype = type), colour = "#1F4E79", linewidth = 0.9) +
          geom_point(data = filter(d, type == "observé"), colour = "#1F4E79", size = 1.4) +
          geom_vline(xintercept = input$annee, linetype = "dotted", colour = "grey50") +
          scale_linetype_manual(values = c("observé" = "solid", "prédiction" = "22"), guide = "none") +
        scale_x_continuous(breaks = function(l) seq(ceiling(l[1]), floor(l[2]), by = 2)) +
          labs(x = NULL, y = NULL, title = paste0(sub("^Facteur : ", "", i$libelle), " : observé et prédit")) +
          theme_minimal(base_size = 10) + theme(plot.title = element_text(size = 10, face = "bold")))
      }
      d <- D$srd |> filter(zone == z) |> select(annee, type, taux_decrochage, any_of(c("dec_bas", "dec_haut")))
      jonction <- d |> filter(annee == D$derniere_obs) |> mutate(type = "prédiction")
      d2 <- bind_rows(d, jonction)
      g <- ggplot(d2, aes(annee, taux_decrochage))
      if ("dec_bas" %in% names(d) && any(!is.na(d$dec_bas)))
        g <- g + geom_ribbon(data = filter(d, type == "prédiction"), aes(ymin = dec_bas, ymax = dec_haut), fill = "#C62828", alpha = 0.12)
      g + geom_line(aes(linetype = type), colour = "#C62828", linewidth = 0.9) +
        geom_point(data = filter(d, type == "observé"), colour = "#C62828", size = 1.4) +
        geom_vline(xintercept = input$annee, linetype = "dotted", colour = "grey50") +
        scale_linetype_manual(values = c("observé" = "solid", "prédiction" = "22"), guide = "none") +
        scale_x_continuous(breaks = function(l) seq(ceiling(l[1]), floor(l[2]), by = 2)) +
        labs(x = NULL, y = NULL, title = "Taux de décrochage (%) : observé et prédit") +
        theme_minimal(base_size = 10) + theme(plot.title = element_text(size = 10, face = "bold"))
    }, res = 96)

    list(vers_simulateur = vers_simulateur)
  })
}
