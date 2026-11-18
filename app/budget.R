# ============================================================
# Onglet Investissement : répartir un budget entre régions et actions
#
# Méthode (allocation par tranches, « au meilleur rendement ») :
#  1. Le budget est découpé en tranches égales (80 par défaut).
#  2. Pour chaque tranche, on teste toutes les combinaisons région x action
#     et on calcule la baisse du score de risque de décrochage qu'elle produirait,
#     pondérée par le nombre d'enfants concernés et par l'urgence
#     (alerte rouge > orange > jaune > vert).
#  3. La tranche va à la meilleure combinaison ; on recommence avec la
#     situation mise à jour. Les rendements décroissent (une région déjà
#     aidée gagne moins à chaque nouvelle tranche), d'où une répartition
#     entre régions et, dans chaque région, entre actions.
#
# Actions : ouvrir des classes (salle + enseignant), maintien à l'école
# (cantines, kits), remédiation CM1-CM2, actions pour les filles.
# Coûts unitaires et effets : HYPOTHÈSES indicatives, modifiables dans
# l'interface ; à remplacer par les coûts du ministère.
# ============================================================

ACTIONS <- c(classes = "Ouvrir des classes (salle + enseignant)", maintien = "Maintien à l'école (cantines, kits)",
             remediation = "Remédiation CM1-CM2", filles = "Actions pour les filles")
COULEURS_ACTIONS <- c(classes = "#86AEE6", maintien = "#F47A5B", remediation = "#7FCFAE", filles = "#B39DDB")
URGENCE <- c(vert = 1, jaune = 1.3, orange = 1.7, rouge = 2.2)

# État d'une région -> leviers du simulateur
# Rendements décroissants : on cible d'abord les élèves les plus à risque, donc
# les premiers % de couverture rapportent plus que les derniers (1 - (1 - c)^2).
rendement <- function(c) 1 - (1 - c)^2
leviers_depuis_etat <- function(b, etat, h) {
  list(ecoles = 0, enseignants = 0,
       eleves_direct = etat$classes * h$eleves_par_classe,
       maintien = 100 * h$effet_maintien * rendement(etat$maintien),
       cep = h$effet_cep * rendement(etat$remediation),
       parite = 100 * h$effet_filles * rendement(etat$filles))
}

cout_etat <- function(b, etat, h) {
  etat$classes * (h$cout_classe + h$salaire_enseignant) +
    etat$maintien * b$eleves * h$cout_maintien +
    etat$remediation * b$eleves * h$part_cm * h$cout_remediation +
    etat$filles * b$eleves * h$part_filles * h$cout_filles
}

# Coût d'une couverture complète (100 %) de chaque action « par élève » et coût d'une classe
cout_unitaire <- function(b, h) {
  c(classes = h$cout_classe + h$salaire_enseignant,
    maintien = b$eleves * h$cout_maintien,
    remediation = b$eleves * h$part_cm * h$cout_remediation,
    filles = b$eleves * h$part_filles * h$cout_filles)
}

repartir_budget <- function(bases, ref, poids, seuils, budget, h, param, n_tranches = 80, urgence = URGENCE) {
  tranche <- budget / n_tranches
  zones <- bases$zone
  etats <- setNames(lapply(zones, \(z) list(classes = 0, maintien = 0, remediation = 0, filles = 0)), zones)
  depense <- setNames(lapply(zones, \(z) setNames(rep(0, 4), names(ACTIONS))), zones)
  # poids « enfants » : taille d'une cohorte d'âge (population 6-11 ans / 6), normalisée
  cohorte <- with(bases, eleves / (tbs / 100) / 6)
  w_enf <- setNames(cohorte / mean(cohorte, na.rm = TRUE), zones)

  simuler_etat <- function(z, etat) {
    b <- bases[bases$zone == z, ]
    simuler(b, leviers_depuis_etat(b, etat, h), param)
  }
  actuel <- scorer_srd(bind_rows(lapply(zones, \(z) simuler_etat(z, etats[[z]]))), ref, poids, seuils)
  reste <- budget
  for (k in seq_len(n_tranches)) {
    cand <- list(); meta <- list()
    for (z in zones) {
      b <- bases[bases$zone == z, ]; cu <- cout_unitaire(b, h)
      for (a in names(ACTIONS)) {
        e <- etats[[z]]
        inc <- tranche / cu[[a]]
        if (a == "classes") {
          hors <- max(0, b$eleves / (b$tbs / 100) - b$eleves)
          if ((e$classes + inc) * h$eleves_par_classe > hors + h$eleves_par_classe) next   # plus d'enfants à accueillir
          e$classes <- e$classes + inc
        } else {
          if (e[[a]] >= 1) next                                                      # couverture déjà complète
          e[[a]] <- min(1, e[[a]] + inc)
        }
        cand[[length(cand) + 1]] <- simuler_etat(z, e)
        meta[[length(meta) + 1]] <- list(z = z, a = a, e = e)
      }
    }
    if (!length(cand)) break
    sc <- scorer_srd(bind_rows(cand), ref, poids, seuils)
    gains <- sapply(seq_along(meta), \(j) {
      z <- meta[[j]]$z
      # gain = points de décrochage évités x taille de la génération x urgence de la région
      (actuel$taux_decrochage[actuel$zone == z] - sc$taux_decrochage[j]) * w_enf[[z]] *
        urgence[[as.character(actuel$alerte[actuel$zone == z])]]
    })
    j <- which.max(gains)
    if (!length(j) || gains[j] <= 1e-9) break
    z <- meta[[j]]$z; a <- meta[[j]]$a
    etats[[z]] <- meta[[j]]$e
    depense[[z]][[a]] <- depense[[z]][[a]] + tranche
    actuel[actuel$zone == z, ] <- sc[j, names(actuel)]
    reste <- reste - tranche
  }

  avant <- scorer_srd(bases, ref, poids, seuils)
  detail <- map_dfr(zones, \(z) {
    b <- bases[bases$zone == z, ]; e <- etats[[z]]; ap <- actuel[actuel$zone == z, ]; av <- avant[avant$zone == z, ]
    tibble(zone = z, montant = sum(depense[[z]]),
           classes = depense[[z]][["classes"]], maintien = depense[[z]][["maintien"]],
           remediation = depense[[z]][["remediation"]], filles = depense[[z]][["filles"]],
           n_classes = e$classes, n_enseignants = e$classes,
           eleves_cantine = e$maintien * b$eleves, eleves_remediation = e$remediation * b$eleves * h$part_cm,
           filles_aidees = e$filles * b$eleves * h$part_filles,
           eleves_accueillis = ap$eleves_supplementaires,
           tap_avant = av$tap, tap_apres = ap$tap, srd_avant = av$srd, srd_apres = ap$srd,
           alerte_avant = av$alerte, alerte_apres = ap$alerte,
           dec_avant = av$taux_decrochage, dec_apres = ap$taux_decrochage,
           # enfants d'une génération qui ne décrochent plus (à terme)
           enfants_sans_decrochage = (b$eleves / (b$tbs / 100) / 6) * (av$taux_decrochage - ap$taux_decrochage) / 100)
  }) |> arrange(desc(montant))
  list(detail = detail, reste = reste)
}

# ---- Interface ---------------------------------------------------------------
budget_ui <- function(id, annees, annee_defaut) {
  ns <- NS(id)
  layout_sidebar(fillable = FALSE,
    sidebar = sidebar(width = 310,
      numericInput(ns("budget"), "Budget à investir (milliards de FCFA)", value = 10, min = 0.5, max = 500, step = 0.5),
      selectInput(ns("annee"), "Situation de départ", choices = setNames(annees, ifelse(annees > min(annees), paste(annees, "(prédiction)"), paste(annees, "(observé)"))),
                  selected = annee_defaut),
      radioButtons(ns("priorite"), "Priorité", choices = c("Urgence d'abord (rouge > orange > jaune)" = "urgence",
                                                            "Seulement le nombre d'enfants" = "neutre")),
      actionButton(ns("calculer"), "Calculer la répartition", class = "btn-primary w-100", icon = icon("calculator")),
      accordion(open = FALSE, class = "mt-3",
        accordion_panel("Coûts unitaires (hypothèses à remplacer)",
          numericInput(ns("cout_classe"), "Construction et équipement d'une classe (FCFA)", 15e6, 0, step = 1e6),
          numericInput(ns("salaire"), "Salaire annuel d'un enseignant, charges comprises (FCFA)", 2.4e6, 0, step = 1e5),
          numericInput(ns("eleves_par_classe"), "Élèves accueillis par classe ouverte", 50, 20, 90),
          numericInput(ns("cout_maintien"), "Cantine + kit par élève et par an (FCFA)", 30000, 0, step = 1000),
          numericInput(ns("cout_remediation"), "Remédiation par élève de CM1-CM2 (FCFA)", 15000, 0, step = 1000),
          numericInput(ns("cout_filles"), "Bourse / kit par fille et par an (FCFA)", 25000, 0, step = 1000)),
        accordion_panel("Effets attendus (hypothèses)",
          sliderInput(ns("effet_maintien"), "Baisse du décrochage si tous les élèves sont couverts", 0, 60, 25, post = " %"),
          sliderInput(ns("effet_cep"), "Gain de réussite au CEP si tous les CM1-CM2 sont couverts", 0, 25, 10, post = " pts"),
          sliderInput(ns("effet_filles"), "Réduction de l'écart filles/garçons si toutes les filles sont couvertes", 0, 100, 60, post = " %")))
    ),
    uiOutput(ns("kpis")),
    layout_columns(col_widths = c(7, 5),
      card(card_header("Répartition du budget par région et par action"), plotOutput(ns("barres"), height = 430)),
      card(card_header("Niveau d'alerte après investissement"), leafletOutput(ns("carte_apres"), height = 430))
    ),
    card(card_header("Détail par région : sur quoi investir"), DTOutput(ns("detail"))),
    p(class = "small text-muted",
      "Méthode : le budget est réparti par tranches ; chaque tranche va à la région et à l'action qui font le plus baisser",
      "le risque, en tenant compte du nombre d'enfants et de l'urgence. Les coûts et les effets sont des hypothèses",
      "indicatives : remplacez-les par les coûts du ministère. Les effets sont évalués « à terme », une fois les",
      "mesures pleinement en place.")
  )
}

# ---- Serveur -----------------------------------------------------------------
budget_server <- function(id, D, param_defaut) {
  moduleServer(id, function(input, output, session) {
    ref <- D$srd |> filter(type == "observé")
    part_cm <- 0.28            # part des élèves en CM1-CM2 (ordre de grandeur, tableau P04 national)

    resultat <- eventReactive(input$calculer, {
      an <- as.integer(input$annee)
      bases <- D$srd |> filter(annee == an) |> arrange(zone)
      h3 <- D$srd |> filter(annee == an - 3) |>
        select(zone, tap_3 = tap, eleves_3 = eleves, dec_3 = taux_decrochage, any_of(c(ecoles_3 = "ecoles")))
      bases <- bases |> left_join(h3, by = "zone")
      # mêmes compléments que le simulateur : écoles et enseignants de la dernière année connue
      ec <- ref |> filter(!is.na(ecoles)) |> slice_max(annee, n = 1, by = zone) |> select(zone, ecoles_ref = ecoles)
      bases <- bases |> left_join(ec, by = "zone") |> mutate(ecoles = coalesce(ecoles, ecoles_ref)) |> select(-ecoles_ref)
      bases$enseignants <- NA_real_
      if ("ctx_enseignants_primaire" %in% names(ref)) {
        ens <- ref |> filter(!is.na(ctx_enseignants_primaire)) |> slice_max(annee, n = 1, by = zone, with_ties = FALSE) |>
          select(zone, ens = ctx_enseignants_primaire)
        bases <- bases |> select(-enseignants) |> left_join(ens, by = "zone") |> rename(enseignants = ens)
      }
      pf <- if (!is.null(D$ind_obs)) D$ind_obs |> filter(sexe == "T", !is.na(part_filles)) |>
        slice_max(annee, n = 1, by = zone) |> select(zone, part_filles) else NULL
      h <- list(cout_classe = input$cout_classe, salaire_enseignant = input$salaire, eleves_par_classe = input$eleves_par_classe,
                cout_maintien = input$cout_maintien, cout_remediation = input$cout_remediation, cout_filles = input$cout_filles,
                effet_maintien = input$effet_maintien / 100, effet_cep = input$effet_cep, effet_filles = input$effet_filles / 100,
                part_cm = part_cm, part_filles = if (!is.null(pf)) median(pf$part_filles, na.rm = TRUE) / 100 else 0.48)
      urg <- if (input$priorite == "neutre") c(vert = 1, jaune = 1, orange = 1, rouge = 1) else URGENCE
      withProgress(message = "Répartition du budget en cours...", value = 0.3, {
        r <- repartir_budget(bases, ref, D$poids, D$seuils, input$budget * 1e9, h, param_defaut, urgence = urg)
        incProgress(0.7)
      })
      r$annee <- an
      r
    })

    # notifications : régions qui changent de couleur grâce à l'investissement
    observeEvent(resultat(), {
      d <- resultat()$detail |> filter(alerte_avant != alerte_apres)
      for (k in seq_len(min(nrow(d), 6))) {
        x <- d[k, ]
        mieux <- match(x$alerte_apres, names(COULEURS_ALERTE)) < match(x$alerte_avant, names(COULEURS_ALERTE))
        showNotification(div(style = sprintf("border-left:5px solid %s;padding-left:8px;", if (mieux) "#2E7D32" else "#C62828"),
                             strong(x$zone), sprintf(" : %s → %s", x$alerte_avant, x$alerte_apres)),
                         type = if (mieux) "message" else "error", duration = 8)
      }
    })

    output$kpis <- renderUI({
      if (input$calculer == 0) return(div(class = "alert alert-info",
        "Indiquez le budget puis cliquez sur « Calculer la répartition ». Le calcul prend quelques secondes."))
      r <- resultat(); d <- r$detail
      sortis <- sum(d$alerte_avant == "rouge" & d$alerte_apres != "rouge")
      ameliores <- sum(match(d$alerte_apres, names(COULEURS_ALERTE)) < match(d$alerte_avant, names(COULEURS_ALERTE)))
      boite <- function(titre, valeur, sous) div(class = "col", card(class = "mb-2", card_body(class = "p-2",
        div(class = "small text-muted", titre), h4(class = "mb-0", valeur), div(class = "small", sous))))
      div(class = "row g-2",
          boite("Budget réparti", sprintf("%s Md FCFA", fmt_num((input$budget * 1e9 - r$reste) / 1e9, 1)),
                if (r$reste > 1) sprintf("non utilisé : %s Md (plus de gain possible)", fmt_num(r$reste / 1e9, 1)) else "totalité utilisée"),
          boite("Enfants qui ne décrochent plus", fmt_ent(sum(d$enfants_sans_decrochage, na.rm = TRUE)), "par génération d'âge, à terme"),
          boite("Régions qui changent de couleur", ameliores, sprintf("dont %d sortie(s) du rouge", sortis)),
          boite("Classes ouvertes", fmt_ent(sum(d$n_classes)), sprintf("%s enfants accueillis en plus", fmt_ent(sum(d$eleves_accueillis, na.rm = TRUE)))))
    })

    output$barres <- renderPlot({
      req(input$calculer > 0)
      d <- resultat()$detail |> select(zone, all_of(names(ACTIONS))) |>
        pivot_longer(-zone, names_to = "action", values_to = "montant") |>
        mutate(zone = factor(zone, levels = rev(resultat()$detail$zone)), action = factor(action, levels = names(ACTIONS)))
      ggplot(d, aes(montant / 1e9, zone, fill = action)) +
        geom_col(width = 0.65) +
        scale_fill_manual(values = COULEURS_ACTIONS, labels = ACTIONS, name = NULL) +
        labs(x = "Milliards de FCFA", y = NULL) +
        theme_minimal(base_size = 12) + theme(legend.position = "bottom", legend.direction = "vertical",
                                              panel.grid.major.y = element_blank())
    }, res = 96)

    output$carte_apres <- renderLeaflet({
      req(input$calculer > 0)
      d <- resultat()$detail
      g <- D$carte |> left_join(d |> select(zone, alerte_apres, alerte_avant, montant), by = "zone")
      fill <- unname(COULEURS_ALERTE[g$alerte_apres]); fill[is.na(fill)] <- "#BDBDBD"
      lab <- sprintf("<b>%s</b><br>%s → %s<br>%s Md FCFA", g$zone, toupper(g$alerte_avant), toupper(g$alerte_apres),
                     fmt_num(g$montant / 1e9, 2)) |> lapply(HTML)
      leaflet(g) |> addProviderTiles(providers$CartoDB.PositronNoLabels) |>
        addPolygons(fillColor = fill, fillOpacity = 0.85, color = "white", weight = 1.2, label = lab) |>
        addLabelOnlyMarkers(data = D$centres, lng = ~lng, lat = ~lat, label = ~zone,
                            labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE,
                                                        className = "etiquette-region")) |>
        addLegend("bottomright", colors = unname(COULEURS_ALERTE), labels = c("Vert", "Jaune", "Orange", "Rouge"), opacity = 0.9)
    })

    output$detail <- renderDT({
      req(input$calculer > 0)
      d <- resultat()$detail |>
        transmute(`Région` = zone, `Montant (Md FCFA)` = round(montant / 1e9, 2),
                  `Part du budget` = sprintf("%.0f %%", 100 * montant / max(sum(montant), 1)),
                  `Classes à ouvrir` = round(n_classes), `Enseignants à recruter` = round(n_enseignants),
                  `Élèves en cantine` = round(eleves_cantine), `Élèves en remédiation` = round(eleves_remediation),
                  `Filles aidées` = round(filles_aidees),
                  `Décrochage avant → après (%)` = sprintf("%s → %s", fmt_num(dec_avant), fmt_num(dec_apres)),
                  `Alerte avant` = alerte_avant, `Alerte après` = alerte_apres)
      datatable(d, rownames = FALSE, options = list(dom = "t", pageLength = 20, scrollX = TRUE)) |>
        formatStyle(c("Alerte avant", "Alerte après"), backgroundColor = styleEqual(names(COULEURS_ALERTE), COULEURS_ALERTE),
                    color = "white", fontWeight = "bold") |>
        formatRound(c("Classes à ouvrir", "Enseignants à recruter", "Élèves en cantine", "Élèves en remédiation", "Filles aidées"),
                    digits = 0, mark = " ")
    })
  })
}
