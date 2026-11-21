# app.R — Bayes et la pièce de monnaie (mise à jour bêta-binomiale)
# Révision novembre 2026 :
#  - Graine (0 = au hasard) : expérience reproductible, graine affichée
#  - A priori « Personnalisé » (α, β), avec sa moyenne et son poids en lancers fictifs
#  - Virgule décimale partout (axes, curseurs, graphiques interactifs, notation scientifique)
#  - Comparaison avec l'intervalle de confiance de Clopper-Pearson
#  - Probabilité prédictive du prochain lancer (règle de succession de Laplace)
#  - Probabilité réelle dans ou hors de l'intervalle crédible (pièce connue ou révélée)
#  - Mission 6 : un a priori très confiant
library(shiny)
library(plotly)

# Une pièce, des données et une mise à jour bêta-binomiale.
priors <- list(
  "Uniforme" = c(1, 1),
  "Plutôt équilibrée" = c(8, 8),
  "Face favorisée" = c(8, 3),
  "Pile favorisée" = c(3, 8)
)
fmt <- function(x, digits = 3) formatC(x, format = "f", digits = digits, decimal.mark = ",")
# Étiquettes d'axes avec virgule décimale (graphiques de base)
virgule <- function(v) sub("\\.", ",", format(v, trim = TRUE, drop0trailing = TRUE))
axes_fr <- function() {
  a <- axTicks(1); axis(1, at = a, labels = virgule(a))
  b <- axTicks(2); axis(2, at = b, labels = virgule(b), las = 1)
}
GRAINE_MAX <- 99999L
# Notation scientifique à la française : 9,313 × 10⁻¹⁰
exposant <- function(e) {
  sup <- c("0"="⁰","1"="¹","2"="²","3"="³","4"="⁴","5"="⁵","6"="⁶","7"="⁷","8"="⁸","9"="⁹","-"="⁻")
  paste(sup[strsplit(as.character(e), "")[[1]]], collapse = "")
}
sci_fr <- function(x, digits = 3) {
  if (!is.finite(x) || x == 0) return(fmt(x, digits))
  e <- floor(log10(abs(x))); m <- x / 10^e
  if (e == 0) return(fmt(x, digits))
  paste0(fmt(m, digits), " × 10", exposant(e))
}
# Paramètres bêta : entiers sans décimales, sinon une décimale
fmt_par <- function(v) ifelse(abs(v - round(v)) < 1e-9, format(round(v), trim = TRUE), fmt(v, 1))

ui <- fluidPage(
  tags$head(tags$style(HTML('
    body {font-size: 16px; color: #243b53;}
    .container-fluid {max-width: 1550px;}
    .navbar-default {background: #f5f9fc;}
    .well {background: #f4f8fc; border: 1px solid #c9dce9; border-radius: 8px;}
    .resume {display:flex; flex-wrap:wrap; gap:12px; margin:12px 0 18px;}
    .resume div {flex:1 1 145px; background:#f3f8fc; border:1px solid #c9dce9; border-radius:6px; padding:10px 14px;}
    .resume strong {display:block; font-size:1.2em; color:#164e70;}
    .explication {background:#f3f8fc; border-left:4px solid #4184a8; padding:12px 16px; margin:14px 0;}
    .formule {font-size: 1.08em; padding: 8px;}
    .btn {font-size: 15px;}
    .calcul-etape {background:#fff; border:1px solid #c9dce9; border-radius:8px; padding:14px 18px; margin:14px 0;}
    .calcul-etape h4 {color:#164e70; margin-top:0;}
    .calcul-resultat {color:#a12626; font-size:1.2em; font-weight:700;}
    .calcul-ligne {font-size:1.08em; margin:8px 0;}
    .calcul-note {color:#465d6d; font-size:.95em;}
  '))),
  withMathJax(),
  # Curseurs : virgule décimale dans les étiquettes (ionRangeSlider)
  tags$script(HTML("
    $(document).on('shiny:sessioninitialized', function() {
      setTimeout(function() {
        $('.js-range-slider').each(function() {
          var s = $(this).data('ionRangeSlider');
          if (s) s.update({prettify_enabled: true, prettify: function(n) { return String(n).replace('.', ','); }});
        });
      }, 300);
    });
  ")),
  titlePanel("Bayes et la pièce de monnaie"),
  sidebarLayout(
    sidebarPanel(width = 3,
      h4("La pièce"),
      radioButtons("mode", "Choisir la pièce", c("Probabilité connue" = "connue", "Pièce mystère" = "mystere"), selected = "connue"),
      conditionalPanel("input.mode == 'mystere'", p("À chaque nouvelle expérience, la probabilité de Face est choisie au hasard entre 0,05 et 0,95 et reste fixe pendant les lancers.", style = "font-size: 0.9em;")),
      conditionalPanel("input.mode == 'connue'", sliderInput("theta", "Probabilité réelle de Face", 0, 1, 0.7, 0.01)),
      actionButton("nouvelle", "Nouvelle expérience", class = "btn-default", width = "100%"),
      p("Une nouvelle expérience efface les lancers précédents.", style = "font-size: 0.9em; margin-top: 6px;"),
      numericInput("graine", "Graine (0 = au hasard)", value = 0, min = 0, max = GRAINE_MAX, step = 1),
      uiOutput("info_graine"),
      hr(),
      selectInput("prior", "Distribution a priori", choices = c(names(priors), "Personnalisé"), selected = "Uniforme"),
      conditionalPanel("input.prior == 'Personnalisé'",
        fluidRow(
          column(6, numericInput("alpha", "α", value = 2, min = 0.5, max = 500, step = 0.5)),
          column(6, numericInput("beta", "β", value = 2, min = 0.5, max = 500, step = 0.5))
        ),
        uiOutput("info_prior")),
      p("Vous pouvez changer l’a priori après les lancers pour comparer les résultats avec les mêmes données.", style = "font-size: 0.9em;"),
      sliderInput("lot", "Nombre de lancers à ajouter", min = 1, max = 100, value = 10, step = 1),
      actionButton("lancer", "Lancer la pièce", class = "btn-primary", width = "100%"),
      br(), br(),
      uiOutput("bouton_reveler")
    ),
    mainPanel(width = 9,
      tabsetPanel(
        tabPanel("Introduction",
          h3("Que peut-on apprendre en lançant une pièce ?"),
          p("Une pièce produit Face avec une probabilité inconnue θ. Avant de voir les résultats, nous décrivons les valeurs plausibles de θ par une distribution a priori."),
          p("Après chaque série de lancers, nous combinons cette distribution avec les résultats observés. La distribution a posteriori représente alors ce que nous savons sur θ, compte tenu de l’a priori et des données."),
          div(class="explication", strong("Pour commencer : "), "ouvrez l’onglet Explorer, ajoutez quelques lancers, puis comparez l’a priori et l’a posteriori. Essayez ensuite un autre a priori sans changer les données."),
          h4("Une expérience à mener"),
          tags$ol(tags$li("Choisissez la pièce mystère et l’a priori uniforme."),
            tags$li("Ajoutez 10 lancers. Quelles valeurs de θ sont plausibles ?"),
            tags$li("Changez l’a priori : la conclusion change-t-elle ?"),
            tags$li("Ajoutez plusieurs séries de lancers, puis révélez la probabilité réelle.")),
          p("Le modèle suppose que les lancers sont indépendants et que la probabilité de Face reste constante pendant l’expérience."),
          div(class="explication", strong("Comment fonctionne la pièce mystère ? "), "Pour chaque nouvelle expérience, l’application choisit au hasard une probabilité de Face entre 0,05 et 0,95 et la conserve pendant tous les lancers. Les a priori proposés servent à analyser les résultats et à comparer différents points de départ ; ils ne correspondent pas nécessairement à la règle utilisée pour choisir la pièce.")
        ),
        tabPanel("Explorer",
          uiOutput("etat_piece"),
          uiOutput("resume"),
          radioButtons("zone", "Zone à mettre en évidence :", choices=c("Intervalle crédible à 95 %"="intervalle", "Probabilité au-delà d’un seuil"="seuil"), selected="intervalle", inline=TRUE),
          conditionalPanel("input.zone == 'seuil'", sliderInput("seuil", "Seuil de probabilité de Face", min=0, max=1, value=0.5, step=0.01, width="350px"), uiOutput("probabilite_superieure")),
          fluidRow(
            column(6, plotOutput("graphique", height = "380px")),
            column(6, plotOutput("evolution", height = "380px"))
          ),
          uiOutput("interpretation"),
          p("Graphique d’évolution : chaque point correspond à une série ajoutée. Les courbes évoluent avec les observations et l’a priori choisi.")
        ),
        tabPanel("Comparer les a priori",
          h3("Les mêmes données, quatre points de départ"),
          p("Les quatre distributions sont mises à jour avec les mêmes lancers de l’expérience. Ajoutez des lancers dans la barre latérale pour observer comment les résultats évoluent."),
          div(class="explication", strong("Utiliser les graphiques interactifs : "), "survolez une courbe ou un point pour afficher ses valeurs ; utilisez la barre d’outils pour zoomer ou enregistrer le graphique. Cliquez sur un nom dans la légende pour masquer ou réafficher une courbe ; double-cliquez pour l’isoler, puis double-cliquez de nouveau pour rétablir les autres."),
          fluidRow(
            column(6, plotlyOutput("comparaison", height = "480px")),
            column(6, h4("Résultats pour les mêmes lancers"),
              div(class="table-responsive", tableOutput("table_comparaison")),
              uiOutput("ecart_comparaison"),
              p("Les intervalles crédibles à 95 % sont à queues égales et dépendent de l’a priori de chaque ligne."))
          ),
          h4("Évolution des moyennes a posteriori"),
          fluidRow(
            column(6, plotlyOutput("comparaison_evolution", height = "430px")),
            column(6, div(class="explication", strong("Comment lire ce graphique ? "),
              "Au départ, les points représentent les moyennes a priori. Chaque point suivant correspond à une série de lancers ajoutée. Les quatre courbes utilisent les mêmes observations ; la ligne grise donne la proportion observée, vers laquelle elles convergent. Survolez un point pour voir ses valeurs ; cliquez sur un nom dans la légende pour masquer ou afficher sa courbe. Avec davantage de données, l’influence relative des a priori diminue généralement ; leur rapprochement dépend des observations."))
          )
        ),
        tabPanel("Comprendre le calcul",
          h3("Suivre le calcul avec vos résultats"),
          p("Les résultats ci-dessous utilisent l’a priori choisi dans la barre latérale et les lancers de l’expérience en cours. Ils se mettent à jour automatiquement."),
          uiOutput("formule_posterieure"),
          uiOutput("ponderation"),
          uiOutput("frequentiste"),
          h4("Examiner une valeur candidate"),
          p("Déplacez le curseur pour examiner comment une valeur particulière de θ est traitée avant et après les observations."),
          sliderInput("candidate", "Valeur candidate de θ", 0.01, 0.99, 0.5, 0.01, width = "350px"),
          uiOutput("calcul"),
          h4("La règle générale"),
          div(class="formule", "\\[p(\\theta\\mid\\text{données})=\\frac{p(\\theta)\\,p(\\text{données}\\mid\\theta)}{p(\\text{données})}\\]"),
          uiOutput("bayes_courant"),
          p("La vraisemblance décrit la compatibilité des données avec une valeur de θ. Le dénominateur normalise le produit afin que l’aire sous la courbe a posteriori soit égale à 1."),
          p("Les hauteurs des courbes sont des densités, et non des probabilités pour une valeur exacte : une probabilité correspond à une aire sur un intervalle de valeurs de θ.")
        ),
        tabPanel("Expérimenter",
          h3("Missions pour comprendre la mise à jour"),
          p("Pour chaque mission, formulez une prédiction avant de manipuler la pièce. Comparez ensuite votre prédiction aux résultats et expliquez ce que vous observez. Les lancers sont aléatoires : deux expériences peuvent donner des résultats différents."),
          div(class="calcul-etape", h4("Mission 1 — L’influence de l’a priori"),
            p(strong("Prédire : "), "Avec seulement 10 lancers, les quatre a priori conduiront-ils à des conclusions semblables ? Et après plusieurs centaines de lancers ?"),
            p(strong("Expérimenter : "), "Commencez une nouvelle expérience, ajoutez 10 lancers et ouvrez Comparer les a priori. Notez l’écart entre les moyennes. Ajoutez ensuite plusieurs séries et observez cet écart et les courbes."),
            p(strong("Expliquer : "), "Que devient le poids relatif de l’a priori ? Les distributions et leurs intervalles sont-ils exactement identiques ?"),
            tags$details(tags$summary("Voir une piste de réponse"),
              p("Avec peu de lancers, les a priori peuvent produire des moyennes et des intervalles sensiblement différents. Lorsque le nombre de lancers augmente, le poids relatif de chaque a priori diminue et les conclusions tendent à se rapprocher. Elles ne deviennent pas nécessairement exactement identiques ; examinez aussi la forme des courbes et leurs intervalles."))),
          div(class="calcul-etape", h4("Mission 2 — Quand les données surprennent"),
            p(strong("Prédire : "), "Si l’on favorise Face au départ, comment les conclusions changeront-elles si l’on observe surtout Pile ?"),
            p(strong("Expérimenter : "), "Choisissez une probabilité réelle de Face de 0,30, commencez une nouvelle expérience et sélectionnez l’a priori Face favorisée. Ajoutez 10 lancers, puis d’autres séries. Dans Explorer et Comprendre le calcul, comparez l’a priori, la proportion observée et l’a posteriori."),
            p(strong("Expliquer : "), "Dans quelle direction la distribution se déplace-t-elle ? Que se passe-t-il si les premiers lancers ne produisent pas surtout Pile ? Poursuivez l’expérience ou recommencez : une proportion de 0,30 n’assure pas un résultat particulier dans un petit échantillon."),
            tags$details(tags$summary("Voir une piste de réponse"),
              p("Si la proportion observée de Face est inférieure à la moyenne a priori, la moyenne a posteriori se déplace vers les petites valeurs de θ. Son déplacement dépend du nombre de lancers et de la force de l’a priori. Les premiers résultats peuvent toutefois aller dans l’autre sens : le hasard ne garantit pas une majorité de Pile dans une petite série."))),
          div(class="calcul-etape", h4("Mission 3 — Une pièce presque équilibrée"),
            p(strong("Prédire : "), "Si la probabilité réelle de Face est de 0,52, quelques lancers suffiront-ils à établir que Face est favorisée ?"),
            p(strong("Expérimenter : "), "Choisissez 0,52, commencez une nouvelle expérience et utilisez l’a priori uniforme. Ajoutez progressivement des lancers. Dans Explorer, observez l’intervalle crédible. Choisissez ensuite « Probabilité au-delà d’un seuil », réglez le seuil à 0,5 et observez la probabilité que θ lui soit supérieur."),
            p(strong("Expliquer : "), "Comment ces résultats évoluent-ils ? Pourquoi peuvent-ils varier d’une expérience à l’autre ? La probabilité a posteriori dépend des observations et de l’a priori ; elle ne garantit pas une conclusion particulière après un nombre donné de lancers."),
            tags$details(tags$summary("Voir une piste de réponse"),
              p("Comme 0,52 est proche de 0,50, les fluctuations d’échantillonnage peuvent masquer ou amplifier cette petite différence. L’intervalle crédible tend à se resserrer avec davantage de lancers, mais ses bornes et la probabilité que θ dépasse 0,5 peuvent monter ou descendre au fil des séries. Des expériences différentes peuvent donc conduire temporairement à des conclusions différentes."))),
          div(class="calcul-etape", h4("Mission 4 — Même centre, force différente"),
            p(strong("Prédire : "), "Les a priori Uniforme et Plutôt équilibrée ont la même moyenne. Leur influence sur les résultats sera-t-elle la même ?"),
            p(strong("Expérimenter : "), "Choisissez une probabilité réelle de Face de 0,70 et commencez une nouvelle expérience. Avant tout lancer, comparez les deux courbes dans Comparer les a priori. Ajoutez 10 lancers, puis comparez leurs moyennes et leurs intervalles ; ajoutez ensuite d’autres séries."),
            p(strong("Expliquer : "), "Pourquoi deux distributions centrées sur 0,5 peuvent-elles conduire à des conclusions différentes ?"),
            tags$details(tags$summary("Voir une piste de réponse"),
              p("Les deux a priori ont une moyenne de 0,5, mais l’a priori Plutôt équilibrée est plus concentré autour de cette valeur. Son poids relatif dans la moyenne a posteriori est plus grand pour un même nombre de lancers. Si les premières données sont proches de 0,5, les moyennes peuvent néanmoins être très semblables ; comparez aussi les intervalles."))),
          div(class="calcul-etape", h4("Mission 5 — Faire varier le seuil"),
            p(strong("Prédire : "), "Après une série de lancers, que devient la probabilité a posteriori lorsque le seuil passe de 0,5 à 0,7 ?"),
            p(strong("Expérimenter : "), "Dans Explorer, choisissez « Probabilité au-delà d’un seuil ». Avec les mêmes lancers et le même a priori, déplacez le seuil de 0,5 à 0,7 et observez l’aire et la probabilité affichées."),
            p(strong("Expliquer : "), "Pourquoi cette probabilité ne peut-elle pas augmenter lorsque le seuil augmente ? Que se passe-t-il aux seuils 0 et 1 ?"),
            tags$details(tags$summary("Voir une piste de réponse"),
              p("L’ensemble des valeurs supérieures à 0,7 est inclus dans celui des valeurs supérieures à 0,5 : l’aire correspondante ne peut donc qu’être plus petite ou égale. Pour les distributions continues utilisées ici, la probabilité vaut 100 % au seuil 0 et 0 % au seuil 1. Les résultats affichés peuvent être arrondis."))),
          div(class="calcul-etape", h4("Mission 6 — Un a priori très confiant"),
            p(strong("Prédire : "), "Un a priori bêta(50, 50) affirme avec force que la pièce est équilibrée. Combien de lancers faudra-t-il pour que l’a posteriori reconnaisse une pièce qui donne Face 80 % du temps ?"),
            p(strong("Expérimenter : "), "Choisissez une probabilité réelle de Face de 0,80, commencez une nouvelle expérience et sélectionnez l’a priori Personnalisé avec α = 50 et β = 50. Ajoutez une série de 100 lancers, puis d’autres séries. Dans Comprendre le calcul, suivez le poids de l’a priori ; comparez avec l’a priori uniforme sans relancer la pièce."),
            p(strong("Expliquer : "), "Pourquoi la moyenne a posteriori reste-t-elle longtemps en deçà de la proportion observée ? Pourquoi un a priori très confiant doit-il être justifié ?"),
            tags$details(tags$summary("Voir une piste de réponse"),
              p("L’a priori bêta(50, 50) pèse comme 100 lancers fictifs équilibrés. Après 100 lancers réels, il compte encore pour la moitié de la moyenne a posteriori, qui reste vers 0,65 si la proportion observée est proche de 0,80. Un a priori fort accélère l’apprentissage lorsqu’il est juste, mais le freine lorsqu’il est faux : sa force doit refléter une information réelle."))),
          div(class="explication", strong("À retenir : "), "Une nouvelle expérience efface les lancers précédents. La probabilité de la pièce est fixée au premier lancer ; pour modifier la pièce utilisée, commencez une nouvelle expérience. Pour reproduire une expérience, entrez une graine avant le premier lancer."))
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(n=0L, faces=0L, theta=NULL, mode=NULL, revealed=FALSE,
                       hist=data.frame(n=integer(), faces=integer()))
  params <- reactive({
    req(input$prior)
    if (identical(input$prior, "Personnalisé")) {
      borne <- function(v, d) { v <- suppressWarnings(as.numeric(v)); if (length(v) != 1 || is.na(v)) d else min(max(v, 0.5), 500) }
      c(borne(input$alpha, 2), borne(input$beta, 2))
    } else priors[[input$prior]]
  })

  # ── Graine : la suite aléatoire d'une expérience est fixée au premier lancer ──
  graine_exp <- reactiveVal(NA_integer_)
  etat_rng <- NULL
  demarrer_suite <- function() {
    g <- suppressWarnings(as.integer(input$graine))
    if (length(g) != 1 || is.na(g) || g < 1) { set.seed(NULL); g <- sample.int(GRAINE_MAX, 1) }
    g <- min(g, GRAINE_MAX)
    set.seed(g)
    etat_rng <<- get(".Random.seed", envir = globalenv())
    graine_exp(g)
  }
  tirer <- function(f) {
    assign(".Random.seed", etat_rng, envir = globalenv())
    res <- f()
    etat_rng <<- get(".Random.seed", envir = globalenv())
    res
  }
  output$info_graine <- renderUI({
    g <- graine_exp()
    p(style = "font-size: 0.85em; color: #465d6d; margin-top: -6px;",
      if (is.na(g)) "La graine est fixée au premier lancer de l’expérience." else
        HTML(paste0("Graine de l’expérience en cours : <strong>", g, "</strong>. Même graine et mêmes clics : mêmes lancers.")))
  })
  output$info_prior <- renderUI({
    pr <- params()
    p(style = "font-size: 0.85em; color: #465d6d;",
      HTML(paste0("Moyenne a priori : <strong>", fmt(pr[1] / sum(pr)), "</strong>. Dans la moyenne a posteriori, cet a priori pèse comme <strong>",
      fmt_par(sum(pr)), "</strong> lancers fictifs (α + β).")))
  })
  posterior <- reactive({ p <- params(); c(p[1] + rv$faces, p[2] + rv$n - rv$faces) })
  observeEvent(input$nouvelle, {
    rv$n <- 0L; rv$faces <- 0L; rv$theta <- NULL; rv$mode <- NULL; rv$revealed <- FALSE
    rv$hist <- data.frame(n=integer(), faces=integer())
    graine_exp(NA_integer_)
  }, ignoreInit=TRUE)
  observeEvent(input$lancer, {
    if (is.null(rv$theta)) {
      demarrer_suite()
      rv$mode <- input$mode
      rv$theta <- if (input$mode == "mystere") tirer(function() runif(1, 0.05, 0.95)) else input$theta
    }
    f <- tirer(function() rbinom(1, size = input$lot, prob = rv$theta))
    rv$n <- rv$n + as.integer(input$lot); rv$faces <- rv$faces + as.integer(f)
    rv$hist <- rbind(rv$hist, data.frame(n=rv$n, faces=rv$faces))
  }, ignoreInit=TRUE)
  output$bouton_reveler <- renderUI({
    if (!is.null(rv$theta) && identical(rv$mode, "mystere")) {
      if (!rv$revealed) actionButton("reveler", "Révéler la probabilité réelle", width = "100%")
      else div(class="explication", HTML(paste0("<strong>Pièce révélée :</strong> probabilité réelle de Face = <strong>",
               fmt(rv$theta), "</strong>.")))
    }
  })
  observeEvent(input$reveler, { if (!is.null(rv$theta) && identical(rv$mode, "mystere")) rv$revealed <- TRUE }, ignoreInit=TRUE)
  output$etat_piece <- renderUI({
    if (is.null(rv$theta)) return(div(class="explication", "Choisissez la pièce, puis lancez-la. La probabilité réelle sera fixée pour toute l’expérience."))
    piece <- if (rv$mode == "mystere" && !rv$revealed) "Pièce mystère : probabilité réelle cachée." else paste0("Probabilité réelle de Face : ", fmt(rv$theta), ".")
    div(class="explication", piece, if (!identical(rv$mode, input$mode) || (rv$mode == "connue" && !isTRUE(all.equal(rv$theta, input$theta)))) " Les nouveaux réglages de la pièce prendront effet à la prochaine expérience." else NULL)
  })
  output$resume <- renderUI({
    pp <- posterior(); ci <- qbeta(c(.025,.975), pp[1], pp[2]); prop <- if (rv$n) fmt(rv$faces/rv$n) else "—"
    div(class="resume", div("Lancers", strong(rv$n)), div("Face / Pile", strong(paste(rv$faces, "/", rv$n-rv$faces))),
        div("Proportion de Face", strong(prop)), div("Moyenne a posteriori", strong(fmt(pp[1]/sum(pp)))),
        div("Intervalle crédible à 95 %", strong(paste0("[",fmt(ci[1])," ; ",fmt(ci[2]),"]"))))
  })
  output$graphique <- renderPlot({
    par(mar=c(5, 4.5, 3, 1) + 0.1)
    p <- params(); q <- posterior(); x <- seq(0, 1, length.out=2001)
    a <- dbeta(x, p[1], p[2]); b <- dbeta(x, q[1], q[2]); ymax <- max(c(a,b)[is.finite(c(a,b))])
    plot(x, b, type="l", lwd=3, col="#125c88", ylim=c(0,ymax*1.12), xlab="Probabilité de Face (θ)", ylab="Densité", main="A priori et a posteriori", xaxt="n", yaxt="n")
    axes_fr()
    ci <- qbeta(c(.025,.975), q[1],q[2])
    bornes <- if (identical(input$zone, "seuil")) c(input$seuil, 1) else ci
    xx <- sort(unique(c(bornes, x[x > bornes[1] & x < bornes[2]])))
    if(diff(bornes)>0) polygon(c(xx[1],xx,tail(xx,1)), c(0,dbeta(xx,q[1],q[2]),0), col=adjustcolor("#87bdd7",.35), border=NA)
    if (identical(input$zone, "seuil")) abline(v=input$seuil, col="#8b4290", lty=5, lwd=2)
    lines(x, a, col="#ce7924", lwd=2, lty=2); lines(x,b,col="#125c88",lwd=3)
    if(rv$n) abline(v=rv$faces/rv$n, col="#b74040", lty=3, lwd=2)
    if(!is.null(rv$theta) && (rv$mode == "connue" || rv$revealed)) abline(v=rv$theta, col="#28834c", lty=4, lwd=2)
    position <- if (q[1]/sum(q) > .5) "topleft" else "topright"
    legend(position, legend=c("A priori", "A posteriori", if(input$zone == "intervalle") "Intervalle crédible à 95 %" else "Aire au-delà du seuil", if(rv$n) "Proportion observée", if(!is.null(rv$theta) && (rv$mode == "connue" || rv$revealed)) "Probabilité réelle", if(input$zone == "seuil") "Seuil"),
           col=c("#ce7924", "#125c88", NA, if(rv$n) "#b74040", if(!is.null(rv$theta) && (rv$mode == "connue" || rv$revealed)) "#28834c", if(input$zone == "seuil") "#8b4290"),
           lty=c(2,1,NA,if(rv$n) 3,if(!is.null(rv$theta) && (rv$mode == "connue" || rv$revealed)) 4, if(input$zone == "seuil") 5), lwd=2,
           fill=c(NA, NA, adjustcolor("#87bdd7",.35), if(rv$n) NA, if(!is.null(rv$theta) && (rv$mode == "connue" || rv$revealed)) NA, if(input$zone == "seuil") NA), border=NA, bg="white", cex=1)
  })
  output$interpretation <- renderUI({
    q <- posterior(); ci <- qbeta(c(.025,.975),q[1],q[2])
    div(class="explication", if(rv$n==0) "Aucun lancer : la distribution a posteriori coïncide avec l’a priori." else
      paste0("Après ",rv$n," lancers, la moyenne a posteriori est de ",fmt(q[1]/sum(q)),". Selon ce modèle et cet a priori, la probabilité que θ se situe entre ",fmt(ci[1])," et ",fmt(ci[2])," est de 95 %. L’intervalle crédible est à queues égales.",
        if (!is.null(rv$theta) && (rv$mode == "connue" || rv$revealed)) paste0(" La probabilité réelle (", fmt(rv$theta), ") se trouve ",
          if (rv$theta >= ci[1] && rv$theta <= ci[2]) "dans" else "hors de", " cet intervalle.") else ""))
  })
  output$probabilite_superieure <- renderUI({
    q <- posterior()
    seuil <- input$seuil
    prob <- pbeta(seuil, q[1], q[2], lower.tail=FALSE)
    div(class="explication", HTML(paste0("Avec l’a priori choisi et les données actuelles : <strong>P(θ > ", fmt(seuil, 2),
        " | données) = ", fmt(100 * prob, 1), " %</strong>. Cette probabilité correspond à l’aire colorée sous la courbe a posteriori. ")),
        if (rv$n == 0) "Sans lancer, elle provient entièrement de l’a priori." else NULL)
  })
  output$ponderation <- renderUI({
    pr <- params(); total <- sum(pr) + rv$n
    wa <- sum(pr)/total; wd <- rv$n/total; m <- pr[1]/sum(pr)
    div(class="calcul-etape", h4("Comment l’a priori et les données se combinent"),
        p("La moyenne a posteriori est une moyenne pondérée de la moyenne a priori et de la proportion observée de Face."),
        div(class="calcul-ligne", "Poids de l’a priori : ", span(class="calcul-resultat", paste0(fmt(100*wa,1), " %")),
            " ; poids des données : ", span(class="calcul-resultat", paste0(fmt(100*wd,1), " %"))),
        if (rv$n > 0) div(class="calcul-ligne", paste0(fmt(wa,3), " × ", fmt(m,3), " + ", fmt(wd,3), " × ", fmt(rv$faces/rv$n,3), " = "),
                          span(class="calcul-resultat", fmt(posterior()[1]/sum(posterior()))))
        else div(class="calcul-note", "Sans lancer, le poids des données est nul et la proportion observée n’est pas encore définie."),
        div(class="calcul-note", paste0("La moyenne a posteriori est aussi la probabilité, selon le modèle, que le prochain lancer donne Face. Avec l’a priori uniforme, elle vaut (Face + 1) / (n + 2) : c’est la règle de succession de Laplace.")),
        div(class="calcul-note", paste0("Les poids sont calculés à partir de α + β = ", fmt_par(sum(pr)),
          " et du nombre de lancers n = ", rv$n, ". Les valeurs affichées sont arrondies ; le calcul utilise les valeurs non arrondies. Avec plus de lancers, le poids relatif de l’a priori diminue.")))
  })
  # Comparaison avec l'intervalle de confiance fréquentiste (Clopper-Pearson)
  output$frequentiste <- renderUI({
    if (rv$n == 0) return(NULL)
    po <- posterior(); ci <- qbeta(c(.025, .975), po[1], po[2])
    cp <- binom.test(rv$faces, rv$n)$conf.int
    div(class="calcul-etape", h4("Et l’intervalle de confiance fréquentiste ?"),
      div(class="calcul-ligne", paste0("Intervalle crédible à 95 % (bayésien, a priori ", input$prior, ") : "),
          span(class="calcul-resultat", paste0("[", fmt(ci[1]), " ; ", fmt(ci[2]), "]"))),
      div(class="calcul-ligne", "Intervalle de confiance à 95 % (exact de Clopper-Pearson) : ",
          span(class="calcul-resultat", paste0("[", fmt(cp[1]), " ; ", fmt(cp[2]), "]"))),
      div(class="calcul-note", "Les deux intervalles se ressemblent souvent, surtout avec un a priori peu informatif et beaucoup de lancers, mais ils ne disent pas la même chose. ",
          "L’intervalle crédible donne une probabilité sur θ, sachant les données et l’a priori. ",
          "L’intervalle de confiance décrit une méthode : construite ainsi à chaque expérience, elle couvrirait la vraie valeur de θ au moins 95 % du temps. ",
          "Pour un intervalle de confiance particulier, θ est dedans ou non ; on ne lui attache pas de probabilité."))
  })

  output$evolution <- renderPlot({
    par(mar=c(5, 4.5, 3, 1) + 0.1)
    h <- rv$hist; validate(need(nrow(h)>0,"Ajoutez une série de lancers pour voir l’évolution.")); p <- params()
    plot(h$n,h$faces/h$n, type="o", pch=19, col="#b74040", ylim=c(0,1), xlab="Nombre cumulé de lancers", ylab="Probabilité de Face", main="Évolution au fil des lancers", xaxt="n", yaxt="n")
    axes_fr()
    lines(h$n,(p[1]+h$faces)/(sum(p)+h$n),type="o",pch=16,col="#125c88")
    if(rv$mode=="connue" || rv$revealed) abline(h=rv$theta,col="#28834c",lty=2)
    position <- if (mean(c(h$faces/h$n, (p[1]+h$faces)/(sum(p)+h$n))) > .5) "bottomleft" else "topleft"
    legend(position, legend=c("Proportion observée","Moyenne a posteriori",if(rv$mode=="connue" || rv$revealed) "Probabilité réelle"),col=c("#b74040","#125c88",if(rv$mode=="connue" || rv$revealed) "#28834c"),lty=c(1,1,if(rv$mode=="connue" || rv$revealed) 2),pch=c(19,16,if(rv$mode=="connue" || rv$revealed) NA),bg="white", cex=1)
  })
  couleurs_comparaison <- c("#125c88", "#a45f00", "#9b3374", "#28834c")
  posterior_tous <- reactive({
    lapply(priors, function(p) c(p[1] + rv$faces, p[2] + rv$n - rv$faces))
  })
  output$comparaison <- renderPlotly({
    x <- seq(0, 1, length.out=2001)
    qs <- posterior_tous()
    traits <- c("solid", "dash", "dot", "dashdot")
    g <- plot_ly()
    for (i in seq_along(qs)) {
      densite <- dbeta(x, qs[[i]][1], qs[[i]][2])
      info <- paste0("A priori : ", names(priors)[i],
        "<br>θ : ", fmt(x), "<br>Densité : ", fmt(densite, 3))
      g <- add_trace(g, x=x, y=densite, type="scatter", mode="lines",
        name=names(priors)[i], text=info, hoverinfo="text",
        line=list(color=couleurs_comparaison[i], width=3, dash=traits[i]))
    }
    if (rv$n > 0) {
      prop <- rv$faces/rv$n
      ymax <- max(vapply(qs, function(q) max(dbeta(x,q[1],q[2])), numeric(1)))
      g <- add_trace(g, x=c(prop,prop), y=c(0,ymax), type="scatter", mode="lines",
        name="Proportion observée", text=paste0("Proportion observée : ",fmt(prop)),
        hoverinfo="text", line=list(color="#555555", width=2, dash="dash"))
    }
    layout(g, title=list(text=if(rv$n == 0) "Distributions a priori" else "Distributions a posteriori", font=list(size=16)),
      xaxis=list(title="Probabilité de Face (θ)", range=c(0,1)),
      yaxis=list(title="Densité", rangemode="tozero"),
      legend=list(font=list(size=13), orientation="h", x=0, y=-0.35, yanchor="top"),
      margin=list(l=60,r=20,t=50,b=160), hovermode="closest", separators=", ")
  })
  output$table_comparaison <- renderTable({
    qs <- posterior_tous(); avant <- rv$n == 0
    tab <- data.frame("A priori"=names(priors),
      "Distribution"=vapply(qs, function(q) paste0("bêta(", fmt_par(q[1]), ", ", fmt_par(q[2]), ")"), character(1)),
      "Moyenne"=vapply(qs, function(q) fmt(q[1]/sum(q)), character(1)),
      "Intervalle"=vapply(qs, function(q) { ci <- qbeta(c(.025,.975),q[1],q[2]); paste0("[",fmt(ci[1])," ; ",fmt(ci[2]),"]") }, character(1)),
      check.names=FALSE)
    names(tab)[2:4] <- if (avant) c("Distribution a priori", "Moyenne a priori", "Intervalle central à 95 %")
                       else c("A posteriori", "Moyenne", "Intervalle crédible à 95 %")
    tab
  }, striped=TRUE, bordered=TRUE, spacing="m")
  output$ecart_comparaison <- renderUI({
    moyennes <- vapply(posterior_tous(), function(q) q[1]/sum(q), numeric(1))
    imin <- which.min(moyennes); imax <- which.max(moyennes)
    div(class="explication", strong("Comparer les conclusions : "),
        paste0("Les moyennes ", if (rv$n == 0) "a priori" else "a posteriori",
          " vont de ", fmt(moyennes[imin]), " (", names(priors)[imin], ") à ",
          fmt(moyennes[imax]), " (", names(priors)[imax], "). Leur écart est de ",
          fmt(moyennes[imax] - moyennes[imin]), ". "),
        if (rv$n == 0) "Ajoutez des lancers pour voir comment cet écart évolue avec les mêmes observations." else
          "Cet écart résume les différences entre moyennes ; il ne décrit pas à lui seul les différences entre les distributions ou leurs intervalles crédibles.")
  })
  output$comparaison_evolution <- renderPlotly({
    h <- rbind(data.frame(n=0L, faces=0L), rv$hist)
    g <- plot_ly()
    traits <- c("solid", "dash", "dot", "dashdot")
    symboles <- c("circle", "square", "diamond", "triangle-up")
    for (i in seq_along(priors)) {
      p <- priors[[i]]
      moyennes <- (p[1] + h$faces)/(sum(p) + h$n)
      info <- paste0("A priori : ", names(priors)[i],
        "<br>Lancers : ", h$n,
        "<br>Moyenne ", ifelse(h$n == 0, "a priori", "a posteriori"), " : ", fmt(moyennes),
        "<br>Face : ", h$faces, " ; Pile : ", h$n - h$faces)
      g <- add_trace(g, x=h$n, y=moyennes, type="scatter", mode="lines+markers",
        name=names(priors)[i], text=info, hoverinfo="text",
        line=list(color=couleurs_comparaison[i], width=2, dash=traits[i]),
        marker=list(color=couleurs_comparaison[i], size=8, symbol=symboles[i]))
    }
    if (nrow(h) > 1) {
      hp <- h[h$n > 0, ]
      g <- add_trace(g, x=hp$n, y=hp$faces/hp$n, type="scatter", mode="lines+markers", name="Proportion observée",
        text=paste0("Proportion observée : ", fmt(hp$faces/hp$n), "<br>Lancers : ", hp$n), hoverinfo="text",
        line=list(color="#555555", width=1.5, dash="longdash"), marker=list(color="#555555", size=6, symbol="x"))
    }
    layout(g, title=list(text="Influence de l’a priori", font=list(size=16)),
      xaxis=list(title="Nombre cumulé de lancers", rangemode="tozero"),
      yaxis=list(title="Moyenne a posteriori", range=c(0,1)),
      legend=list(font=list(size=13), orientation="h", x=0, y=-0.35, yanchor="top"),
      margin=list(l=65, r=20, t=50, b=150), hovermode="closest", separators=", ")
  })
  output$bayes_courant <- renderUI({
    pr <- params(); po <- posterior(); f <- rv$faces; piles <- rv$n - f
    t <- input$candidate
    log_prior <- dbeta(t, pr[1], pr[2], log=TRUE)
    log_v <- f*log(t) + piles*log1p(-t)
    log_norm <- lbeta(po[1], po[2]) - lbeta(pr[1], pr[2])
    sci <- function(z) {
      if (!is.finite(z)) return(if (z < 0) "0" else "\\text{indéfinie}")
      exposant <- floor(z/log(10)); mantisse <- exp(z - exposant*log(10))
      if (exposant %in% c(-1, 0)) return(gsub(",", "{,}", fmt(exp(z), if (exposant == 0) 3 else 4), fixed=TRUE))
      paste0(gsub(",", "{,}", fmt(mantisse, 3), fixed=TRUE), "\\times 10^{", exposant, "}")
    }
    tex_num <- function(x, digits=3) gsub(",", "{,}", fmt(x, digits), fixed=TRUE)
    formule <- paste0("\\[\\begin{aligned}",
      "p(\\theta=", tex_num(t,2), "\\mid\\text{données}) & = ",
      "\\frac{p(\\theta=", tex_num(t,2), ")\\,p(\\text{données}\\mid\\theta=", tex_num(t,2), ")}{p(\\text{données})} \\\\[8pt]",
      "& = \\frac{", sci(log_prior), "\\times ", sci(log_v), "}{", sci(log_norm), "}",
      " = \\color{#a12626}{\\mathbf{", tex_num(dbeta(t,po[1],po[2]),4), "}}",
      "\\end{aligned}\\]")
    withMathJax(div(class="calcul-etape", h4("La règle avec vos données"),
        div(class="calcul-ligne", paste0("Pour θ = ", fmt(t, 2), " et ", f, " Face / ", piles, " Pile :")),
        div(class="formule", formule),
        div(class="calcul-note", "La vraisemblance et la constante correspondent ici à une séquence particulière de lancers. La constante est la probabilité de cette séquence, obtenue en tenant compte de toutes les valeurs possibles de θ. Les valeurs intermédiaires sont arrondies ; le résultat utilise les valeurs non arrondies."),
        div(class="calcul-note", "Le résultat est une densité en cette valeur de θ : ce n’est pas une probabilité ponctuelle.")))
  })
  output$calcul <- renderUI({
    pr <- params(); po <- posterior(); t <- input$candidate
    f <- rv$faces; piles <- rv$n - f
    logv <- f * log(t) + piles * log1p(-t)
    vraisemblance <- exp(logv)
    format_v <- if (vraisemblance == 0 && logv < 0) "trop petite pour être affichée numériquement" else sci_fr(vraisemblance)
    div(class="calcul-etape",
      h4("3. Pour la valeur candidate choisie"),
      div(class="calcul-ligne", "θ = ", span(class="calcul-resultat", fmt(t,2))),
      div(class="calcul-ligne", "Densité a priori en θ : ", span(class="calcul-resultat", fmt(dbeta(t,pr[1],pr[2]),4))),
      div(class="calcul-ligne", "Vraisemblance de la séquence : ",
          paste0(fmt(t,2), exposant(f), " × (1 − ", fmt(t,2), ")", exposant(piles), " = "),
          span(class="calcul-resultat", format_v)),
      div(class="calcul-note", paste0("Log-vraisemblance : ", fmt(logv,3), ". Ce calcul concerne une séquence particulière ; pour le seul nombre de faces, le facteur binomial s’ajoute, sans modifier l’a posteriori. La valeur absolue peut être très petite : ce sont les comparaisons entre valeurs candidates qui éclairent leur compatibilité relative avec les données.")),
      div(class="calcul-ligne", "Densité a posteriori en θ : ", span(class="calcul-resultat", fmt(dbeta(t,po[1],po[2]),4))),
      div(class="calcul-note", if (rv$n == 0) "Sans observations, la vraisemblance vaut 1 et les deux densités coïncident." else "Comparez plusieurs valeurs candidates : ce sont les hauteurs relatives de la courbe, et non des probabilités ponctuelles."))
  })
  output$formule_posterieure <- renderUI({
    pr <- params(); po <- posterior(); f <- rv$faces; piles <- rv$n-f
    ci <- qbeta(c(.025,.975), po[1], po[2])
    div(
      div(class="calcul-etape", h4("1. Point de départ et observations"),
        div(class="calcul-ligne", "A priori choisi : ", strong(input$prior), paste0(" — bêta(", fmt_par(pr[1]), ", ", fmt_par(pr[2]), ")")),
        div(class="calcul-ligne", "Données : ", span(class="calcul-resultat", paste0(rv$n, " lancers : ", f, " Face et ", piles, " Pile"))),
        if (rv$n == 0) div(class="calcul-note", "Aucun lancer pour le moment : ajoutez-en pour voir la mise à jour.") else NULL),
      div(class="calcul-etape", h4("2. Mise à jour bêta-binomiale"),
        div(class="calcul-ligne", "bêta(α + Face, β + Pile) = ",
            paste0("bêta(", fmt_par(pr[1]), " + ", f, ", ", fmt_par(pr[2]), " + ", piles, ") = "),
            span(class="calcul-resultat", paste0("bêta(", fmt_par(po[1]), ", ", fmt_par(po[2]), ")"))),
        div(class="calcul-ligne", "Moyenne a posteriori : ",
            paste0(fmt_par(po[1]), " / (", fmt_par(po[1]), " + ", fmt_par(po[2]), ") = "),
            span(class="calcul-resultat", fmt(po[1]/sum(po)))),
        div(class="calcul-ligne", "Intervalle crédible à 95 % : ",
            span(class="calcul-resultat", paste0("[",fmt(ci[1])," ; ",fmt(ci[2]),"]"))),
        div(class="calcul-note", "Intervalle à queues égales : selon l’a priori et le modèle, 95 % de la distribution a posteriori se trouve entre ces bornes.")))
  })
}
shinyApp(ui, server)
