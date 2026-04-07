library(shiny)

contraintes <- matrix(
  c( 2,  2, NA, NA,
     3,  2, NA,  1,
     3,  0,  2, NA,
     NA,  3,  2,  2),
  nrow = 4, byrow = TRUE
)

n <- nrow(contraintes)

# Arêtes horizontales : (n+1) lignes × n colonnes → 1 = tracée, 0 = non tracée
aretes_h <- matrix(0, nrow = n + 1, ncol = n)
# Arêtes verticales  : n lignes × (n+1) colonnes
aretes_v <- matrix(0, nrow = n, ncol = n + 1)

ui <- fluidPage(
  titlePanel("Slitherlink"),
  mainPanel(
    plotOutput("grille", width = "400px", height = "400px",
               click = "clic_grille")  # on écoute les clics !
  )
)

server <- function(input, output, session) {
  
  # Stockage réactif des arêtes (se met à jour quand on clique)
  ah <- reactiveVal(aretes_h)
  av <- reactiveVal(aretes_v)
  
  # Quand l'utilisateur clique sur la grille
  observeEvent(input$clic_grille, {
    cx <- input$clic_grille$x
    cy <- input$clic_grille$y
    
    seuil <- 0.2  # distance max pour détecter une arête
    
    # Chercher l'arête horizontale la plus proche du clic
    for (i in 0:n) {
      for (j in 1:n) {
        # Centre de l'arête horizontale entre (j-1, i) et (j, i)
        mx <- j - 0.5
        my <- i
        if (abs(cx - mx) < 0.4 && abs(cy - my) < seuil) {
          m <- ah()
          m[i + 1, j] <- 1 - m[i + 1, j]  # bascule 0↔1
          ah(m)
          return()
        }
      }
    }
    
    # Chercher l'arête verticale la plus proche du clic
    for (i in 1:n) {
      for (j in 0:n) {
        mx <- j
        my <- i - 0.5
        if (abs(cx - mx) < seuil && abs(cy - my) < 0.4) {
          m <- av()
          m[i, j + 1] <- 1 - m[i, j + 1]
          av(m)
          return()
        }
      }
    }
  })
  
  output$grille <- renderPlot({
    plot(NULL,
         xlim = c(-0.5, n + 0.5), ylim = c(-0.5, n + 0.5),
         asp = 1, xlab = "", ylab = "", axes = FALSE)
    
    # Dessiner les arêtes tracées
    for (i in 0:n) {
      for (j in 1:n) {
        if (ah()[i + 1, j] == 1) {
          segments(j - 1, i, j, i, col = "steelblue", lwd = 3)
        }
      }
    }
    for (i in 1:n) {
      for (j in 0:n) {
        if (av()[i, j + 1] == 1) {
          segments(j, i - 1, j, i, col = "steelblue", lwd = 3)
        }
      }
    }
    
    # Points
    for (i in 0:n) {
      for (j in 0:n) {
        points(j, i, pch = 19, cex = 1.8)
      }
    }
    
    # Chiffres
    for (i in 1:n) {
      for (j in 1:n) {
        val <- contraintes[n + 1 - i, j]
        if (!is.na(val)) {
          text(j - 0.5, i - 0.5, labels = val, cex = 2, font = 2, col = "#333333")
        }
      }
    }
  })
}

shinyApp(ui = ui, server = server)