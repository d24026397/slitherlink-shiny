library(shiny)

contraintes <- matrix(
  c( 2,  2, NA, NA,
     3,  2, NA,  1,
     3,  0,  2, NA,
     NA,  3,  2,  2),
  nrow = 4, byrow = TRUE
)

n <- nrow(contraintes)
aretes_h <- matrix(0, nrow = n + 1, ncol = n)
aretes_v <- matrix(0, nrow = n, ncol = n + 1)

# Compte les segments autour d'une case (i=ligne, j=colonne)
compter_segments <- function(ah, av, i, j) {
  haut  <- ah[i,     j]   # arête du haut
  bas   <- ah[i + 1, j]   # arête du bas
  gauche <- av[i,   j]    # arête gauche
  droite <- av[i,   j + 1] # arête droite
  return(haut + bas + gauche + droite)
}

ui <- fluidPage(
  titlePanel("Slitherlink"),
  mainPanel(
    plotOutput("grille", width = "400px", height = "400px",
               click = "clic_grille"),
    textOutput("message")  # message de validation
  )
)

server <- function(input, output, session) {
  
  ah <- reactiveVal(aretes_h)
  av <- reactiveVal(aretes_v)
  
  observeEvent(input$clic_grille, {
    cx <- input$clic_grille$x
    cy <- input$clic_grille$y
    seuil <- 0.2
    
    for (i in 0:n) {
      for (j in 1:n) {
        if (abs(cx - (j - 0.5)) < 0.4 && abs(cy - i) < seuil) {
          m <- ah()
          m[i + 1, j] <- 1 - m[i + 1, j]
          ah(m)
          return()
        }
      }
    }
    for (i in 1:n) {
      for (j in 0:n) {
        if (abs(cx - j) < seuil && abs(cy - (i - 0.5)) < 0.4) {
          m <- av()
          m[i, j + 1] <- 1 - m[i, j + 1]
          av(m)
          return()
        }
      }
    }
  })
  
  # Vérification des contraintes
  output$message <- renderText({
    erreurs <- 0
    ok <- 0
    
    for (i in 1:n) {
      for (j in 1:n) {
        val <- contraintes[n + 1 - i, j]
        if (!is.na(val)) {
          segments_case <- compter_segments(ah(), av(), i, j)
          if (segments_case == val) {
            ok <- ok + 1
          } else {
            erreurs <- erreurs + 1
          }
        }
      }
    }
    
    total <- ok + erreurs
    if (erreurs == 0 && ok > 0) {
      paste("Toutes les contraintes sont respectées !")
    } else {
      paste(ok, "/", total, "contraintes respectées")
    }
  })
  
  output$grille <- renderPlot({
    plot(NULL,
         xlim = c(-0.5, n + 0.5), ylim = c(-0.5, n + 0.5),
         asp = 1, xlab = "", ylab = "", axes = FALSE)
    
    # Colorier les cases selon leur état
    for (i in 1:n) {
      for (j in 1:n) {
        val <- contraintes[n + 1 - i, j]
        if (!is.na(val)) {
          segments_case <- compter_segments(ah(), av(), i, j)
          couleur <- if (segments_case == val) "#c8f7c5" else "white"
          rect(j - 1, i - 1, j, i, col = couleur, border = NA)
        }
      }
    }
    
    # Arêtes tracées
    for (i in 0:n) {
      for (j in 1:n) {
        if (ah()[i + 1, j] == 1)
          segments(j - 1, i, j, i, col = "steelblue", lwd = 3)
      }
    }
    for (i in 1:n) {
      for (j in 0:n) {
        if (av()[i, j + 1] == 1)
          segments(j, i - 1, j, i, col = "steelblue", lwd = 3)
      }
    }
    
    # Points
    for (i in 0:n)
      for (j in 0:n)
        points(j, i, pch = 19, cex = 1.8)
    
    # Chiffres avec couleur selon l'état
    for (i in 1:n) {
      for (j in 1:n) {
        val <- contraintes[n + 1 - i, j]
        if (!is.na(val)) {
          segments_case <- compter_segments(ah(), av(), i, j)
          couleur_texte <- if (segments_case == val) "#27ae60" else "#e74c3c"
          text(j - 0.5, i - 0.5, labels = val, cex = 2, font = 2, col = couleur_texte)
        }
      }
    }
  })
}

shinyApp(ui = ui, server = server)