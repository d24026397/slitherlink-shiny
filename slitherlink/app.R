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

# Compte les segments autour d'une case
compter_segments <- function(ah, av, i, j) {
  ah[i, j] + ah[i + 1, j] + av[i, j] + av[i, j + 1]
}

# Compte les segments qui touchent un point (col, row)
segments_au_point <- function(ah, av, row, col) {
  total <- 0
  if (col > 1) total <- total + ah[row, col - 1]      # arête gauche
  if (col <= n) total <- total + ah[row, col]          # arête droite
  if (row > 1) total <- total + av[row - 1, col]      # arête du haut
  if (row <= n) total <- total + av[row, col]          # arête du bas
  return(total)
}

# Vérifie si les segments forment une seule boucle fermée
verifier_boucle <- function(ah, av) {
  # Règle 1 : chaque point doit avoir 0 ou 2 segments (jamais 1 ou 3)
  for (row in 1:(n + 1)) {
    for (col in 1:(n + 1)) {
      s <- segments_au_point(ah, av, row, col)
      if (s == 1 || s == 3) return("invalide")
    }
  }
  
  # Compter le total de segments tracés
  total_segments <- sum(ah) + sum(av)
  if (total_segments == 0) return("vide")
  
  # Règle 2 : une seule boucle (parcours en profondeur)
  # Trouver un point de départ (premier point avec 2 segments)
  depart_row <- NA
  depart_col <- NA
  for (row in 1:(n + 1)) {
    for (col in 1:(n + 1)) {
      if (segments_au_point(ah, av, row, col) == 2) {
        depart_row <- row
        depart_col <- col
        break
      }
    }
    if (!is.na(depart_row)) break
  }
  
  if (is.na(depart_row)) return("vide")
  
  # Parcourir la boucle depuis le point de départ
  visites <- 0
  cur_row <- depart_row
  cur_col <- depart_col
  prev_row <- NA
  prev_col <- NA
  
  repeat {
    visites <- visites + 1
    if (visites > total_segments + 1) break  # sécurité anti-boucle infinie
    
    # Trouver le prochain point connecté (différent du précédent)
    suivant_row <- NA
    suivant_col <- NA
    
    # Regarder les 4 voisins possibles
    voisins <- list(
      c(cur_row, cur_col - 1, "h_gauche"),
      c(cur_row, cur_col,     "h_droite"),
      c(cur_row - 1, cur_col, "v_haut"),
      c(cur_row, cur_col,     "v_bas")
    )
    
    # Arête gauche → voisin (row, col-1)
    if (cur_col > 1 && ah[cur_row, cur_col - 1] == 1) {
      nr <- cur_row; nc <- cur_col - 1
      if (is.na(prev_row) || nr != prev_row || nc != prev_col) {
        suivant_row <- nr; suivant_col <- nc
      }
    }
    # Arête droite → voisin (row, col+1)
    if (is.null(suivant_row) || is.na(suivant_row)) {
      if (cur_col <= n && ah[cur_row, cur_col] == 1) {
        nr <- cur_row; nc <- cur_col + 1
        if (is.na(prev_row) || nr != prev_row || nc != prev_col) {
          suivant_row <- nr; suivant_col <- nc
        }
      }
    }
    # Arête haut → voisin (row-1, col)
    if (is.null(suivant_row) || is.na(suivant_row)) {
      if (cur_row > 1 && av[cur_row - 1, cur_col] == 1) {
        nr <- cur_row - 1; nc <- cur_col
        if (is.na(prev_row) || nr != prev_row || nc != prev_col) {
          suivant_row <- nr; suivant_col <- nc
        }
      }
    }
    # Arête bas → voisin (row+1, col)
    if (is.null(suivant_row) || is.na(suivant_row)) {
      if (cur_row <= n && av[cur_row, cur_col] == 1) {
        nr <- cur_row + 1; nc <- cur_col
        if (is.na(prev_row) || nr != prev_row || nc != prev_col) {
          suivant_row <- nr; suivant_col <- nc
        }
      }
    }
    
    if (is.na(suivant_row)) break
    
    # On est revenu au départ → boucle complète !
    if (suivant_row == depart_row && suivant_col == depart_col) {
      if (visites == total_segments) return("boucle_valide")
      else return("plusieurs_boucles")
    }
    
    prev_row <- cur_row
    prev_col <- cur_col
    cur_row <- suivant_row
    cur_col <- suivant_col
  }
  
  return("en_cours")
}

ui <- fluidPage(
  titlePanel("Slitherlink"),
  mainPanel(
    plotOutput("grille", width = "400px", height = "400px",
               click = "clic_grille"),
    uiOutput("message")
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
  
  # Message de statut
  output$message <- renderUI({
    # Contraintes
    erreurs <- 0; ok <- 0
    for (i in 1:n) {
      for (j in 1:n) {
        val <- contraintes[n + 1 - i, j]
        if (!is.na(val)) {
          if (compter_segments(ah(), av(), i, j) == val) ok <- ok + 1
          else erreurs <- erreurs + 1
        }
      }
    }
    
    # Boucle
    statut_boucle <- verifier_boucle(ah(), av())
    
    # Couleur et texte selon le statut
    if (statut_boucle == "boucle_valide" && erreurs == 0) {
      div(style = "color: #27ae60; font-size: 18px; font-weight: bold; margin-top: 10px;",
          "Puzzle résolu ! Félicitations !")
    } else if (statut_boucle == "invalide") {
      div(style = "color: #e74c3c; font-size: 14px; margin-top: 10px;",
          "Un point a trop de segments (doit être 0 ou 2)")
    } else if (statut_boucle == "plusieurs_boucles") {
      div(style = "color: #e67e22; font-size: 14px; margin-top: 10px;",
          paste(ok, "/", ok + erreurs, "contraintes OK — attention : plusieurs boucles détectées"))
    } else {
      div(style = "color: #555; font-size: 14px; margin-top: 10px;",
          paste(ok, "/", ok + erreurs, "contraintes respectées"))
    }
  })
  
  output$grille <- renderPlot({
    plot(NULL,
         xlim = c(-0.5, n + 0.5), ylim = c(-0.5, n + 0.5),
         asp = 1, xlab = "", ylab = "", axes = FALSE)
    
    # Colorier les cases
    for (i in 1:n) {
      for (j in 1:n) {
        val <- contraintes[n + 1 - i, j]
        if (!is.na(val)) {
          s <- compter_segments(ah(), av(), i, j)
          couleur <- if (s == val) "#c8f7c5" else "white"
          rect(j - 1, i - 1, j, i, col = couleur, border = NA)
        }
      }
    }
    
    # Colorier les points invalides en rouge
    for (row in 1:(n + 1)) {
      for (col in 1:(n + 1)) {
        s <- segments_au_point(ah(), av(), row, col)
        if (s == 1 || s == 3) {
          points(col - 1, row - 1, pch = 19, cex = 2.2, col = "#e74c3c")
        }
      }
    }
    
    # Arêtes
    for (i in 0:n)
      for (j in 1:n)
        if (ah()[i + 1, j] == 1)
          segments(j - 1, i, j, i, col = "steelblue", lwd = 3)
    for (i in 1:n)
      for (j in 0:n)
        if (av()[i, j + 1] == 1)
          segments(j, i - 1, j, i, col = "steelblue", lwd = 3)
    
    # Points normaux
    for (i in 0:n)
      for (j in 0:n) {
        s <- segments_au_point(ah(), av(), i + 1, j + 1)
        if (s != 1 && s != 3)
          points(j, i, pch = 19, cex = 1.8)
      }
    
    # Chiffres
    for (i in 1:n) {
      for (j in 1:n) {
        val <- contraintes[n + 1 - i, j]
        if (!is.na(val)) {
          s <- compter_segments(ah(), av(), i, j)
          col_texte <- if (s == val) "#27ae60" else "#e74c3c"
          text(j - 0.5, i - 0.5, labels = val, cex = 2, font = 2, col = col_texte)
        }
      }
    }
  })
}

shinyApp(ui = ui, server = server)