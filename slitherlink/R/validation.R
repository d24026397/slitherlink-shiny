#' Compte les segments autour d'une case
#'
#' @param grille La grille courante
#' @param i Ligne de la case
#' @param j Colonne de la case
#' @return Nombre de segments autour de la case
#' @export
compter_segments <- function(grille, i, j) {
  grille$aretes_h[i, j] +
    grille$aretes_h[i + 1, j] +
    grille$aretes_v[i, j] +
    grille$aretes_v[i, j + 1]
}

#' Compte les segments sur un point
#'
#' @param grille La grille courante
#' @param row Ligne du point
#' @param col Colonne du point
#' @return Nombre de segments sur ce point
#' @export
segments_au_point <- function(grille, row, col) {
  n <- grille$n
  total <- 0
  if (col > 1)  total <- total + grille$aretes_h[row, col - 1]
  if (col <= n) total <- total + grille$aretes_h[row, col]
  if (row > 1)  total <- total + grille$aretes_v[row - 1, col]
  if (row <= n) total <- total + grille$aretes_v[row, col]
  total
}

#' Vérifie si les segments forment une boucle valide
#'
#' @param grille La grille courante
#' @return Un texte décrivant le statut : "vide", "invalide", "en_cours", "boucle_valide", "plusieurs_boucles"
#' @export
verifier_boucle <- function(grille) {
  n <- grille$n
  ah <- grille$aretes_h
  av <- grille$aretes_v
  
  for (row in 1:(n + 1)) {
    for (col in 1:(n + 1)) {
      s <- segments_au_point(grille, row, col)
      if (s == 1 || s == 3) return("invalide")
    }
  }
  
  total_segments <- sum(ah) + sum(av)
  if (total_segments == 0) return("vide")
  
  depart_row <- NA
  depart_col <- NA
  for (row in 1:(n + 1)) {
    for (col in 1:(n + 1)) {
      if (segments_au_point(grille, row, col) == 2) {
        depart_row <- row
        depart_col <- col
        break
      }
    }
    if (!is.na(depart_row)) break
  }
  
  if (is.na(depart_row)) return("vide")
  
  visites <- 0
  cur_row <- depart_row
  cur_col <- depart_col
  prev_row <- NA
  prev_col <- NA
  
  repeat {
    visites <- visites + 1
    if (visites > total_segments + 1) break
    
    suivant_row <- NA
    suivant_col <- NA
    
    if (cur_col > 1 && ah[cur_row, cur_col - 1] == 1) {
      nr <- cur_row; nc <- cur_col - 1
      if (is.na(prev_row) || nr != prev_row || nc != prev_col) {
        suivant_row <- nr; suivant_col <- nc
      }
    }
    if (is.na(suivant_row) && cur_col <= n && ah[cur_row, cur_col] == 1) {
      nr <- cur_row; nc <- cur_col + 1
      if (is.na(prev_row) || nr != prev_row || nc != prev_col) {
        suivant_row <- nr; suivant_col <- nc
      }
    }
    if (is.na(suivant_row) && cur_row > 1 && av[cur_row - 1, cur_col] == 1) {
      nr <- cur_row - 1; nc <- cur_col
      if (is.na(prev_row) || nr != prev_row || nc != prev_col) {
        suivant_row <- nr; suivant_col <- nc
      }
    }
    if (is.na(suivant_row) && cur_row <= n && av[cur_row, cur_col] == 1) {
      nr <- cur_row + 1; nc <- cur_col
      if (is.na(prev_row) || nr != prev_row || nc != prev_col) {
        suivant_row <- nr; suivant_col <- nc
      }
    }
    
    if (is.na(suivant_row)) break
    
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