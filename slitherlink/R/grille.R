#' Crée une grille Slitherlink vide
#'
#' @param n Taille de la grille (nombre de cases par côté)
#' @return Une liste contenant la grille et les arêtes
#' @export
creer_grille <- function(n) {
  list(
    n          = n,
    aretes_h   = matrix(0, nrow = n + 1, ncol = n),
    aretes_v   = matrix(0, nrow = n, ncol = n + 1)
  )
}

#' Bascule une arête horizontale
#'
#' @param grille La grille courante
#' @param i Ligne de l'arête
#' @param j Colonne de l'arête
#' @return La grille mise à jour
#' @export
basculer_arete_h <- function(grille, i, j) {
  grille$aretes_h[i, j] <- 1 - grille$aretes_h[i, j]
  grille
}

#' Bascule une arête verticale
#'
#' @param grille La grille courante
#' @param i Ligne de l'arête
#' @param j Colonne de l'arête
#' @return La grille mise à jour
#' @export
basculer_arete_v <- function(grille, i, j) {
  grille$aretes_v[i, j] <- 1 - grille$aretes_v[i, j]
  grille
}