# Slitherlink

Application Shiny interactive pour jouer au puzzle logique **Slitherlink**.

## Présentation du jeu

Le Slitherlink est un casse-tête logique japonais. Le but est de tracer
des segments entre des points voisins pour former une seule boucle fermée.

Règles :
- La boucle doit être unique et fermée
- Elle ne doit pas se croiser
- Le chiffre dans une case indique combien de ses 4 côtés font partie de la boucle

## Installation

```r
# Installer les dépendances
install.packages("shiny")

# Cloner le dépôt
# git clone https://github.com/d24026397/slitherlink-shiny.git
```

## Lancer l'application

```r
library(shiny)
runApp("app.R")
```

## Fonctionnalités

- Affichage interactif de la grille
- Tracer/effacer des segments en cliquant
- Vérification automatique des contraintes (vert = correct, rouge = incorrect)
- Détection des points invalides
- Détection de la boucle fermée

## Structure du projet

slitherlink/
├── R/
│   ├── grille.R       # Fonctions de gestion de la grille
│   └── validation.R   # Fonctions de vérification des règles
├── app.R              # Application Shiny
├── DESCRIPTION        # Métadonnées du package
└── README.md          # Ce fichier

## Auteur

Ton Houenafa Esperance DJOSSOU — Faculté des sciences de Montpellier