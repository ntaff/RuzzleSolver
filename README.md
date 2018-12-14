# RuzzleSolver

Implémentation d'un solveur du jeu [Ruzzle](https://fr.wikipedia.org/wiki/Ruzzle) écrit entièrement en fonctionnel. 

V1 : Projet fonctionnel.
`Execution : 0.375sec pour trouver les résultats.(Construction du dictionnaire : 0.045 sec)`

V2 : Projet fonctionnel.
`Execution : 0.18sec pour trouver les résultats. (Construction de l'arbre : 15sec)`
      
V3 : Projet fonctionnel. 
`Execution : 0.03sec pour trouver les résultats. (Construction de l'arbre : 0.25sec)`

V4 : Projet fonctionnel. 
`Execution : 0.025sec pour trouver les résultats et les points.`

Contient les fichiers suivants (V4) :

1. `dico.txt`: Contient un ensemble de 21529 mots, triés par ordre alphabétique.

2. `parser.py`: Contient des fonctions permettant de parser un fichier texte afin d'enlever les caractères indésirables (accents, mots composés...)

3. `projetV4.ml`: Contient les fonctions permettant, à partir un tableau de jeu, de renvoyer l'ensemble des mots possibles, à partir d'un dictionnaire de mots

3. `jeuDeTests.ml`: Contient un exemple d'utilisation du solveur avec une grille donnée sous forme d'une chaîne de caractères


About the authors                                                  {#about}
-----------------

This program was written by Nicolas Taffoureau and Adrien Treilhou.
