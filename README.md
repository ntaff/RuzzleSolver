# RuzzleSolver

Implémentation d'un solveur du jeu [Ruzzle](https://fr.wikipedia.org/wiki/Ruzzle). 

Contient les fichiers suivants :

1. `dico.txt`: Contient un ensemble de 21000 mots, triés.

2. `parser.py`: Contient des fonctions permettant de parser un fichier texte afin d'enlever les caractères indésirables (accents, mots composés...)
   - `transform(s)` Enlève les accents, les cedilles etc...
   - `singleWord(s)` Retourne false si le mot est un mot composé
   - `parseFile()` Parse le dictionnaire et le tri dans l'ordre alphabétique



About the authors                                                  {#about}
-----------------

This program was written by Nicolas Taffoureau and Adrien Treilhou.
