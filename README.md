# RuzzleSolver

Implémentation d'un solveur du jeu [Ruzzle](https://fr.wikipedia.org/wiki/Ruzzle). 

V1 : Projet fonctionnel.
`Execution : 0.375sec pour trouver les résultats.(Construction du dictionnaire : 0.045 sec)`

V2 : Projet fonctionnel.
`Execution : 0.18sec pour trouver les résultats. (Construction de l'arbre : 15sec)`
      
V3 : Projet fonctionnel. 
`Execution : 0.03sec pour trouver les résultats. (Construction de l'arbre : 0.25sec)`

V3BONUS : Projet fonctionnel. 
`Execution : 0.025sec pour trouver les résultats et les points.`

V4 : A VENIR

Contient les fichiers suivants (V1) :

1. `dico.txt`: Contient un ensemble de 21529 mots, triés par ordre alphabétique.

2. `parser.py`: Contient des fonctions permettant de parser un fichier texte afin d'enlever les caractères indésirables (accents, mots composés...)
   - `transform(s)` Enlève les accents, les cedilles etc...
   - `singleWord(s)` Retourne false si le mot est un mot composé
   - `parseFile()` Parse le dictionnaire et le tri dans l'ordre alphabétique
   
3. `projetV1.ml`: Contient les fonctions permettant, à partir un tableau de jeu, de renvoyer l'ensemble des mots possibles, à partir d'un dictionnaire de mots
   - `sousChaine(s, n, m)` Retourne la chaîne de caractères entre les indices n et m (encapsulation de substring)
   - `tetec(s)` Renvoie le premier caractère d'une chaîne sous forme d'un char
   - `tetes(s)` Renvoie le premier caractère d'une chaîne sous forme d'un string
   - `readFileByLines(fd)` Lit un fichier ligne par ligne à partir de son fd et le renvois sous forme de le liste
   - `removeLetter(s, i)` Remplace la lettre à l'index "i" par un espace dans la chaîne s
   - `spaceNext(before, last` Renvois un true si le déplacement est possible, c'est à dire si la case fin est adjacente à la case avant la fonction mem vérifie la condition si last est contenu dans la liste suivante, false sinon.
   - `resetIndex(i)` Fonction d'optimisation permettant de commencer la recherche à la première case possible, évite de re parcourir tout le tableau
   - `recherche(s, s1, i, j)` Fonction de recherche appelant les fonctions annexes et renvois si un mot est constructible à partir du tableau de jeu
   - `start(s, sl)` Renvois une liste de tous les mots possible avec un tableau de jeu donné


About the authors                                                  {#about}
-----------------

This program was written by Nicolas Taffoureau and Adrien Treilhou.
