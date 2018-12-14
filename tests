(****************************************************)
(****************** JEUX DE TESTS *******************)
(****************************************************)

(* On inclu le projet contenant les fonctions du projet *)
include "projetV4.ml";;

(* Chemin d'accès absolu vers le dictionnaire *)
let file = "C:\Users\Taffoureau\Documents\dico.txt";;

(* La grille de jeu, sous forme de triplet (lettre, points, bonus) *)
let grille=[("d", 1, 0);
			("i", 1, 0);
			("t", 2, DL);
			("a", 1, 0);
			("m", 1, DW);
			("j", 1, 0);
			("n", 3, 0);
			("a", 1, 0);
			("e", 1, TW);
			("a", 1, 0);
			("z", 2, 0);
			("g", 1, 0);
			("e", 1, 0);
			("s", 1, TL);
			("i", 1, 0);
			("f", 2, 0)];;
			
(* On ouvre le fichier dans un file descriptor *)
let fd = open_in(file);;

(* On prend l'empreinte du temps courante *)
let timeRead = Sys__time();;

(* On recupère le contenu du dictionnaire *)
let fileContent = readFileByLines(fd);;

(* Combien de temps la lecture du dictionnaire a-t-elle prise *)
let timeRead = Sys__time() -. timeRead;;

(* On ferme le fd *)
close_in fd;;

(* On reprend l'empreinte de temps courante *)
let timeTree = Sys__time();;

(* On crée l'arbre associé au contenu du dictionnaire *)
let arbre = createTree(fileContent, Noeud((` `, false), []));;

(* Combien de temps la génération de l'arbre a-t-elle prise ? *)
let timeTree = Sys__time() -. timeTree;;

(* On reprend l'empreinte de temps courante *)
let timeResult = Sys__time();;

(* On lance l'algorithme de recherche des résultats à partir de la grille et de l'arbre construit *)
let (result, points) = solve grille arbre;;

(* Combien de temps la recherche des resultat a-t-elle prise ? *)
let timeResult = Sys__time() -. timeResult;;


(* On affiche statistiques, temps, points et nombre de résultats *)
print_string("\n\nLecture du fichier: " ^ string_of_float(timeRead));;
print_string("Creation de l'arbre: " ^ string_of_float(timeTree));;
print_string("Recherche des résultats: " ^ string_of_float(timeResult));;
print_string("Nombre de résultats: " ^ string_of_int(list_length result));;
print_string("Nombre de points: " ^ string_of_int(points));;
print_string("Liste des résultats:");;
displayStringList(result, "\n", 8);;
