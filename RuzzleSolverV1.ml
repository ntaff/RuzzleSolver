(****************************************************)
(***************PROJET RUZZLE SOLVER*****************)
(************NICOLAS TAFFOUREAU (@ntaff)*************)
(************ADRIEN TREILHOU (@AdrienFAC)************)
(*******************VERSION 1************************)
(****************************************************)


(********************** CHAINE **********************)

(* sousChaine : string * int * int -> string = <fun> *)
let sousChaine = fun (s, n, m)
-> if m < n then "" else sub_string s (n - 1) (m - n + 1);;

(* tetec : string -> char = <fun> *)
let tetec = fun 
"" -> failwith "tetec: chaine vide"
| s -> nth_char s 0;;

(* tetes : string -> string = <fun> *)
let tetes = fun s
-> string_of_char(tetec(s));;

(* reste : string -> string = <fun> *)
let reste = fun 
"" -> failwith "La chaine est vide"
| s -> sousChaine(s, 2, string_length s);;


(****************** LECTURE FICHIER ******************)

(* Lit un fichier ligne par ligne à partir de son fd et le renvois sous forme de le liste*)
(* readFileByLines : in_channel -> string list = <fun> *)
let rec readFileByLines fd =
	try
		let l = input_line fd in
		l :: readFileByLines(fd)
	with _ -> [];;


(********************* FONCTIONS DE L'ALGORITHME DE RECHERCHE *******************************)


(* Remplace la lettre à l'index "i" par un espace*)
(* removeLetter : string * int -> string = <fun> *)
let rec removeLetter = fun
(s, 0) -> " " ^ reste(s)
| (s, i) -> tetes(s) ^ removeLetter(reste(s), i - 1);;



(* spaceNext(avant, fin) ---> bool si le déplacement est possible
   si la case fin est adjacente à la case avant
   La fonction mem vérifie la condition si last est contenu dans la liste suivante*)
(* spaceNext : int * int -> bool = <fun> *)
let spaceNext = fun
(_, -1) -> true
| (0, last) -> mem last [0;4;5]
| (1, last) ->  mem last [0;2;4;5;6]
| (2, last) ->  mem last [1;3;5;6;7]
| (3, last) ->  mem last [2;6;7]
| (4, last) ->  mem last [0;1;5;8;9]
| (5, last) ->  mem last [0;1;2;4;6;8;9;10]
| (6, last) ->  mem last [1;2;3;5;7;9;10;11]
| (7, last) ->  mem last [2;3;6;10;11]
| (8, last) ->  mem last [4;5;9;12;13]
| (9, last) ->  mem last [4;5;6;8;10;12;13;14]
| (10, last) ->  mem last [5;6;7;9;11;13;14;15]
| (11, last) ->  mem last [6;7;10;14;15]
| (12, last) ->  mem last [8;9;13]
| (13, last) ->  mem last [8;9;10;12;14]
| (14, last) ->  mem last [9;10;11;13;15]
| (15, last) ->  mem last [10;11;14]
| (_, _) -> false;;



(* Fonction d'optimisation permettant de commencer la recherche à la première case possible, évite de
   re parcourir tout le tableau *)
(* resetIndex : int -> int = <fun> *)
let resetIndex = fun
0 -> 0
| i when (i mod 4 >= 1) & ((i - (i mod 4)) / 4 >= 1) -> i - 5
| i when (i / 4) = 0 -> i - 1
| i when (i mod 4) = 0 -> i - 4;;


(* Fonction de recherche appelant les fonctions annexes et renvois si un mot est constructible à partir du tableau de jeu  *)
(* recherche : string * string * int * int -> bool = <fun> *)
let rec recherche = fun
(_, "", _, _) -> true
| (_, _, 16, _) -> false
| (p, word, indice, last) -> if p.[indice] = tetec(word) & spaceNext(indice, last)
								then if recherche(removeLetter(p, indice), reste(word), resetIndex(indice), indice) = false
										then recherche(p, word, indice + 1, last)
									else true
							else recherche(p, word, indice + 1, last);;


(* Renvois une liste de tous les mots possible avec un tableau de jeu donné *)
(* start : string * string list -> string list = <fun> *)
let rec start = fun
(p, x :: l) -> if recherche(p, x, 0, -1)
					then x :: start(p, l)
				else start(p, l)
| _ -> [];;


(*********************** JEU DE TESTS *********************************)

(* Chemin d'accès absolu vers le dictionnaire *)
let file = "C:\Users\Taffoureau\Documents\dico.txt";;  

(* On ouvre le fichier dans un fd *)
let fd = open_in(file);; 

(* On tri le dictionnaire par taille de mots puis par ordre alphabétique à l'aide d'un tri Fusion *)
let fileContent = readFileByLines(fd);; 

(* On ferme le fd *)
close_in fd;;

(* On définie un tableau de jeu comme étant une chaîne de caractères *)
let tableaudejeu = "ditamjnaeazgesif";;

(* On prend l'empreinte de temps courante *)
let time = Sys__time();;

(* On lance la recherche des solution à partir du tableau de jeu donné *)
let result = start(tableaudejeu, fileContent);; (* 50 mots *)

(* Combien de temps a pris la recherche *)
let timefinal = Sys__time() -. time;;

(* On affiche le nombre de résultats et le temps mis pour les trouver *)
"Pour le tableau de jeu : " ^ tableaudejeu ^ " on trouve " ^ string_of_int(list_length result) ^ " résultats en : " ^ string_of_float(timefinal) ^ " secondes.";;
