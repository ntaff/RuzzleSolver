#open "hashtbl";;

(********************** CHAINE **********************)

let longChaine = fun s -> string_length s;;

let sousChaine = fun (s, n, m) -> if m < n
									then ""
								else sub_string s (n - 1) (m - n + 1);;

let tetec = fun 
"" -> failwith "tetec: chaine vide"
| s -> nth_char s 0;;

let tetes = fun s -> string_of_char(tetec(s));;

let reste = fun 
"" -> failwith "La chaine est vide"
| s -> sousChaine(s, 2, longChaine(s));;


(****************** LECTURE FICHIER ******************)

(* Lit un fichier ligne par ligne à partir de son fd et le renvois sous forme de le liste*)
(* in_channel -> string list *)
let rec readFileByLines fd =
	try
		let l = input_line fd in
		l :: readFileByLines(fd)
	with _ -> [];;

(* DEBUGAGE Affiche une liste de string *)
(* string list -> unit *)
let rec displayStringList = fun
(x :: l) -> print_string(x ^ "\n"); displayStringList(l)
|[] -> print_string "\n";;


(****************************************************)


let rec copyString = fun
"" -> ""
| s -> tetes(s) ^ copyString(reste(s));;


let rec removeLetter = fun
(p, 0) -> " " ^ reste(p)
| (p, index) -> tetes(p) ^ removeLetter(reste(p), index - 1);;


let spaceNext = fun
(0, last) -> mem last [0;4;5]
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


let resetIndex = fun x -> 0;;


let rec suite = fun
(_, "", _, _) -> true
| (_, _, 16, _) -> false
| (p, word, indice, last) -> if p.[indice] = tetec(word) & spaceNext(indice, last) = true
								then if suite(removeLetter(copyString(p), indice), reste(word), 0, indice) = false
										then suite(p, word, indice + 1, last)
									else true
							else suite(p, word, indice + 1, last);;


let rec depart = fun
(p, word, 16) -> false
| (p, word, indice) -> if p.[indice] = tetec(word)
							then if suite(removeLetter(copyString(p), indice), reste(word), 0, indice) = false
									then depart(p, word, indice + 1)
								else true
						else depart(p, word, indice + 1);;

let rec start = fun
(p, x :: l) -> if depart(copyString(p), x, 0) = true
					then x :: start(p, l)
				else start(p, l)
| _ -> [];;




let file = "C:\Users\adrie\Desktop\Projet\dico.txt";;

let fd = open_in(file);;

let fileContent = readFileByLines(fd);;

let resultats = start("pudnsicyazbdifan", fileContent);;
