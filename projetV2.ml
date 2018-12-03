
(********************** CHAINE **********************)

let sousChaine = fun (s, n, m) -> if m < n
									then ""
								else sub_string s (n - 1) (m - n + 1);;

let tetec = fun 
"" -> failwith "tetec: chaine vide"
| s -> nth_char s 0;;

let tetes = fun s -> string_of_char(tetec(s));;

let reste = fun 
"" -> failwith "La chaine est vide"
| s -> sousChaine(s, 2, string_length s);;


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


(********************* TRI **************************)

let compare s1 s2 = let len1 = (string_length s1) and len2 = (string_length s2) in
						len1 < len2 or
						(if len1 = len2
							then s1 < s2
						else false);;


let rec division lst =
    match lst with
    | a::b::l -> let (lst1,lst2) = division l in
				(a::lst1, b::lst2)
    | a::l -> (lst, l)
    | _ -> ([], []);;


let rec fusion lst1 lst2 =
    match (lst1, lst2) with
    | [],_ -> lst2
    | _,[] -> lst1
    | t1::q1, t2::q2 ->
        if (compare t1 t2) then
            t1::(fusion q1 lst2)
        else
            t2::(fusion lst1 q2);;


let rec tri_fusion lst =
    match lst with
    | a::b::l -> let (lst1, lst2) = division (a::b::l) in
            fusion (tri_fusion lst1) (tri_fusion lst2)
    | a::l -> lst
    | _ -> [];;

	
(********************* ARBRE ************************)

type Arbre = Noeud of string * Arbre list;;

(*let matchString = fun
("", _) -> true
| (s1, s2) -> s1 = (sub_string s2 0 (string_length s1));;*)

let rec matchString = fun
("", _) -> true
| (s1, s2) -> if s1.[0] = s2.[0] then matchString(reste(s1), reste(s2)) else false;;


let rec addInTree = fun
(mot, Noeud(s, lst) :: suite) -> if matchString(s, mot)
									then Noeud(s, addInTree(mot, lst)) :: suite
								 else Noeud(s, lst) :: addInTree(mot, suite)
| (mot, arbre) -> Noeud(mot, []) :: arbre;;


let rec createTree = fun
(mot :: reste, arbre) -> createTree(reste, addInTree(mot, arbre))
| (_, arbre) -> arbre;;

(********************* ALGO *************************)


(* Remplace la lettre à l'index "i" par un espace*)
(* string * int -> string *)
let rec removeLetter = fun
(s, 0) -> " " ^ reste(s)
| (s, i) -> tetes(s) ^ removeLetter(reste(s), i - 1);;


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


let resetIndex = fun
0 -> 0
| i when (i mod 4 >= 1) & ((i - (i mod 4)) / 4 >= 1) -> i - 5
| i when (i / 4) = 0 -> i - 1
| i when (i mod 4) = 0 -> i - 4
| _ -> 0;;


let rec recherche = fun
(_, "", _, _) -> true
| (_, _, 16, _) -> false
| (p, word, indice, last) -> if p.[indice] = tetec(word) & spaceNext(indice, last)
								then if recherche(removeLetter(p, indice), reste(word), resetIndex(indice), indice) = false
										then recherche(p, word, indice + 1, last)
									else true
							else recherche(p, word, indice + 1, last);;



let rec start = fun
(grille, Noeud(s, lst) :: suite) -> if recherche(grille, s, 0, -1)
											then s :: start(grille, lst) @ start(grille, suite)
										else start(grille, suite)
| (grille, []) -> [];;




(****************************************************)

let file = "C:\Users\adrie\Desktop\Projet\dico.txt";;

let fd = open_in(file);;

let fileContent = tri_fusion(readFileByLines(fd));;

close_in fd;;

let arbre = createTree(fileContent, [Noeud("", [])]);;

let result = start("cotanuaeoresiphc", arbre);; (* 129 *)

let time = Sys__time();

Sys__time() -. time;;
