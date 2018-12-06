
(********************** CHAINE **********************)

let tetec = fun 
"" -> failwith "tetec: chaine vide"
| s -> nth_char s 0;;

let tetes = fun s -> string_of_char(tetec(s));;

let reste = fun
"" -> ""
| s -> sub_string s (1) ((string_length s) - 1);;


(****************** LECTURE FICHIER ******************)

(* Lit un fichier ligne par ligne à partir de son fd et le renvois sous forme de le liste*)
(* in_channel -> string list *)
let rec readFileByLines fd =
	try
		let l = input_line fd in
		l :: readFileByLines(fd)
	with _ -> [];;

	
let rec setString = fun s -> let rec aux = fun
							(s, 0) -> s
							| (s, n) -> aux(s, n-1) ^ " "
							in aux(s, 16 - (string_length s));;

(* DEBUGAGE Affiche une liste de string *)
(* string list -> unit *)
let rec displayStringList = fun
(a :: b :: c :: d :: l) -> print_string(setString(a) ^ "   " ^ setString(b) ^ "   " ^ setString(c) ^ "   " ^ setString(d) ^ "\n");
						displayStringList(l)
|(a :: b :: c :: l) -> print_string(setString(a) ^ "   " ^ setString(b) ^ "   " ^ setString(c) ^ "\n");
						displayStringList(l)
|(a :: b :: l) -> print_string(setString(a) ^ "   " ^ setString(b) ^ "\n");
						displayStringList(l)
|(a :: l) -> print_string(setString(a) ^ "\n");
						displayStringList(l)
|[] -> print_string "\n";;

	
(********************* ARBRE ************************)

type Arbre = Noeud of (char * bool) * Arbre list;;

let rec space = fun
(-1) -> ""
| (0) -> " "
| (n) -> " " ^ space(n-1);;

(* DEBUGAGE Affiche d'un arbre *)
let rec displayTree = fun
(Noeud((c, b), lst) :: suite, p) -> print_string(space(p) ^ string_of_char(c) ^ "\n");
									displayTree(lst, p+1);
									displayTree(suite, p)
| ([], p) -> print_string("");;


let rec addInTree = fun
("", arbre) -> arbre
| (mot, []) -> [Noeud((mot.[0], (string_length mot) = 1), addInTree(reste(mot), []))]
| (mot, Noeud((c, b), lst) :: suite) -> if mot.[0] = c
											then Noeud((c, b), addInTree(reste(mot), lst)) :: suite
										else Noeud((c, b), lst) :: addInTree(mot, suite);;


let rec createTree = fun
(mot :: reste, arbre) -> createTree(reste, addInTree(mot, arbre))
| (_, arbre) -> arbre;;


let create = fun
(dico, Noeud((c, b), lst)) -> createTree(dico, lst);;

(********************** ALGO ************************)

let removeDuplicate = fun
(x :: l) -> let rec aux = fun
			(a :: b, lst) -> if (mem a lst) then aux(b, lst) else aux(b, a::lst)
			|([], lst) -> lst
			in aux(l, [x])
|[] -> [];;

let rec removeLetter = fun
(s, 0) -> "." ^ reste(s)
| (s, i) -> tetes(s) ^ removeLetter(reste(s), i - 1);;

exception Not_Found;; 
let rec getBranche = fun
(lettre, Noeud((c, b), lst) :: suite) -> if lettre = c
											then (Noeud((c, b), lst))
										 else getBranche(lettre, suite)
| (lettre, []) -> raise Not_Found;;


let rec canDo = fun
(x :: l, g) -> if g.[x] != `.`
					then x :: canDo(l, g)
				else canDo(l, g)
| (_, _) -> [];;


let deplacement = fun
(0, g) -> canDo([1;4;5], g)
| (1, g) -> canDo([0;2;4;5;6], g)
| (2, g) -> canDo([1;3;5;6;7], g)
| (3, g) -> canDo([2;6;7], g)
| (4, g) -> canDo([0;1;5;8;9], g)
| (5, g) -> canDo([0;1;2;4;6;8;9;10], g)
| (6, g) -> canDo([1;2;3;5;7;9;10;11], g)
| (7, g) -> canDo([2;3;6;10;11], g)
| (8, g) -> canDo([4;5;9;12;13], g)
| (9, g) -> canDo([4;5;6;8;10;12;13;14], g)
| (10, g) -> canDo([5;6;7;9;11;13;14;15], g)
| (11, g) -> canDo([6;7;10;14;15], g)
| (12, g) -> canDo([8;9;13], g)
| (13, g) -> canDo([8;9;10;12;14], g)
| (14, g) -> canDo([9;10;11;13;15], g)
| (15, g) -> canDo([10;11;14], g)
| (_, _) -> [];;



let rec start = fun
(16, g, arbre) -> []
| (i, g, arbre) -> start(i+1, g, arbre) @ let rec suite (x::l) g arbre mot =
											try
												let branche = getBranche(g.[x], arbre) and gnew = removeLetter(g, x) in
												let aux = fun
												(Noeud((c, b), lst)) -> (if b then [(mot ^ string_of_char(c))] else []) @ 
													let rec ici = fun
													((t :: q), gril, arbree, mott) -> (suite [t] gril arbree mott) @ ici(q, gril, arbree, mott)
													| ([], _, _, _) -> []
													in ici((l @ deplacement(x, g)), gnew, lst, (mot ^ string_of_char(c)))
												in aux(branche)
											with Not_Found -> if l = [] then [] else suite l g arbre mot
										in (suite [i] g arbre "");;

(****************************************************)

let file = "E:\FAC\Caml\Projet\dico.txt";;

let fd = open_in(file);;

let timeRead = Sys__time();;
let fileContent = readFileByLines(fd);;
let timeRead = Sys__time() -. timeRead;;

close_in fd;;

let timeTree = Sys__time();;
let arbre = create(fileContent, Noeud((` `, false), []));;
let timeTree = Sys__time() -. timeTree;;

let timeResult = Sys__time();;
let result = removeDuplicate(start(0, "ditamjnaeazgesif", arbre));; (* 428 mots *)
let timeResult = Sys__time() -. timeResult;;

print_string("Lecture du fichier: " ^ string_of_float(timeRead));;
print_string("Creation de l'arbre: " ^ string_of_float(timeTree));;
print_string("Recherche des résultats: " ^ string_of_float(timeResult));;
print_string("Nombre de résultats: " ^ string_of_int(list_length result));;
