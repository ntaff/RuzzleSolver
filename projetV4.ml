(****************************************************)
(***************PROJET RUZZLE SOLVER*****************)
(************NICOLAS TAFFOUREAU (@ntaff)*************)
(************ADRIEN TREILHOU (@AdrienFAC)************)
(*****************VERSION 3BONUS*********************)
(****************************************************)

let DL = 1;; (* Lettre compte double *)
let TL = 2;; (* Lettre compte triple *)
let DW = 3;; (* Mot compte double *)
let TW = 4;; (* Mot compte triple *)


(****************************************************)
(********************** CHAINE **********************)
(****************************************************)

(* tetec : string -> char = <fun> *)
let tetec = fun 
"" -> failwith "tetec: chaine vide"
| s -> nth_char s 0;;

(* tetes : string -> string = <fun> *)
let tetes = fun s -> string_of_char(tetec(s));;

(* reste : string -> string = <fun> *)
let reste = fun
"" -> ""
| s -> sub_string s (1) ((string_length s) - 1);;



(****************************************************)
(**************** LECTURE DU FICHIER ****************)
(****************************************************)

(* Lit un fichier ligne par ligne à partir de son file descriptor
et le renvois sous forme de le liste de string *)
(* readFileByLines : in_channel -> string list = <fun> *)
let rec readFileByLines fd =
	try
		let l = input_line fd in
		l :: readFileByLines(fd)
	with _ -> [];;



(****************************************************)
(********************** ARBRE ***********************)
(****************************************************)

type Arbre = Noeud of (char * bool) * Arbre list;;


(* Ajoute un mot dans un arbre *)
(* addInTree : string * Arbre list -> Arbre list = <fun> *)
let rec addInTree = fun
("", arbre) -> arbre
| (mot, []) -> [Noeud((mot.[0], (string_length mot) = 1), addInTree(reste(mot), []))]
| (mot, Noeud((c, b), lst) :: suite) -> if mot.[0] = c
											then Noeud((c, b), addInTree(reste(mot), lst)) :: suite
										else Noeud((c, b), lst) :: addInTree(mot, suite);;


(* Créé un arbre à partir d'une liste de mot *)
(* createTree : string list * Arbre -> Arbre list = <fun> *)
let createTree = fun
(dico, Noeud((c, b), lst)) -> let rec aux = fun
								(mot :: reste, arbre) -> aux(reste, addInTree(mot, arbre))
								| (_, arbre) -> arbre
								in aux(dico, lst);;



(****************************************************)
(********************* DEBUGAGE *********************)
(****************************************************)

(* setString : string -> string = <fun> *)
let rec setString = fun s -> let rec aux = fun
							(s, 0) -> s
							| (s, n) -> aux(s, n-1) ^ " "
							in aux(s, 16 - (string_length s));;


(* DEBUGAGE Affiche la liste des résultats (n représente le nombre de collones) *)
(* displayStringList : string list * string * int -> unit = <fun> *)
let rec displayStringList = fun
(x :: l, s, n) -> let rec aux = fun
			(lst, 0, s, save) -> displayStringList(lst, s ^ "\n", save)
			| ([], _, s, save) -> displayStringList([], s ^ "\n", save)
			| ((x :: l), n, s, save) -> aux(l, n - 1, s ^ setString(x) ^ " ", save)
			in aux(x :: l, n, s, n)
| ([], s, _) -> print_string s;;


(* space : int -> string = <fun> *)
let rec space = fun
(-1) -> ""
| (0) -> " "
| (n) -> " " ^ space(n-1);;


(* DEBUGAGE Affiche l'arbre
   /!\ p représente le nombre d'espaces, mettre -1 lors de l'appel 
   Exemple: print_string (displayTree(arbre, -1)) *)
(* displayTree : Arbre list * int -> string = <fun> *)
let rec displayTree = fun
(Noeud((c, b), lst) :: suite, p) -> (space(p) ^ string_of_char(c) ^ "\n") ^ displayTree(lst, p+1) ^ displayTree(suite, p)
| ([], p) -> "";;



(****************************************************)
(****************** FONCTIONS BONUS *****************)
(****************************************************)

(* Divise la grille en une string et 2 listes
   (string contenant la grille, une liste avec les points et une autre avec les bonus) *)
let rec divideInformations = fun
((a, b, c) :: l, (d, e, f)) -> divideInformations(l, (d ^ a, e @ [b], f @ [c]))
| ([], triplet) -> triplet;;


(* Renvois la taille du mot trouvé *)
let rec numberLetter = fun
"" -> 0
| s -> if s.[0] = `.`
			then numberLetter(reste(s)) + 1
		else numberLetter(reste(s));;


(* Applique un bonus en fonction de la longueur du mot *)
let lengthBonus = fun len -> if len > 4
								then (len - 4) * 5
							else 0;;


(* Comptabilise le nombre de point en appliquant
   les bonus de lettre compte double/triple *)
let rec calculPoints = fun							
(s, px::pl, bx::bl) -> if s.[0] = `.`
								then calculPoints(reste(s), pl, bl) + (if bx = DL then px*2 else (if bx = TL then px*3 else px))
							else calculPoints(reste(s), pl, bl) + 0
| (_, _, _) -> 0;;


(* Applique les bonus de mot compte double/triple *)
let rec wordMult = fun
(s, 3::l, points, total) when s.[0] = `.` -> wordMult(reste(s), l, points, total + 2)
| (s, 4::l, points, total) when s.[0] = `.` -> wordMult(reste(s), l, points, total + 3)
| (s, x::l, points, total) -> wordMult(reste(s), l, points, total)
| (_, _, points, total) -> if total = 0 then points else points * total;;


(* Renvois le score d'un mot *)
let score = fun (s, p, b) -> wordMult(s, b, calculPoints(s, p, b), 0) + lengthBonus(numberLetter(s));;


(* Renvois le cumul de tous les points *)
let rec getFinalScore = fun lst -> let rec aux = fun
									([], lst, score) -> (lst, score)
									| ((mot, points) :: l, lst, score) -> aux(l, mot :: lst, score + points)
									in aux(lst, [], 0);;



(****************************************************)
(*********************** ALGO ***********************)
(****************************************************)

let rec insere = fun
((mot, point), (xmot, xpoint) :: l) -> if mot < xmot
											then (mot, point) :: (xmot, xpoint) :: l
										else if mot = xmot
													then if point > xpoint
															then (xmot, point) :: l
														else (xmot, xpoint) :: l
												else (xmot, xpoint) :: insere((mot, point), l)
| ((mot, point), []) -> [(mot, point)];;


(* Retire les mots en double en gardant ceux qui rapportent le plus de points *)
(* removeDuplicate : 'a list -> 'a list = <fun> *)
let removeDuplicate = fun ((mot, point) :: l) -> let rec aux = fun
												((mot, point) :: l, lst) -> aux(l, insere((mot, point), lst))
												| ([], lst) -> lst
												in aux((mot, point) :: l, [])
							| _ -> [];;


(* Remplace la lettre à l'index (i) par un (".") *)
(* removeLetter : string * int -> string = <fun> *)
let rec removeLetter = fun
(s, 0) -> "." ^ reste(s)
| (s, i) -> tetes(s) ^ removeLetter(reste(s), i - 1);;


(* Recherche dans un arbre une lettre.
   Raise Not_Found si la lettre n'est pas dans l'arbre *)
exception Not_Found;;
(* getBranche : char * Arbre list -> Arbre = <fun> *)
let rec getBranche = fun
(lettre, Noeud((c, b), lst) :: suite) -> if lettre = c
											then (Noeud((c, b), lst))
										 else getBranche(lettre, suite)
| (lettre, []) -> raise Not_Found;;


(* Gère les déplacements dans la grille
   Renvois la liste des déplacements possibles *)
(* canDo : int list * string -> int list = <fun> *)
let rec canDo = fun
(x :: l, g) -> if g.[x] != `.`
					then x :: canDo(l, g)
				else canDo(l, g)
| (_, _) -> [];;


(* Gère les déplacements dans la grille
   Renvois la liste des déplacements possibles *)
(* deplacement : int * string -> int list = <fun> *)
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


(* Coeur de l'algorithme
   Renvois tous les mots contenus dans la grille de jeu *)
(* start : int * string * Arbre list * int list * int list -> (string * int) list = <fun> *)
let rec start = fun
(16, g, arbre, points, bonus) -> []
| (i, g, arbre, points, bonus) -> start(i+1, g, arbre, points, bonus) @
									let rec search (x::l) g arbre mot =
										try
											let branche = getBranche(g.[x], arbre) and gnew = removeLetter(g, x) in
											let aux = fun
											(Noeud((c, b), lst), points, bonus) -> (if b then [((mot ^ string_of_char(c)), score(gnew, points, bonus))] else []) @ 
												let rec reSearch = fun
												((x :: l), g, arbre, mot) -> (search [x] g arbre mot) @ reSearch(l, g, arbre, mot)
												| ([], _, _, _) -> []
												in reSearch((l @ deplacement(x, g)), gnew, lst, (mot ^ string_of_char(c)))
											in aux(branche, points, bonus)
										with Not_Found -> if l = [] then [] else search l g arbre mot
									in search [i] g arbre "";;


(* solve : (string * int * int) list -> Arbre list -> string list * int = <fun> *)
let rec solve grille arbre = let (g, p, b) = divideInformations(grille, ("", [], [])) in
								getFinalScore(removeDuplicate(start(0, g, arbre, p, b)));;
