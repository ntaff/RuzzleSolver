(****************************************************)
(***************PROJET RUZZLE SOLVER*****************)
(************NICOLAS TAFFOUREAU (@ntaff)*************)
(************ADRIEN TREILHOU (@AdrienFAC)************)
(*******************VERSION 3************************)
(****************************************************)


(********************** CHAINE **********************)

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


(* DEBUGAGE Affiche une liste de string *)
(* string list -> unit *)
let rec displayStringList = fun
(x :: l) -> print_string(x ^ "\n");
			displayStringList(l)
|[] -> print_string "\n";;

	
(********************* ARBRE ************************)

(* On crée un arbre contenant des Noeuds de couples de char et bool ou des listes d'arbres  *)
type Arbre = Noeud of (char * bool) * Arbre list;;

(* On ajoute les noeuds à l'arbre *)
let rec addInTree = fun
("", arbre) -> arbre
| (mot, []) -> [Noeud((mot.[0], (string_length mot) = 1), addInTree(reste(mot), []))]
| (mot, Noeud((c, b), lst) :: suite) -> if mot.[0] = c
											then Noeud((c, b), addInTree(reste(mot), lst)) :: suite
										else Noeud((c, b), lst) :: addInTree(mot, suite);;


(* On crée l'arbre à partir du dictionnaire *)
let rec createTree = fun
(mot :: reste, arbre) -> createTree(reste, addInTree(mot, arbre))
| (_, arbre) -> arbre;;


(*********************** JEU DE TESTS *****************************)

let file = "C:\Users\Taffoureau\Documents\dico.txt";;

let fd = open_in(file);;

let fileContent = readFileByLines(fd);;

close_in fd;;

let time = Sys__time();;

let arbre = createTree(fileContent, [Noeud((` `, false), [])]);;

Sys__time() -. time;; (* 0.25sec *)
