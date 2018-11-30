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
let rec display = fun
(x :: l) -> print_string(x ^ "\n"); display(l)
|[] -> print_string "\n";;


(******************** TRI CHAINE ********************)

let rec insere = fun
(c, "") -> c ^ ""
| (c, s) -> if c <= tetes(s)
				then c ^ tetes(s) ^ reste(s)
			else
				tetes(s) ^ insere(c, reste(s));;

(* Tri une chaine *)
let rec tri_insere = fun
"" -> ""
|s -> insere(tetes(s), tri_insere(reste(s)));;




let file = "C:\Users\adrie\Desktop\Projet\dico.txt";;

let fd = open_in(file);;

let fileContent = readFileByLines(fd);;

let proposition = "HDFETIQAYOPDNCTE";;

type arbre = Feuille of string | Noeud of string*arbre*arbre;;






