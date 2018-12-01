
(* SOURCE : https://caml.inria.fr/pub/docs/manual-caml-light/node15.8.html *)

#open "hashtbl";;


(* recher à l'élément "elem" de la table et renvoit toutes ses valeurs sous forme de liste *)
let getFromTable = fun (table, elem) -> try
						find_all table elem
					with Not_found -> [];;


(* Affiche le contenu d'une table /!\ A utiliser avec do_table *)
let display = fun s v -> print_string("Key: " ^ s ^ " \tValue: " ^ v ^ "\n");;


(*************************************************************)

let table = new 1;; (* La veleur après le new n'a pas d'importance *)

add table "key1" "val1";;
add table "key1" "val2";;
add table "key2" "val3";;
add table "key2" "val4";;
add table "key1" "val5";;

do_table display table;;

getFromTable(table, "key1");;
