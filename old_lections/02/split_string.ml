(* split_string s = (n,op,m);; *)

(* primo_non_numerico: string -> int che è la posizione  *)
(* substring: string -> int -> int -> string;; *)
(* int_of_string: string -> int *)

(* String.sub : string -> int -> int -> string *)

(* String.length: string -> int *)

(* INIZIAMO A RISOLVERE IL PROBLEMA 
    PARTIAMO DAL SOTTO PROBLEMA PER TROVARE LA POSIZIONE NELLA STRINGA DELL'ELEMENTO CHE INDICA OPERAZIONE ALGEBRICA.

*)

let numeric c = 
  c >= '0' && c<= '9';;

let rec loop s i = 
  if not(numeric s.[i]) then i
  else loop s (i+1);;

(* let primo_non_numerico s = loop s 0;; *)

let primo_non_numerico = loop s 0;

let split_string s =
  let i = primo_non_numerico s 
  in (int_of_string(String.sub s 0 (i-1))),
  s.[i],
  int_of_string(String.sub s, (i+1), ((String.length s) - 1)));;


