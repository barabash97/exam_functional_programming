
(*
#load "language.cmo";;

#load "parser.cmo";;
#load "lexer.cmo";;
*)

open Language

let f1 = Or(Not(Prop "p"),Prop "q");;
let f2 = Not(Imp(Prop "p",Prop "q"));;
let f3 = And(f1,f2);;
let f4 = Or(f3,And(Prop "r", Prop "s"));;
let f5 = Not(Imp(Not(Prop "p"),Prop "q"));;
let f6 = Not(Or(Prop "p",Not(Prop "q")));;
let f7 = Not(And(f5,f6))

(* f2s : form -> string *)
(* f2s f = stringa che rappresenta la formula f *)
(* per stampare la formula f:
    print_string ((f2s f7)^"\n") *)
let rec f2s = function
    True -> "T"
  | False -> "F"
  | Prop p -> p
  | Not f -> "-"^(f2s f)
  | And(f,g) -> String.concat "" ["(";f2s f;"&"; f2s g;")"]
  | Or(f,g) -> String.concat "" ["(";f2s f;"|"; f2s g;")"]
  | Imp(f,g) -> String.concat "" ["(";f2s f;"->"; f2s g;")"]
;;
let f1 = Or(Not(Prop "p"),Prop "q");;
let f2 = Not(Imp(Prop "p",Prop "q"));;
let f3 = And(f1,f2);;
let f4 = Or(f3,And(Prop "r", Prop "s"));;
let f5 = Not(Imp(Not(Prop "p"),Prop "q"));;
let f6 = Not(Or(Prop "p",Not(Prop "q")));;
let f7 = Not(And(f5,f6))

(* f2s : form -> string *)
(* f2s f = stringa che rappresenta la formula f *)
(* per stampare la formula f:
    print_string ((f2s f7)^"\n") *)
let rec f2s = function
    True -> "T"
  | False -> "F"
  | Prop p -> p
  | Not f -> "-"^(f2s f)
  | And(f,g) -> String.concat "" ["(";f2s f;"&"; f2s g;")"]
  | Or(f,g) -> String.concat "" ["(";f2s f;"|"; f2s g;")"]
  | Imp(f,g) -> String.concat "" ["(";f2s f;"->"; f2s g;")"]
;;


(*            ocamlyacc, ocamllex                 *)
(* =========== parsing di formule ============ *)
(* negazione: -
   congiunzione: &
   disgiunzione: |
   implicazione: => oppure -> 
   doppia implicazione: <=> oppure <->
   True: T
   False: F
   atomi: iniziano con un carattere alfabetico, eventualmente
          seguito da uno o piu' caratteri alfanumerici e '_' 
*)


(* string -> Language.form *)
(* solleva Parsing.parse_error in caso di fallimento *)
(* vedi modulo Parsing della libreria standard *)
let parse stringa =
  let lexbuf = Lexing.from_string stringa
  in Parser.formula Lexer.token lexbuf

(* lettura di una formula da tastiera: *)
(* read: unit -> formula *)
let read() =
  try parse (read_line())
  with Parsing.Parse_error -> 
    print_string "Syntax error\n";
    False

(* lettura di una formula, se il parser fallisce, viene richiesto un
   altro input *)
let rec read2() =
  try parse (read_line())
  with Parsing.Parse_error -> 
    print_string "Syntax error\n";
    read2()

(* esempi:
# let f=read();;
-p=>(pippo|-(pluto=>paperino))
val f : Language.form =
  Imp (Not (Prop "p"),
   Or (Prop "pippo", Not (Imp (Prop "pluto", Prop "paperino"))))
# let g=read();;
A<=>B
val g : Language.form =
  And (Imp (Prop "A", Prop "B"), Imp (Prop "B", Prop "A"))
# f2s g;;
- : string = "((A->B)&(B->A))"
*)

(* ================================================ *)
(* slides => compilazione separata e codice eseguibile *)
(* ================================================ *)

type interpretation = string list

(* models: form -> interpretation -> bool *)
(* models f emme = emme rappresenta un modello di f *)
let rec models f emme = 
  match f with
    True -> true
  | False -> false
  | Prop name -> List.mem name emme
  | Not f1 -> not(models f1 emme)
  | And(f1,f2) -> models f1 emme && models f2 emme 
  | Or(f1,f2) -> models f1 emme || models f2 emme 
  | Imp(f1,f2) -> not(models f1 emme) || models f2 emme

(* setadd : 'a -> 'a list -> 'a list *)
(* aggiunta insiemistica *)
let setadd x xs 
    = if List.mem x xs then xs else x::xs

(* union : 'a list -> 'a list -> 'a list *)
(* unione insiemistica *)
let rec union set = function
    [] -> set
  | x::rest -> setadd x (union set rest)
        
(* atomlist : form -> string list *)
(* atomlist f = lista senza ripetizioni dei nomi degli atomi in f *)
let rec atomlist = function
    True | False -> []
  | Prop s -> [s]
  | Not f -> atomlist f
  | Or(f1,f2) -> union (atomlist f1) (atomlist f2)
  | And(f1,f2) -> union (atomlist f1) (atomlist f2)
  | Imp(f1,f2) -> union (atomlist f1) (atomlist f2)

(* cons : 'a -> 'a list -> 'a list *)
(* inserimento in testa (List.cons) *)
let cons x xs = x::xs;;
 
(* powerset : 'a list -> 'a list list *)  
(* powerset s = insieme potenza di s *) 
let rec powerset = function
    [] -> [[]]
  | p::props -> 
       let all_ints = powerset props
       in all_ints @ List.map (cons p) all_ints

(* valid : form -> bool *)
(* valid f = f e' valida *)
let valid f = 
  List.for_all (models f) (powerset (atomlist f));;
 
(* truthtable : form -> (interpretation * bool) list *)
(* truthtable f = rappresentazione della tabella di verita` di f *)
let truthtable f =
  let mkrow emme = (emme, models f emme)
  in List.map mkrow (powerset (atomlist f))

(* o anche *)
let truthtable f =
  List.map (function emme -> (emme, models f emme))
     (powerset (atomlist f))

(*
# truthtable (read());;
p & q | p => q.
- : (string list * bool) list =
[([], true); (["q"], true); (["p"], false); (["p"; "q"], true)]
*)

(* ========= slides *)

(* valid_tt : form -> bool *)
(* valid_tt f = f e' valida, utilizzando le tavole di verita` *)
let valid_tt f =
   List.for_all 
     (function (_,value) -> value) (truthtable f)

(* # valid_tt (read());;
p | -p.
- : bool = true
*)

(* non : ('a -> bool) -> 'a -> bool *)
let non p x = not(p x)
(* contradiction : form -> bool *)
(* contradiction f = f e' una contraddizione *)
let contradiction f = 
  List.for_all (non (models f)) (powerset (atomlist f));;

(* sat : form -> bool *)
(* sat f = f e' soddisfacibile *)
let sat f =
  List.exists (models f) (powerset (atomlist f))

(* logequiv: form -> form -> bool *)
(* logequiv f g = f e g sono logicamente equivalenti *)
let logequiv f g =
  valid (And(Imp(f,g),Imp(g,f)))

(** oppure **)
let logequiv f g =
  List.for_all
    (function emme -> models f emme = models g emme)
    (powerset (union (atomlist f)(atomlist g)))

(* mkand: form list -> form *)
(* mkand [f1;...;fk] = And(f1,And(f2,....And(...,fk))) *)
let rec mkand = function
    [] -> True
  | [f] -> f
  | f::rest -> And(f,mkand rest)

(* consequence: form list -> form -> bool *)
(* consequence set f = f e' una conseguenza logica di set *)
let consequence set f =
  valid (Imp(mkand set,f))

(*** forma normale negativa ***)
  (* fnn: form -> form *)
(* fnn f = forma normale negativa di f *)
let rec fnn = function
  | And(f,g) -> And(fnn f,fnn g)
  | Or(f,g) -> Or(fnn f,fnn g)
  | Imp(f,g) -> fnn(Or(Not f,g))
  | Not(And(f,g)) -> Or(fnn(Not f),fnn(Not g))
  | Not(Or(f,g))  -> And(fnn(Not f),fnn(Not g))
  | Not(Imp(f,g)) -> And(fnn f,fnn(Not g))
  | Not(Not f) -> fnn f
  | f -> f

(* FINE *)

(* per chi vuole: trasformazione in CNF e tableaux *)
(* cnf : form -> form *)
(* cnf f = una forma normale congiuntiva di f *)
let rec cnf f = 
  let rec distrib  = function
      (* applicazione delle distributive *)
      Or(f,g) ->
	(match (distrib f,distrib g) with 
	  (And(f1,g1),h) | (h,And(f1,g1)) -> 
	    distrib (And(Or(h,f1), Or(h,g1)))
	| (f1,h) -> Or(f1,h))
       (* ricorsione generale *)
    | And(f,g) -> And(distrib f,distrib g)
    | f -> f
  in distrib(fnn f)

(* tableaux *)
(* alpha : form -> form * form *)
(* se f e' un'alpha-formula: alpha f = (f1,f2) dove f1 e f2 sono
   le "componenti" di f *)
let alpha = function
    And(f,g) -> (f,g)
  | Not(Or(f,g)) -> (Not f,Not g)
  | Not(Imp(f,g)) -> (f,Not g)
  | _ -> failwith "alpha"

(* val beta : form -> form * form *)
(* se f e' una beta-formula: alpha f = (f1,f2) dove f1 e f2 sono
   le "componenti" di f *)
let beta = function
    Or(f,g) -> (f,g)
  | Not(And(f,g)) -> (Not f,Not g)
  | Imp(f,g) -> (Not f,g)
  | _ -> failwith "beta"

(* complement : form -> form *)
(* complement f = complemento di f (se f e' un letterale) *)
let complement = function
    Prop p -> Not(Prop p)
  | Not(Prop p) -> Prop p
  | _ -> failwith "complement"

(* valid : form -> bool *)
(* valid f = f e' valida, mediante la costruzione di un tableau per f *)
let valid_tab f =
  let rec closed pending lits = 
    match pending with
      [] -> false
    |  f::rest ->  match f with
        | True | Not False -> closed rest lits
        | False | Not True -> true
        | Prop _ | Not(Prop _) ->
            if List.mem f lits then closed rest lits
            else List.mem (complement f) lits ||
              closed rest (f::lits)
        | Not(Not f) -> closed (f::rest) lits
	| And(_,_) | Not(Or(_,_)) | Not(Imp(_,_)) ->
	    let (f1,f2)= alpha f in
	    closed (f1::f2::rest) lits
	| Or(_,_) | Not(And(_,_)) | Imp(_,_) -> 
	    let (f1,f2) = beta f in
            closed (f1::rest) lits && closed (f2::rest) lits
  in closed [Not f] [];;


(* controllo di soddisfacibilita' di un insieme di formule mediante
   il metodo dei tableaux *)

exception NotSat;;

(* sat_tab: form list -> form list *)
(* sat_tab formlist = formlist e' un insieme soddisfacibile *)
let sat_tab formlist =
  let rec aux pending lits = 
    match pending with
      [] -> lits
    | f::rest ->
        match f with
          True | Not False -> aux rest lits
        | False | Not True -> raise NotSat
        | Prop _ | Not(Prop _) ->
            if List.mem f lits then aux rest lits
            else if List.mem (complement f) lits  then raise NotSat
            else aux rest (f::lits)
        | Not(Not f) -> aux (f::rest) lits
	| And(_,_) | Not(Or(_,_)) | Not(Imp(_,_)) ->
	    let (f1,f2)= alpha f in
	    aux (f1::f2::rest) lits
	| Or(_,_) | Not(And(_,_)) | Imp(_,_) -> 
	    let (f1,f2) = beta f in
            try aux (f1::rest) lits 
	    with NotSat -> aux (f2::rest) lits
   in aux formlist []


(* all_models: form list -> form list list *)
(* all_models formlist = lista con tutti i modelli di formlist *)
let all_models formlist =
  let rec aux pending lits = 
    match pending with
      [] -> [lits]
    | f::rest ->
        match f with
          True | Not False -> aux rest lits
        | False | Not True -> []
        | Prop _ | Not(Prop _) ->
            if List.mem f lits then aux rest lits
            else if List.mem (complement f) lits  then []
            else aux rest (f::lits)
        | Not(Not f) -> aux (f::rest) lits
	| And(_,_) | Not(Or(_,_)) | Not(Imp(_,_)) ->
	    let (f1,f2)= alpha f in
	    aux (f1::f2::rest) lits
	| Or(_,_) | Not(And(_,_)) | Imp(_,_) -> 
	    let (f1,f2) = beta f in
            (aux (f1::rest) lits) @ (aux (f2::rest) lits)
   in aux formlist []



(* forma normale disgiuntiva, con il metodo dei tableaux *)
(* mkdnf : form form list -> form *)
(* mkdnf [ramo1;...;ramok] = disgiunzione delle congiunzioni
   di letterali nei rami aperti di un tableau *)
let rec mkdnf = function
    [] -> False (* se non ci sono rami aperti, la formula
                   e' contraddittoria *)
  | [branch] -> mkand branch
  | branch::rest ->
      Or(mkand branch,mkdnf rest)

(* dnf_tab: form -> form *)
(* dnf_tab f = forma normale disgiuntiva di f *)
let dnf_tab f =
  mkdnf(all_models [f])

