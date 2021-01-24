
(* slides *)

type direzione = Su | Giu | Destra | Sinistra;;

type seme = Bastoni | Coppe | Denari | Spade;;

type valore = Asso | Due | Tre | Quattro | Cinque
            | Sei | Sette | Fante | Cavallo | Re;;

(* valore : valore -> int *)
let valore = function
     Asso -> 1
  |  Due -> 2
  |  Tre -> 3
  |  Quattro -> 4
  |  Cinque -> 5
  |  Sei -> 6
  |  Sette -> 7
  |  Fante -> 8
  |  Cavallo -> 9
  |  Re -> 10;;

(* slides => rappresentazione di posizioni e movimenti sul piano *)

(* rappresentazione delle posizioni: punto + direzione *)
(* Possiamo rappresentare le posizioni mediante triple *)
type posizione = Pos of int * int * direzione
(* definizione di un nuovo tipo, con costruttore Pos, che, applicato a una
   tripla di tipo int * int * direzione, costruisce una posizione *)

type posizione = int * int * direzione
(* dichiarazione type che NON introduce un nuovo tipo,
   ma da` un altro nome a un tipo gia` esistente *)

(* selettori del tipo posizione (definiti mediante pattern matching) *)
(* xcoord : posizione -> int *)
let xcoord (x,_,_) = x
(* qual e` la risposta di OCaml? *)

(* ycoord : posizione -> int *)
let ycoord (_,y,_) = y

(* dir : posizione -> direzione *)
let dir (_,_,d) = d

(* slides => *)
(* rappresentazione delle azioni:
   - girare di 90 gradi in senso orario
   - andare avanti di n passi *)
type azione =
    Gira
  | Avanti of int;;

(* sottoproblemi per risolvere il problema dello spostamento sul piano *)
(* gira : direzione -> direzione *)
(* gira dir = direzione che si ottiene eseguendo l'azione Gira *)
(**       attenzione alle maiuscole/minuscole      **)
let gira = function
    Su -> Destra
  | Giu -> Sinistra
  | Destra -> Giu
  | Sinistra -> Su

(* avanti : posizione -> int -> posizione *)
(* avanti pos n = posizione che si ottiene eseguendo l'azione 
   Avanti n *)
let avanti (x,y,dir) n =
  match dir with
    Su -> (x,y+n,dir)
  | Giu -> (x,y-n,dir)
  | Destra -> (x+n,y,dir)
  | Sinistra -> (x-n,y,dir)

(* problema principale: data la pozione pos e l'azione act, 
   qual e` la posizione che si ottiene eseguendo l'azione 
   act a partire dalla posizione pos? *)
(* sposta : posizione -> azione -> posizione *)
let sposta (x,y,dir) act =
  match act with
    Gira -> (x,y,gira dir)
              (* le coordinate non cambiano,
                 la direzione gira di 90 gradi in senso orario *)
  | Avanti n -> avanti (x,y,dir) n
  
(* ------------------------------ *)
(* slides => Unione di tipi *)

type number = Int of int
            | Float of float

(* sum : number * number -> number *)
let sum = function
    (Int x,Int y) -> Int (x + y)
  | (Int x,Float y) -> Float ((float x) +. y)
  | (Float x,Float y) -> Float (x +. y)
  | (Float x,Int y) -> Float(x +. float y)

(* ------------------------------ *)
(* slides => Tipi definiti ricorsivamente *)
type nat = Zero | Succ of nat;;

(* int_of_nat : nat -> int *)
(* int_of_nat n = valore intero corrispondente a n *)
let rec int_of_nat = function
    Zero -> 0
  | Succ n -> succ(int_of_nat n)

(* nat_of_int: int -> nat *)
let nat_of_int n =
  if n<0 then failwith "Nat_of_int"
  else 
    let rec aux = function
	0 -> Zero
      | n -> Succ (aux (n-1))
    in aux n

(* somma : nat -> nat -> nat *)
let rec somma n m = 
  match n with
     Zero -> m
   | Succ k -> Succ(somma k m)

(*  slides => definizione di Costruttori di tipi *)
(* liste, matrici *)
type 'a mylist = Nil | Cons of 'a * 'a mylist

type 'a matrix = Mat of 'a list list

(* il tipo 'a option *)
type 'a option = None | Some of 'a

(* new_assoc : ('a * 'b) list -> 'a -> 'b option *)
let rec new_assoc lst x = match lst with
    [] -> None
  | (k,v)::rest -> if x=k then Some v
                   else new_assoc rest x

let alist = [(0,"pippo"); (1,"pluto"); (2,"paperino")]

(*
# new_assoc alist 2;;                      
- : string option = Some "paperino"            
# new_assoc alist 3;;
- : string option = None

#  new_assoc alist;;
- : int -> string option = <fun>

# List.map (new_assoc alist) [1;5;3;0;4];;
- : string option list = [Some "pluto"; None; None; Some "pippo"; None]
*)

(*  valore : 'a option -> 'a *)
(* valore x : se x=Some n riporta n, altrimenti errore *)
let valore = function
    Some n -> n
  | None -> failwith "valore"

(* ottenere la lista degli elementi associati a una lista di chiavi,
   ignorando quelle che non hanno valore *)
(*  assoc_all : ('a * 'b) list -> 'a list -> 'b list *)
let assoc_all lista chiavi =
  List.map valore
    (List.filter ((<>) None)
       (List.map (new_assoc lista) chiavi))

let chiavi = [1;5;3;0;5]

(*
# assoc_all alist chiavi;;
- : string list = ["pluto"; "pippo"]
*)