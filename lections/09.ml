

(* -------- espressioni come alberi n-ari -------- *)
type expr = 
  Int of int
| Var of string
| Diff of expr * expr
| Div of expr * expr
| Sum of expr list
| Mult of expr list

(* sumof, prodof: int list -> int *)
(* riportano, rispettivamente, la somma e il prodotto degli interi
 contenuti nella lista argomento *)
let rec sumof = function 
  [] -> 0 
| x::rest -> x+sumof rest

(* sumof iterativa *)
let sumof lst =
let rec aux result = function
    [] -> result
  | x::rest -> aux (x+result) rest
in aux 0 lst

(* oppure, usando fold_left: *)
let sumof  =
List.fold_left (+) 0 

let rec prodof = function 
  [] -> 1  
| x::rest -> x*prodof rest

(* oppure, usando fold_left: *)
let prodof  =
List.fold_left ( * ) 1

             
(* valutazione in un ambiente *)
type ambiente = (string * int) list

(* eval : ambiente -> expr -> int *)
(* eval env e = valore dell'espressione e nell'ambiente env.
 Errore se qualche variabile in e  non ha un valore
 associato in env *)
let rec eval env = function
   Int n -> n
|  Var x -> 
    (try List.assoc x env
    with Not_found -> failwith "eval")
|  Diff(e1,e2) -> (eval env e1) - (eval env e2)  
|  Div(e1,e2) -> (eval env e1) / (eval env e2)
|  Sum list -> sumof (List.map (eval env) list)
|  Mult list -> prodof (List.map (eval env) list)

(* ---------------------------- *)
(* slides => Alberi n-ari *)

type 'a ntree = Tr of 'a * 'a ntree list

(* Foglie: *)
(* leaf: 'a -> 'a ntree *)
(* leaf x = foglia etichettata da x *)
let leaf x = Tr(x,[])

(* Esempio: *)
let t = Tr(1,[Tr(2,[Tr(3,[leaf 4;
                        leaf 5]);
                  Tr(6,[leaf 7]);
                  leaf 8]);
            leaf 9;
            Tr(10,[Tr(11,[leaf 12;
                          leaf 13;
                          leaf 14]);
                   leaf 15;
                   Tr(16,[leaf 17;
                          Tr(18,[leaf 19;
                                 leaf 20])])])])

(* calcolo della dimensione di un albero n-ario *)
(* size : 'a ntree -> int *)
(* size t = numero di nodi di t *)
let rec size (Tr(_,tlist)) =
1 + sumof (List.map size tlist)

(* oppure, usando la mutua ricorsione: *)
(* size : 'a ntree -> int 
     size t = numero di nodi di t
 sumofsizes : 'a ntree list -> int 
     sumofsizes tlist = somma del numero dei nodi negli alberi
                        in tlist *)
let rec size (Tr(_,tlist)) =
1 + sumofsizes tlist
and sumofsizes = function
  [] -> 0
| t::rest -> (size t) + sumofsizes rest

(* Visita in preordine: *)
(* preorder : 'a ntree -> 'a list
 preorder t = lista dei nodi di t nell'ordine in cui sarebbero
 visitati secondo la visita in preordine *)
(* preorder_tlist : 'a ntree list -> 'a list  
 preorder_tlist [t1;...;tn] = (preorder t1) @ ... @ (preorder tn)
*)

let rec preorder (Tr(x,tlist)) =
  x::preorder_tlist tlist
and preorder_tlist = function
  [] -> []
| t::ts -> preorder t @ preorder_tlist ts

(* oppure: uso di map al posto della mutua ricorsione: *)
(* preorder : 'a ntree -> 'a list *)
let rec preorder (Tr(x,tlist)) =
  x::List.flatten (List.map preorder tlist)

(* uso di map per lavorare sugli alberi n-ari: quando si devono in ogni
 caso visitare tutti i sottoalberi *)

(* Altezza di un albero *)
(* maxl: 'a list -> 'a 
 maxl lst = elemento massimo di lst *)
let rec maxl = function
  [x] -> x
| x::xs -> max x (maxl xs)
| _ -> failwith "maxl"
  
(* height : 'a ntree -> int
 height t = altezza di t *)
let rec height (Tr(x,tlist)) = match tlist with
[] -> 0
| _ ->  1 + maxl (List.map height tlist)

(* versione con mutua ricorsione *)
(* hl: 'a ntree list -> int
  hl tlist = se tlist <>[] allora il massimo tra le h degli alberi in tlist,
                altrimenti errore *)
let rec h (Tr(x,tlist)) = match tlist with
[] -> 0
| _ -> 1+ hl tlist
and hl = function
  [] -> failwith "h"
| [t] -> h t
| t::rest -> max (h t) (hl rest)

(* Fattore di ramificazione *)
(* branching_factor :  'a ntree -> int *)
let rec branching_factor (Tr(x,tlist)) =
match tlist with
  [] -> 0
 | _ -> max (List.length tlist)
            (maxl (List.map branching_factor tlist))

(* Mutua ricorsione: quando non si deve necessariamente visitare
 tutto l'albero *)

(* Test di esistenza di un nodo in un albero *)
(* occurs_in : 'a ntree -> 'a -> bool
   occurs_in t x = true se x occorre in t
 occurs_in_tlist : 'a ntree list -> 'a -> bool 
   occurs_in_tlist tlist x = true se x occorre in almeno uno degli 
                             alberi in tlist *)
let rec occurs_in (Tr(x,tlist)) y = 
  x=y || occurs_in_tlist tlist y
and occurs_in_tlist tlist y = match tlist with
[] -> false
| t::ts -> occurs_in t y || occurs_in_tlist ts y

(* anche in questo caso comunque si puo' evitare la mutua
 ricorsione, ricorrendo a List.exists: *)
let rec occurs_in (Tr(x,tlist)) y = 
  x=y || List.exists (fun t -> occurs_in t y) tlist

(* verificare se un nodo e' discendente di un altro nodo *)
(* descend : 'a ntree -> 'a -> 'a -> bool 
     descend t ant dis = dis e' un discendente di ant in t 
 descend_tlist : 'a ntree list -> 'a -> 'a -> bool 
     descend_tlist tlist y z = z e' un discendente di y
                   in almeno uno degli alberi in tlist *)
let rec descend (Tr(x,tlist)) y z =
  if x=y then occurs_in_tlist tlist z
  else descend_tlist tlist y z
and descend_tlist tlist y z = match tlist with
[] -> false
| t::ts -> descend t y z || descend_tlist ts y z

(*** Ricerca di un cammino dalla radice dell'albero a una FOGLIA 
   etichettata da un valore dato ***)
(* backtracking *)

exception NotFound;;

(* path : 'a ntree -> 'a -> 'a list 
   path t y = cammino fino a una foglia etichettata da y in t
 path_tlist : 'a ntree list -> 'a -> 'a list 
   path_tlist tlist y = cammino fino a una foglia etichettata da y in
                        uno degli alberi in tlist *)
let rec path (Tr(x,tlist)) y =
match tlist with
  [] -> if x=y then [x] else raise NotFound
| _ -> x::path_tlist tlist y
and path_tlist tlist y = match tlist with
[] -> raise NotFound
| t::ts -> try path t y 
         with NotFound -> path_tlist ts y

(* versione alternativa, con una funzione ausiliaria che
 ha come argomento una lista di alberi.
    aux: 'a ntree list -> 'a list
 aux tlist = cammino desiderato in uno degli alberi in tlist.
 Inizialmente la lista contiene soltanto l'albero iniziale.
 Ad ogni chiamata di aux, la lista di alberi corrisponde a un
 insieme di sottoalberi di uno stesso nodo.
*)

(* path: 'a ntree -> 'a -> 'a list *)
(* aux: 'a ntree list -> 'a list *)
let path t y =
let rec aux = function
    [] -> raise NotFound
  | Tr(x,[])::rest -> 
                      if x=y then [x] 
                      else aux rest
  | Tr(x,tlist)::rest ->
        try x::aux tlist
        with NotFound -> aux rest
in aux [t]

(* slides => alberi di minimax *)
(* Esame di luglio 2013 **)

type player = Min | Max

type minmaxtree = Leaf of int
| Node of (player * int) * minmaxtree list

(* minlist: 'a list -> 'a
 minlist lst = elemento minimo in lst *)
let rec minlist = function
  [] -> failwith "minlist"
| [x] -> x
| x::y::rest -> minlist ((min x y)::rest)

(* maxlist: 'a list -> 'a
 maxlist lst = elemento massimo in lst *)
let rec maxlist = function
  [] -> failwith "maxlist"
| [x] -> x
| x::y::rest -> maxlist ((max x y)::rest)

(* value: minmaxtree -> int
 value t = valore numerico della radice di t *)
let value = function
  Leaf n -> n
| Node((_,n),_) -> n

(* propagate: minmaxtree -> minmaxtree
 propagate t = albero ottenuto da t in modo che i valori
               numerici dei nodi intermedi siano ben assegnati *)
let rec propagate = function
  Leaf n -> Leaf n
| Node((p,_),tlist) ->
    let subtrees = List.map propagate tlist in
    let v = (if p=Min then minlist else maxlist)
        (List.map value subtrees)
    in Node((p,v),subtrees)

(* minmaxtree di esempio *)
let tree =
Node((Max,0),
     [Node((Min,100),[Leaf 3;Leaf 12;Leaf 8]);
Node((Min,0),[Leaf 2;Leaf 4;Leaf 6]);
Node((Min,30),[Leaf 14;Leaf 15;Leaf 10])])