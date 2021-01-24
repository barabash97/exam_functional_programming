
(* slides => *)
(* rappresentazione di grafi orientati mediante liste di successori *)
type 'a graph = ('a * 'a list) list
let grafo = [(1,[2;3;4]); (2,[6]); (3,[5]); (4,[6]); 
             (5,[4]); (6,[5;7]); (7,[])]

exception Nodo_inesistente
(* successori: 'a  -> 'a graph -> 'a list *)
(* successori x g = successori del nodo x nel grafo g *)
let successori x grafo =
  try List.assoc x grafo
  with Not_found -> raise Nodo_inesistente

(* Rappresentazione mediante lista di archi *)
type 'a graph = ('a * 'a) list
let grafo = [(1,2); (1,3); (1,4); (2,6); (3,5); (4,6);
             (5,4); (6,5); (6,7)]

(* successori: se viene utilizzata per la visita di un grafo a partire
   da un nodo dato, non e` necessario controllare se i nodi appartengono
   al grafo o no: si partira' dal nodo iniziale seguendo i successori *)

(** successori in un grafo orientato **)
(* successori : 'a -> 'a graph -> 'a list  *)
(* successori x g = lista dei successori di x in g (orientato) *)
let rec successori nodo = function 
      [] -> []
    | (x,y)::rest ->
        if x = nodo then y::successori nodo rest
        else successori nodo rest 
(* oppure *)
let successori nodo grafo =
    List.map snd (List.filter (function (x,_) -> x=nodo) grafo)

(** vicini in un grafo non orientato **)
(*  vicini : 'a -> 'a graph -> 'a list  *)
(* vicini x g = lista dei vicini di x in g (non orientato) *)
let rec vicini nodo = function 
    [] -> []
  | (x,y)::rest ->
      if x = nodo then y::vicini nodo rest
      else if y = nodo then x::vicini nodo rest
      else vicini nodo rest

(** se serve ottenere la lista dei nodi (non isolati): **)
(* setadd: 'a -> 'a list -> 'a list *)
(* setadd x lst = aggiunge x a lst, se non c'e' gia' *)
let setadd x set = 
  if List.mem x set then set else x::set

(* nodes : 'a graph -> 'a list *)
(* nodes g = lista dei nodi del grafo g *)
let rec nodes = function
    [] -> []
  | (x,y)::rest ->
      setadd x (setadd y (nodes rest))

(*********** ALGORITMI DI BASE SUI GRAFI *************)

(** VISITA DI GRAFI: in profondita` e in ampiezza **)
(** attenzione ai cicli! **)
(* in profondita`: i nodi in attesa di essere visitati sono
                   gestiti come una pila
   in ampiezza: i nodi in attesa di essere visitati sono
                gestiti come una coda
Visita per collezionare i nodi visitati (grafo orientato): *)

(** visita in profondita` (grafo orientato) **)
(* depth_first_collect : 'a graph -> 'a -> 'a list *)
(* depth_first_collect g x = lista dei nodi raggiungibili da x in g *)
(* search: 'a list -> 'a list -> 'a list *)
(* search visited pending = 
         (nodi raggiungibili da qualche nodo in pending mediante
          un cammino che non passa per visited) @ visited *)
let depth_first_collect graph start =
    let rec search visited = function
        [] -> visited
      | n::rest -> 
            if List.mem n visited 
            then search visited rest 
            else search (n::visited) 
                        ((successori n graph) @ rest)
                            (** oppure: (vicini n graph) @ rest)
                                se il grafo non e' orientato **)
                       (* i nuovi nodi sono inseriti in testa *)
    in search [] [start]

(** Esempio: visita in profondita` per verificare se tutti i nodi 
    raggiungibili da start soddisfano un predicato p.  (grafo orientato) **)
(* conviene utilizzare List.for_all applicata al risultato di
   depth_first_collect? *)
(* depth_first_all: 'a graph -> 'a -> ('a -> bool) -> bool *)
(* detph_first_all g x p = true se tutti i nodi raggiungibili da x in g
                           soddisfano p *)
(* search visited pending = true sse tutti i nodi raggiungibili da nodi in pending
                                          esclusi quelli in visited soddisfano p *)
let depth_first_all graph start p =
    let rec search visited = function
        [] -> true
      | n::rest -> (* i nodi visitati sono gia` stati controllati *)
            if List.mem n visited then search visited rest 
	    else p n && search (n::visited) ((successori n graph) @ rest)
                  (** oppure: vicini n graph se il grafo non e' orientato **)
     in search [] [start]

(** visita in ampiezza (grafo orientato) **)
(* breadth_first_collect : 'a graph -> 'a -> 'a list *)
(* breadth_first_collect g x = lista dei nodi raggiungibili da x in g *)
(* search: 'a list -> 'a list -> 'a list *)
(* search visited pending = 
         (nodi raggiungibili da qualche nodo in pending mediante
          un cammino che non passa per visited) @ visited *)
let breadth_first_collect graph start =
    let rec search visited = function
        [] -> visited
      | n::rest -> 
            if List.mem n visited 
            then search visited rest 
            else search (n::visited) 
                        (rest @ (successori n graph))
                  (** oppure: vicini n graph se il grafo non e' orientato **)
                            (* i nuovi nodi sono inseriti
                               in coda *)
    in search [] [start]

(** Esempio: cercare se dal nodo start e' raggiungibile un nodo che
    soddisfa il predicato p. Riportare tale nodo, se la ricerca
    ha successo (grafo orientato) **)
(* search_node: 'a graph -> 'a -> ('a -> bool) -> 'a *)
(* search_node g x p = nodo che soddisfa p e raggiungibile da x in g *)
(* search: 'a list -> 'a list -> 'a *)
(* search visited pending = nodo che soddisfa p, raggiungibile da
         uno dei nodi in pending senza passare per nodi in visited *)
exception NotFound

let search_node graph start p =
    let rec search visited = function
        [] -> raise NotFound
      | n::rest -> 
            if List.mem n visited 
            then search visited rest 
	    else if p n then n
                 else search (n::visited) 
                             ((successori n graph) @ rest)
                  (** oppure: vicini n graph se il grafo non e' orientato **)
    in search [] [start]


(** Quali sono i vantaggi e gli svantaggi dei due algoritmi di visita? **)

(** RICERCA DI UN CAMMINO mediante backtracking **)
(* versione generale: ricerca di un cammino in un grafo, a partire
    dal nodo start fino a un nodo che soddisfa il predicato p.
    Viene riportata una lista che rappresenta il cammino, o 
    sollevata un'eccezione (grafo non orientato) *)

exception NotFound

(* search_path : 'a graph -> 'a -> ('a -> bool) -> 'a list *)
(* search_path g x p = cammino in g da x fino a un nodo che soddisfa p *)
(* funzioni ausiliarie: *)
   (* ricerca a partire da un singolo nodo *)
   (* from_node: 'a list -> 'a -> 'a list 
      from_node visited a = cammino che non passa per nodi in visited,
                            da a fino a un nodo che soddisfa p *) 
   (* ricerca a partire da una lista di nodi, tutti vicini di uno
      stesso nodo *)
   (* from_list: 'a list -> 'a list -> 'a list
      from_list visited nodes = cammino che non passa per nodi in visited,
                     e che parte da un nodo in nodes e arriva a un nodo che
                     soddisfa p *)
let search_path graph start p =
  let rec from_node visited a =
    if List.mem a visited 
    then raise NotFound
    else if p a then [a]   (* il cammino e' trovato *)
         else a::from_list (a::visited) (vicini a graph)
  and from_list visited = function
      [] -> raise NotFound
    | a::rest -> (* provo a passare per a, ma se fallisco
                    cerco ancora passando per uno dei nodi 
		    di rest *)        
	try from_node visited a 
        with NotFound -> from_list visited rest
   in from_node [] start

(* implementazione alternativa, senza mutua ricorsione: *)
(* gpath:  'a graph -> 'a -> ('a -> bool) -> 'a list *)
(* funzione ausiliaria:
       aux:  'a list -> 'a list -> 'a list
       aux visited nodes = cammino che non passa per nodi in visited,
                     e che parte da un nodo in nodes e arriva a un nodo che
                     soddisfa p *)
let gpath g start p =
  let rec aux visited = function
      [] -> raise NotFound
    | x::rest -> if List.mem x visited 
                 then aux visited rest
                 else if p x then [x]
                      else try x:: aux (x::visited) (vicini x g)
                           (* ricerca a partire dai vicini di x *)
                           with NotFound -> 
                                 aux visited rest
                         (* ricerca a partire dai "fratelli" di x *)
  in aux [] [start]

(* ATTENZIONE: a differenza che negli algoritmi di visita in cui non 
   si riporta un cammino, qui 
   non si mescolano i vicini di un nodo con i suoi fratelli *)

(* guardate anche le dispense *)

(** ------------------------------------------------ **)
(* slides => labirinto *)

let grafo = [(1,2);(1,3);(1,4);(2,3);(2,4);(2,7);
             (3,5);(4,6);(4,7);(5,7);(6,7)]
(* nel caso del labirinto gli archi non sono orientati *)

(** Problema: attraversamento di un labirinto, da una casella
   di entrata a una casella di uscita, senza passare per caselle
   che contengono un mostro, e raccogliendo gli oggetti che
   si trovano nelle altre caselle (se ce ne sono). 

   Si deve riportare un cammino (quindi ricerca di un cammino mediante
   backtracking), ma non si puo' passare per caselle con il mostro. E
   si devono raccogliere gli oggetti. *)

(** un labirinto e' un grafo i cui nodi hanno dei contenuti **)
(* Contenuti possibili: mostro o oggetti *)
type content = Mostro | Obj of string

(* Contenuto delle caselle:
   Lista associativa, in cui si associa un valore solo alle
   caselle che contengono qualcosa *)
type 'a contents = ('a * content list) list 

(* esempio *)
let contents =
  [(1,[Obj "oro"]); (2,[Mostro]); (4,[Obj "computer";Obj "penna"]);
   (7,[Mostro; Obj "libro"])]

(* un labirinto e' un: 'a graph * 'a contents *)
type 'a labirinto = 'a graph * 'a contents

(* esempio *)
let labirinto = (grafo,contents)

(* funzioni di supporto *)
(* trovare il contenuto di una casella *)
(* content: 'a -> 'a contents -> 'b list *)
let content x contenuti =
  try List.assoc x contenuti
  with Not_found -> []

(* verificare se una casella contiene un mostro:
   has_monster:  'a -> 'a contents -> bool *)
let has_monster x contenuti =
 List.mem Mostro (content x contenuti)

(* primo passo: adattiamo l'algoritmo di ricerca di cammino tenendo
   in considerazione la presenza dei mostri, ma ignorando la raccolta 
   degli oggetti *)
(* path: 'a labirinto -> 'a -> 'a -> 'a list *)
let path ((grafo,contenuti): 'a labirinto) ingresso uscita =
  (* cerca_da: 'a list -> 'a  -> 'a list   Ricerca da una singola casella *)
  let rec cerca_da visited casella =
    (* casi di fallimento: casella gia' visitata o contenente un mostro *)
    if List.mem casella visited || has_monster casella contenuti
    then raise NotFound
    else 
    (* siamo arrivati? *)
      if casella = uscita 
      then [casella]
      else (* passiamo per casella e proseguiamo con 
	      una delle caselle accessibili *)
	casella :: cerca_da_una_tra (casella::visited) (vicini casella grafo)
  (* cerca_da_una_tra: 'a list -> 'a list *)
  and cerca_da_una_tra visited = function
      [] -> raise NotFound
    | x::rest -> 
	try cerca_da visited x 
	with NotFound -> cerca_da_una_tra visited rest
  in cerca_da [] ingresso

(* Ora vogliamo in uscita il cammino e la lista degli oggetti raccolti*)

(* path:  'a labirinto -> 'a -> 'a -> 'a list * content list *)
let path ((grafo,contenuti): 'a labirinto) ingresso uscita =
  (* cerca_da: 'a list -> 'a  -> 'a list  * content list
     Ricerca da una singola casella *)
  let rec cerca_da visited casella =
    if List.mem casella visited || has_monster casella contenuti
    then raise NotFound
    else 
    (* siamo arrivati? *)
      if casella = uscita 
      then ([casella],content casella contenuti)
      else  
	let (cammino,oggetti) =
	  cerca_da_una_tra (casella::visited) (vicini casella grafo)
	in (casella::cammino,(content casella contenuti) @ oggetti)
  (* cerca_da_una_tra: 'a list -> 'a list -> 'a list * content list *)
  and cerca_da_una_tra visited = function
      [] -> raise NotFound
    | x::rest -> 
	try cerca_da visited x 
	with NotFound -> cerca_da_una_tra visited rest
  in cerca_da [] ingresso

(****************************************)
(* slides => Dall'esame di febbraio 2012 *)

(* A seconda del problema, le funzioni ausiliarie utilizzate nella ricerca 
   di un cammino possono avere parametri addizionali. In questo caso:
   lista degli oggetti che si devono ancora comprare *)

type shops = (int * string list) list
type city = (int * int) list * shops

(* citta' dell'esempio *)
let grafo = [(1,2);(1,3);(1,4);(2,3);(2,4);(2,7);
             (3,5);(4,6);(4,7);(5,7);(6,7)]
let shops =
  [(2,["carta";"penna"]); (4,["latte";"uova";"pane"]); 
   (5,["biglietto bus";"tabacco"]); (6,["trapano";"chiodi"])]

(* lista di oggetti da comprare dell'esempio *)
let tobuy = ["chiodi";"tabacco";"uova";"penna";"pane"]

(*  vicini : 'a -> ('a * 'a) list -> 'a list *)
(* il grafo non e' orientato *)
let rec vicini x = function
    [] -> []
  | (a,b)::rest ->
      if a=x then b::vicini x rest
      else if b=x then a::vicini x rest
      else vicini x rest

exception Fail

(* funzione ausiliaria *)
(* diff : 'a list -> 'a list -> 'a list *)
(* diff lst1 lst2 = lista che si ottiene da lst2 togliendo tutte
                    le occorrenze di tutti gli elementi di lst1 *)
let diff lst1 lst2 = 
  List.filter
    (function x -> not (List.mem x lst1)) lst2

(* compra : string list -> city -> int -> int list *)
(* funzioni ausiliarie:
   from_node : 'a list -> string list -> 'a -> 'a list
     from_node visited lista_oggetti nodo = cammino nel grafo che, partendo
             da nodo, passa per negozi in cui si possono comprare tutti
             gli oggetti di lista_oggetti 
   from_list : 'a list -> string list -> 'a list -> 'a list 
      from_list visited lista_oggetti lista_nodi = cammino nel grafo che,
            partendo da uno dei nodi in lista_nodi, passa per negozi in 
            cui si possono comprare tutti gli oggetti di lista_oggetti *)
let compra lista (graph,shops) start =
  let rec from_node visited lista nodo =
    if List.mem nodo visited then raise Fail
    else 
      let inshop = (* oggetti che si possono comprare in nodo *)
	try List.assoc nodo shops
	with Not_found -> [] in
      if List.for_all 
	  (function x -> List.mem x inshop) lista
         (* tutti gli oggetti da comprare sono presenti in nodo *)
      then [nodo]
      else nodo::from_list (nodo::visited) 
		   (diff inshop lista) (* togliamo da lista gli oggetti
                                          che possiamo comprare in nodo *)
                (* o direttamente:
		   (List.filter  
		      (function x -> not (List.mem x inshop)) lista) *)
		   (vicini nodo graph)
  and from_list visited lista = function
      [] -> raise Fail
    | n::rest ->
	try from_node visited lista n
	with Fail -> from_list visited lista rest
  in from_node [] lista start

(*
# compra tobuy (grafo,shops) 1;;
- : int list = [1; 2; 3; 5; 7; 4; 6]
*)