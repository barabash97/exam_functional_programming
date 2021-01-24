
(* slides *)
(* ---------- Espressioni aritmetiche ---------- *)

type expr = 
  Int of int
| Var of string
| Sum of expr * expr
| Diff of expr * expr
| Mult of expr * expr
| Div of expr * expr 

type ambiente = (string * int) list

(* eval : ambiente -> expr -> int *)
(* eval env e = valore dell'espressione e  nell'ambiente env.
 Errore se qualche variabile in e  non ha un valore
 associato in env *)
let rec eval env = function
  Int n -> n
| Var x -> 
    (try List.assoc x env
    with Not_found -> failwith "eval")
| Sum(e1,e2) -> (eval env e1) + (eval env e2)
| Diff(e1,e2) -> (eval env e1) - (eval env e2)  
| Mult(e1,e2) -> (eval env e1) * (eval env e2)
| Div(e1,e2) -> (eval env e1) / (eval env e2)


(* restrizioni di tipo nelle dichiarazioni *)
let x = []   (* 'a list *)
let y = ([]: int list)

(* existing_link : 'a -> 'a list -> bool *)
let existing_link link env =
List.mem link env

(* existing_link : string * int -> ambiente -> bool *)
let existing_link link (env: ambiente) =
List.mem link env

(* Rappresentazione alternativa delle espressioni *)
type op = Sum | Diff | Mult | Div
type expr =
  Int of int
| Var of string
| Apply of op * expr * expr

(* op2mat : op -> int -> int -> int *)
(* op2mat op = operazione aritmetica (funzione di tipo int -> int -> int)
 rappresentata da op *)
let op2mat = function
  Sum -> (+)
| Diff -> (-)
| Mult -> ( * )
| Div -> (/)

(*  eval : (string * int) list -> expr -> int *)
let rec eval env = function
  Int n -> n
| Var x -> List.assoc x env
| Apply(op,e1,e2) -> op2mat op (eval env e1) (eval env e2)


(* ---------------------------------------- *)
(* slides => alberi *)

(* Rappresentazione di alberi-binari *)

type 'a tree =
  Leaf of 'a
| One of 'a * 'a tree
| Two of 'a * 'a tree * 'a tree

(* size : 'a tree -> int *)
(* size t = numero di nodi in t *)
let rec size = function
  Leaf _ -> 1
| One(_,t) -> 1 + size t
| Two(_,t1,t2) -> 1 + size t1 + size t2

(* slides => rappresentazione alternativa *)
(* oppure, "inventando" l'albero vuoto: *)

type 'a tree = Empty
| Tr of 'a * 'a tree * 'a tree

(* size : 'a tree -> int *)
let rec size = function
  Empty -> 0
| Tr(_,t1,t2) -> 1 + size t1 + size t2

(* albero di esempio *)
let albero = 
  Tr(1, Tr(2, Tr(4,Empty,Empty), Empty), 
  Tr(3, Tr(5, Tr(6,Empty,Empty), Tr(7,Empty,Empty)),
  Empty)) 

(** ----------------------------------------------- *)
(* tree_exists: ('a -> bool) -> 'a tree -> bool *)
(* tree_exists p t = true sse t contiene almeno un nodo che soddisfa p *)
let rec tree_exists p = function
  Empty -> false
| Tr(x,t1,t2) -> 
    p x || tree_exists p t1 || tree_exists p t2

(* raccogli: 'a tree -> 'a list *)
(* raccogli t = lista di tutte le etichette dei nodi di t (con ripetizioni) *)
let rec raccogli = function
  Empty -> []
| Tr(x,t1,t2) ->
    x::((raccogli t1)@(raccogli t2))

(* foglie: 'a tree -> 'a list *)
(* foglie t = lista di tutte le etichette delle foglie di t (con ripetizioni) *)
let rec foglie = function
  Empty -> []
| Tr(x,Empty,Empty) -> [x]
| Tr(x,t1,t2) ->
    (foglie t1)@(foglie t2)

(* nodi_con_un_figlio: 'a tree -> 'a list *)
(* nodi_con_un_figlio t = 
    lista di tutte le etichette dei nodi di t che hanno esattamente un 
    figlio *)
let rec nodi_con_un_figlio = function
  Empty | Tr(_,Empty,Empty) -> []
| Tr(x,Empty,t) | Tr(x,t,Empty) -> 
    x::(nodi_con_un_figlio t)
| Tr(x,t1,t2) ->
    (nodi_con_un_figlio t1)@(nodi_con_un_figlio t2)


(* ricerca di un ramo dalla radice a una foglia data *)
(* rappresentiamo i rami mediante liste di etichette *)

(* eccezione da sollevare in caso di fallimento 
 (l'albero non ha nessuna foglia etichettata dal valore dato) *)
exception NotFound

(* path_to : 'a -> 'a tree -> 'a list *)
(* path_to x t = ramo in t dalla radice a una foglia (qualsiasi)
               etichettata da x *)
let rec path_to x = function
  Empty -> raise NotFound
| Tr(y,Empty,Empty) -> 
    if y=x then [x]
    else raise NotFound
| Tr(y,t1,t2) ->     (* backtracking *)
    y::(try path_to x t1
        with NotFound -> path_to x t2)

(* --------------------- *)
(* slides => visita di un albero con risultato parziale *)
(* problema: "contare" le occorrenze di un'etichetta in un
            albero binario *)
(* count: 'a tree -> ('a * int) list*)

(* sottoproblema: operazione da compiere quando si visita un nodo *)
(* add : 'a -> ('a * int) list -> ('a * int) list *)
(* add y lst = lista che si ottiene da lst sostituendo la 
             coppia (y,n) con (y,n+1), se una tale coppia esiste, 
             altrimenti aggiungendo la coppia (y,1) *)
let rec add y = function
  [] -> [(y,1)]
| (x,n)::rest ->
    if x=y then (x,n+1)::rest
    else (x,n)::add y rest

(* count : 'a tree -> ('a * int) list *)
(* aux:  ('a * int) list -> 'a tree ->  ('a * int) list *)
(* aux result t = lista che si ottiene da result "aggiungendo"
                        (con add) i nodi di t *)
let count t =
let rec aux result = function
    Empty -> result
  | Tr(a,t1,t2) -> 
aux (aux (add a result) t1) t2
in aux [] t

(* ---------- il codice Morse -------------- *)
(* http://it.wikipedia.org/wiki/Codice_Morse *) 

(* utils *)
(* explode: string -> char list *)
(* explode s = lista dei caratteri che compongono s *)
let explode s =
let rec aux n result =
  if n < 0 then result
  else aux (n-1) (s.[n] :: result) in
aux (String.length s - 1) []
(* implode: char list -> string *)
(* implode lst = stringa con i caratteri in lst *)
let implode lst =
let rec aux result = function
    [] -> result
  | c::rest -> aux (result^(String.make 1 c)) rest
in aux "" lst 

type segnale = 
  Linea | Punto 
| Pausa | Errore (* per la codifica di caratteri 
                    di cui non e` dato il codice *)

type morse_assoc = (char * segnale list) list

(* morse: morse_assoc
 rappresentazione del codice morse come lista associativa *)
let morse =
[ 'A', [Punto;Linea] ; (* non servono le parentesi per le coppie *)
  'B', [Linea;Punto;Punto;Punto];
  'C', [Linea;Punto;Linea;Punto];
  'D', [Linea;Punto;Punto];
  'E', [Punto] ;
  'F', [Punto;Punto;Linea;Punto];
  'G', [Linea;Linea;Punto];
  'H', [Punto;Punto;Punto;Punto];
  'I', [Punto;Punto];
  'J', [Punto;Linea;Linea;Linea];
  'K', [Linea;Punto;Linea];
  'L', [Punto;Linea;Punto;Punto];
  'M', [Linea;Linea];
  'N', [Linea;Punto];
  'O', [Linea;Linea;Linea];
  'P', [Punto;Linea;Linea;Punto];
  'Q', [Linea;Linea;Punto;Linea];
  'R', [Punto;Linea;Punto];
  'S', [Punto;Punto;Punto];
  'T', [Linea];
  'U', [Punto;Punto;Linea];
  'V', [Punto;Punto;Punto;Linea];
  'W', [Punto;Linea;Linea];
  'X', [Linea;Punto;Punto;Linea];
  'Y', [Linea;Punto;Linea;Linea];
  'Z', [Linea;Linea;Punto;Punto]
]
  
(* codifica di una sequenza di caratteri (alfabetici maiuscoli) +
 spazio.   Lo spazio e` codificato con [Pausa] *)
(* encode: morse_assoc -> char -> segnale list *)
(* encode codice c = codifica di c secondo il codice *)
(* si potrebbe anche eliminare il primo argomento e usare
 morse come variabile globale, come abbiamo fatto in 06.ml *)
let encode morse_assoc = function
  ' ' -> [Pausa]
| c -> 
    try List.assoc c morse_assoc
    with Not_found -> [Errore] 

(* encode: morse_assoc -> char list -> segnale list list *)
(* encode_msg chars = sequenza di codici dei caratteri in chars *)
let  encode_msg morse_assoc messaggio = 
List.map (encode morse_assoc) messaggio

let msg = encode_msg morse (explode "CIAO PIPPO") 
let msg2 = encode_msg morse (explode "CiAO PIPPO")

(* per la decodifica:
 albero di caratteri: punto=a sinistra; linea=a destra *)
(* non tutti i nodi  corrispondono a un carattere (alfabetico);
 ad esempio la radice dell'albero *)
(* ma la maggior parte dei nodi intermedi hanno esattamente due figli *)
(* usiamo '*' come etichetta di nodi che non corrispondono a caratteri *)
(* variabile globale *)
let none = '*'

type morse_tree = 
  Leaf of char
| Node of char * morse_tree * morse_tree

(* slides => l'albero del codice morse *)
(* morse_tree: morse_tree
 albero che rappresenta il codice morse
  (si puo` anche usare lo stesso nome del tipo) *)
let morse_tree =  
Node ('*',
Node ('E',
      Node ('I', 
      Node ('S', Leaf 'H', Leaf 'V'), 
      Node ('U', Leaf 'F', Leaf '*')),
      Node ('A', 
      Node ('R', Leaf 'L', Leaf '*'), 
      Node ('W', Leaf 'P', Leaf 'J'))),
Node ('T',
      Node ('N', 
      Node ('D', Leaf 'B', Leaf 'X'), 
      Node ('K', Leaf 'C', Leaf 'Y')),
      Node ('M', 
      Node ('G', Leaf 'Z', Leaf 'Q'), 
      Leaf 'O')))

(* decodifica di una sequenza di segnali *)
(* decode : morse_tree -> segnale list -> char *)
(* decode tree codice = carattere rappresentato dalla lista di segnali
                      codice, secondo la codifica di tree *)
(* si potrebbe eliminare il primo argomento e usare
 morse_tree come globale *)
let decode tree segnale =
(* aux: morse_tree -> segnale list -> char *)
(* stessa specifica di decode, ma escludendo i casi 
   segnale=[Pausa] e segnale=[Errore] *)
let rec aux t lst =
  match (t,lst) with
    (Leaf a, []) | (Node (a,_,_), []) -> a
  | (Node(_,left,_), Punto::rest) ->
aux left rest
  | (Node(_,_,right),Linea::rest) ->
aux right rest
  | _ ->  '*'  (* carattere non riconosciuto,
                  ma non si butta tutto il messaggio *)
               (* Quali sono gli altri casi? *)
in 
if segnale = [Pausa] then ' '
else if segnale = [Errore] then '*'
else aux tree segnale

let s = List.assoc 'V' morse
let _ = decode morse_tree s
let _ = decode morse_tree [Punto; Punto; Punto; Linea; Punto; Linea]

(* decodifica di una sequenza di liste di segnali;
 la lista [Pausa] rappresenta uno spazio *)
(* decode_msg : morse_tree -> segnale list list -> char list *)
(* decode_msg tree lista_codici = lista di caratteri codificati, secondo
                                tree, da lista_codici *)
let rec decode_msg tree messaggio = 
List.map (decode tree) messaggio
 
let decod = implode (decode_msg morse_tree msg)
let decod2 = implode (decode_msg morse_tree msg2)

(* E se non si ha la lista associativa morse, ma soltanto l'albero,
 come si puo' effettuare la codifica?
 Ricerca di un carattere nell'albero, ricordando il cammino
 percorso (come sequenza di linee e punti) *)
exception NotFound
(* encode_from_tree : morse_tree -> char -> segnale list *)
(* encode_from_tree tree c = codice morse di c, secondo tree *)
(* utilizza la funzione ausiliaria
 search: morse_tree -> segnale list 
 search tree = lista di Linea/Punto che rappresentano il cammino
                 dalla radice di tree a un nodo etichettato da c
                 Punto=sinistra, Linea=destra.
                 Solleva NotFound se nessun nodo e' etichettato da c *)

let encode_from_tree tree c =
let rec search = function
    Leaf a -> 
if a=c then [] 
else raise NotFound
  | Node(a,t1,t2) ->
if a=c then []
else (* backtracking *)
  try Punto::search t1         
  with NotFound -> Linea::search t2
in if c = ' ' then [Pausa]
else 
  try search tree
  with NotFound -> [Errore]

(* encode_from_tree_msg : morse_tree -> char list -> segnale list list *)
(* encode_from_tree_msg tree lst = lista dei codici morse dei caratteri
                                 in lst *)
let encode_from_tree_msg tree =
List.map (encode_from_tree tree)

let _ = 
let list = explode "CiAO PIPPO" in
encode_msg morse list = encode_from_tree_msg morse_tree list 

(* Per i curiosi: *)
(* ------ costruzione dell'albero dalla lista associativa ---- *)

(* add : char -> segnale list -> morse_tree -> morse_tree *)
(* aggiunge il carattere c all'albero t; code e' la parte del codice 
   ancora da "scandire" *)
let rec add c code = function
  Leaf a ->
    begin          (* begin .... end *)
match code with
  [] -> (* non possono esserci due caratteri con lo stesso codice *)
    if a=none then Leaf c
    else failwith "Impossible"
| Punto::rest ->
    Node(a,add c rest (Leaf none),Leaf none)
| Linea::rest ->
    Node(a,Leaf none,add c rest (Leaf none))
| _ -> failwith "Impossible" (* quali sono gli altri casi? *)
    end 
| Node(a,t1,t2) ->
    match code with
[] -> 
  if a=none then Node(c,t1,t2)
  else failwith "Errore"
    | Punto::rest ->
  Node(a,add c rest t1,t2)
    | Linea::rest ->
  Node(a,t1,add c rest t2)
    | _ -> failwith "Impossible" (* quali sono gli altri casi? *)

let code = List.assoc 'A' morse
let _ =  add 'A' code (Leaf none)
(*
Node ('*', 
    Node ('*', Leaf '*', Leaf 'A'), 
    Leaf '*')
*)

(* build_morse_tree : morse_assoc -> morse_tree *)
let build_morse_tree lista = 
(* aux : morse_tree -> (char * segnale list) list -> morse_tree *)
let rec aux t = function
    [] -> t
  | (c,code)::rest ->
aux (add c code t) rest
in aux (Leaf none) lista

let morse_tree = build_morse_tree morse

(* --- costruzione della lista associativa a partire dall'albero ---*)
(*  build_assoc_list : morse_tree -> (char * segnale list) list *)
let rec build_assoc_list = function
    Leaf a ->
if a=none then []
else [ (a,[]) ]
  | Node(a,punto,linea) ->
let assoc = 
  (List.map 
    (function (c,code) -> (c,Punto::code))
    (build_assoc_list punto))
  @ (List.map 
       (function (c,code) -> (c,Linea::code))
       (build_assoc_list linea))
in if a='*' then assoc
else (a,[])::assoc

let new_morse = build_assoc_list morse_tree
    
let _ = encode morse 'X' 
let _ = encode new_morse 'X' 