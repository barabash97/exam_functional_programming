
exception Nodo_inesistente;;

let successori x grafo = 
  try 
  List.assoc x grafo 
  with Not_found -> raise Nodo_inesistente;;

type 'a graph = ('a * 'a) list;;

let rec successori nodo = function 
 [] -> []
| (x,y)::rest -> 
    if(x == nodo) then y::successori nodo rest
    else successori nodo rest;;
 
let successori nodo grafo = 
  List.map snd ( List.filter(function(x,_) -> x=nodo) grafo);;    

  
let rec vicini nodo = function 
  [] -> []
 | (x,y)::rest -> 
    if x=nodo then y::vicini nodo rest
    else if y=nodo then x::vicini nodo rest
    else vicini nodo rest;;    

    let grafo = [(1,2); (1,3); (1,4); (2,6); (3,5); (4,6);
    (5,4); (6,5); (6,7)];;
    
    
 (* vicini 1  *) 
 

(* Aggiungere una coppia nel grafo 

setadd: 'a -> 'a list -> 'a list;;
*)
 let setadd x set = 
  if List.mem x set then set else x::set;;
  

  (* Nodi di un grafo 
    nodes: 'a graph -> 'a list
  *)
let rec nodes = function 
  [] -> []
 | (x,y)::rest -> setadd x (setadd y (nodes rest));;



(*********  ALGORITMI SUI GRAFI ********)

(* visita in profondità del grafo non orientato *)

(* depth_first_collect: a' graph -> 'a -> 'a list *)
(* search: 'a list -> 'a list -> 'a list *)
let depth_first_collect graph start = 
  let rec search visited = function 
    [] -> visited
  | n::rest -> 
    if List.mem n visited 
    then search visited rest
    else search (n::visited) ((successori n graph) @ rest)
  
  in search [] [start];;  
 

 (* Visita in profondità per verificare se tutti i nodi soddisfano un predicato *)

let depth_first_all graph start p = 
  let rec search visited = function 
  [] -> true 
 | n::rest -> 
          if List.mem n visited then search visited rest
          else p n && search (n::visited) ((successori n graph) @ rest)
  in search [] [start];;        
  
 (***********    VISITA IN AMPIEZZA *********) 

 (* breadth_first_collect: 'a graph -> 'a -> 'a list*)
(* search: 'a list -> 'a list -> 'a list *)
 let breadth_first_collect graph start = 
  let rec search visited = function
    [] -> visited
   | n::rest -> 
   if List.mem n visited then search visited rest
   else search (n::visited) (rest @ (successori n graph))
   in search [] [start];;


exception NotFound;;

let search_node graph start p = 
  let rec search visited = function 
  [] -> raise NotFound
 | n::rest -> 
        if List.mem n visited then search visited rest
        else if n p then n
        else search (n::visited) ((successori n graph) @ rest)

  in search [] [start];;      


  (* RICERCA DI UN CAMMINO MEDIANTE BACKTRACKING *)

let search_path graph start p = 
  let rec from_node visited a = 

    if List.mem a visited then raise NotFound 
    else if p a then [a]
    else a::from_list (a::visited) (vicini a graph)

    and from_list visited = function
      [] -> raise NotFound 
      | a::rest -> (* provo a passare per a, ma se fallisco cerco
                    ancora passando per uno dei nodi *)
                 try from_node visited a
                 with NotFound -> from_list visited rest
                  
  in from_node [] start;;

(* Ora proviamo a implementare questo metodo, ma senza la mutua ric.*)


let gpath g start p =  
 let rec aux visited = function 
 [] -> raise NotFound 
 | x::rest -> if List.mem x visited then aux visited rest
              else if p x then [x]
              else try x::aux (x::visited) (vicini x g)
                   with NotFound -> aux visited rest
 in aux [] [start];;                 

                     
   
let grafo = [(1,2);(1,3);(1,4);(2,3);(2,4);(2,7);
(3,5);(4,6);(4,7);(5,7);(6,7)];;

(* LABIRINTO *)

(* archi non sono orientati *)
   

type content = Mostro | Obj of string;;

type 'a contents = ('a * content list) list;;

let contents = [(1,[Obj "oro"]); (2,[Mostro]); (4,[Obj "computer";Obj "penna"]);
(7,[Mostro; Obj "libro"])];;

type 'a labirinto = 'a graph * 'a contents

let labirinto = (grafo,contents);;

(* Creiamo le funzioni di supporto *)

(* Trovare il contenuto di una casella*)
let content x contenuti = 
  try 
    List.assoc x contenuti 
  with Not_found -> [];;
  
(* Verificare se una casella contiene *)  
let has_monster x contenuti = 
  List.mem Mostro (content x contenuti);;

 (* Inizialmente adattiamo algoritmo del cammino tenendo in considerazione
 la presenza dei mostri, senza raccogliere oggetti *)
 

let path ((grafo, contenuti): 'a labirinto) ingresso uscita = 
  let rec from_node visited casella = 
    if List.mem casella visited || has_monster casella contenuti then raise NotFound
    else if casella = uscita then [casella]
    else 
      casella::from_list_node (casella::visited) (vicini casella grafo)
    and from_list_node visited = function 
    [] -> raise NotFound
   | x::rest -> try 
                  from_node visited x
                with NotFound -> from_list_node visited rest
    in from_node [] ingresso;;
    
    
(* Ora lo adattiamo anche per raccolta di contenuti *)

let path((grafo, contenuti): 'a labirinto) ingresso uscita = 
  let rec cerca_da visited casella = 
    if List.mem casella visited || has_monster casella contenuti then raise NotFound
    else if casella = uscita then ([casella], content casella contenuti)
    else 
      let(cammino, oggetti) = 
        cerca_da_uno_tra (casella::visited) (vicini casella grafo)
      in (casella::cammino, (content casella contenuti) @ oggetti)
      
      and cerca_da_una_tra visited = function 
        [] -> raise NotFound 
        | x::rest -> try 
            cerca_da visited x 
            with NotFound -> cerca_da_una_Tra visited rest

     in cerca_da [] ingresso;;       
                

(* Esame febbraio 2012 *)

type shops = (int * string list) list;;
type city = (int * int) list * shops;; 

let grafo = [(1,2);(1,3);(1,4);(2,3);(2,4);(2,7);
             (3,5);(4,6);(4,7);(5,7);(6,7)];;

let shops =
[
  (2,["carta";"penna"]); (4,["latte";"uova";"pane"]); 
  (5,["biglietto bus";"tabacco"]); (6,["trapano";"chiodi"])
];;

let tobuy = ["chiodi";"tabacco";"uova";"penna";"pane"];;

let rec vicini x = function 
 [] -> [] 
| (a,b)::rest -> 
  if a=x then b::vicini x rest
  else if b=x then a::vicini x rest
  else vicini x rest;;

exception Fail;;

let diff lst1 lst2 = 
  List.filter(function x -> not(List.mem x lst1)) lst2;;

let compra lista (graph, shops) start = 
  let rec from_node visited lista nodo = 
    if List.mem nodo visited then raise Fail
    else 
      let inshop = (* Oggetti che si possono comprare in nodo *)
          try List.assoc nodo shops 
          with Not_found -> []
      in if List.for_all (function x -> List.mem x inshop) lista then [nodo] (* Tutti oggetti da comprare sono presenti nel nodo*)
         else nodo::from_list (nodo::visited) (diff inshop lista) (vicini nodo graph)
       and from_list visited lista = function 
        [] -> raise Fail
       | n::rest -> 
          try 
            from_node visited lista n
          with Fail -> from list visited lista rest
          
   in from_node [] lista start;;       

