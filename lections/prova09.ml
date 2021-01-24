type 'a graph = ('a * 'a) list;;

let grafo = [(1,2); (1,3); (1,4); (2,6); (3,5); (4,6);
             (5,4); (6,5); (6,7)];;


let rec successori nodo = function
[] -> []
| (a,b)::rest -> 
  if a=nodo then b::successori nodo rest
  else successori nodo rest;;
  
let successori nodo graph = 
  List.map snd( List.filter(function (x,_) -> x = nodo) graph );;  


let rec vicini nodo = function 
[] -> []
| (a,b)::rest -> 
    if nodo=a then b::vicini nodo rest
    else if nodo=b then a::vicini nodo rest
    else vicini nodo rest;;

let setadd x set = 
  if List.mem x set then set else x::set;;
  
let rec nodes = function 
[] -> []
| (a,b)::rest -> setadd a (setadd b (nodes rest));;


(* VISITA IN PROFONDITA' DI UN GRAFO ORIENTATO *)

let depth_fist_collect graph start = 
  let rec search visited = function 
    [] -> visited
    | n::rest -> 
      if List.mem n visited then search visited rest
      else search (n::visited) ((successori n graph) @ rest)
  
  in search [] [start];;
  
  (* Visita in ampiezza *)
let breadth_first_collect graph start = 
  let rec search visited = function 
  [] -> visited 
  |n::rest -> 
    if List.mem n visited then search visited rest
    else search (n::visited) (rest@(successori n graph))

  in search [] [start];;  
     

  (* Visita in profondità con un predicato *)

  let depth_first_all graph start p = 
    let rec search visited = function 
   [] -> true 
   | n::rest -> 
      if List.mem n visited then search visited rest
      else p n && search (n::visited) ((successori n graph@rest))
      
    in search [] [start];;  


  exception NotFound;;


  let search_node graph start p = 
    let search visited = function 
      [] -> raise NotFound
     | n::rest -> 
        if List.mem n visited then search visited rest
        else if p n then n 
        else search (n::visited) ((successori n graph) @ rest)

    int search [] [start];;
    

    let path graph start p = 
      let rec from_node visited node = 
        if List.mem node visited then raise NotFound
        else if p node then [node] (* il cammino è stato trovato *)
        else node::from_list (n::visited) (vicini node graph)
        and from_list visited = function 
          [] -> raise NotFound 
         | n::rest -> 
            try 
              from_node visited n 
            with NotFound -> from_list visited rest
            
       int from_node [] start;;     


(* Ora proviamo ad implementare senza la mutua ricorsione *)

let gpath g start p = 
  let rec aux visited = function 
    [] -> raise NotFound 
   | n::rest ->
    if List.mem n visited then aux visited rest
    else if p n then [n]
    else try n::aux (n::visited) (vicini n g)
          with NotFound -> aux visited rest
   int aux [] [start];;
   
   

(* LABIRINTO *)
let grafo = [(1,2);(1,3);(1,4);(2,3);(2,4);(2,7);
             (3,5);(4,6);(4,7);(5,7);(6,7)];;

type content = Mostro | Obj of string;;

type 'a contents = (a' * content list) list;;

let contents =
  [(1,[Obj "oro"]); (2,[Mostro]); (4,[Obj "computer";Obj "penna"]);
   (7,[Mostro; Obj "libro"])];;

type 'a labirinto = 'a graph * 'a contents;;

let labirinto = (grafo, contents);

let content x contenuti = 
  try 
    List.assoc x contenuti 
  with Not_found -> [];;  

 let has_monster x contenuti = 
    List.mem Mostro (content x contenuti);;
    
let path ((grafo, contenuti): 'a labirinto) ingresso uscita = 
  let rec from_node visited casella = 
    if List.mem casella visited || has_monster casella grafo then raise NotFound
    else 
      if casella = uscita then [casella]
    else 
      casella::from_list (casella::visited) (vicini casella grafo)
      
      and from_list visited = function 
        [] -> raise NotFound 
        x::rest -> try 
                    from_node x visited
                   with NotFound -> from_list casella rest
    from_node [] ingresso;;                

let path ((grafo, contenuti): 'a labirinto) ingresso uscita = 
  let rec from_node visited casella = 
    if List.mem casella visited || has_monster casella contenuti then raise NotFound
    else if casella=uscita then ([casella], content casella contenuti)
    else 
      let (cammino, oggetti) = 
        from_list (n::visited) (vicini casella grafo)
      in (casella:cammino, (content casella contenuti)@oggetti)
    and from_list visited = function 
      [] -> raise NotFound 
    | x::rest -> try 
                  from_node visited x
                 with NotFound -> from_list visited rest
   
    in from_node [] ingresso;;             


(* ESAME FEBBRAIO 2012 *)

(* nella città ci sono n negozi. Non tutti i nodi rappresentano i negozio con i prodotti acquistabili *)

type shops = (int * string list) list;;

type city = (int * int) list * shops;;

let grafo = [(1,2);(1,3);(1,4);(2,3);(2,4);(2,7);
             (3,5);(4,6);(4,7);(5,7);(6,7)];;

let shops =
  [(2,["carta";"penna"]); (4,["latte";"uova";"pane"]); 
   (5,["biglietto bus";"tabacco"]); (6,["trapano";"chiodi"])];;

let tobuy = ["chiodi";"tabacco";"uova";"penna";"pane"];;

let rec vicini x = function 
[] -> []
| (a,b)::rest -> 
    if a = x then b::vicini x rest
    else if b = x then a::vicini x rest
    else vicini x rest;;
 
exception Fail;;

let diff lst1 lst2 = 
  List.filter(function x -> not(List.mem x lst)) lst2;;

 let compra lista (grafo, shops) start = 
  let rec from_node visited lista node = 
    if List.mem node visited then raise Fail
    else 
      let inshop = 
        try List.assoc node shops
        with Not_found -> []
      in 
       if List.for_all (function x -> List.mem x inshop) then [node]
       else 
        node::from_list (node::visited) (diff lista inshop) (vicini node grafo)
      and from_list visited lista = 
      [] -> raise Fail
      | x::rest -> try 
          from_node visited lista x 
          with NotFound -> from_list visited lista rest
          
      in from_node [] lista start;;    














