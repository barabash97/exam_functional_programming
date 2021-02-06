type 'a option = Some of 'a | None;;
type 'a graph = ('a * 'a) list;;


(* successori: 'a -> 'a graph -> 'a list *)
let rec successori nodo = function 
  [] -> []
  | (x,y)::rest -> 
    if x = nodo then y::successori nodo rest
    else successori nodo rest;;

(* checks: 'a -> 'a list -> bool *)
let checks n = function 
Some(y) -> y = n
| None -> true ;;

let grafo = [(1,3); (3,4); (4,3); (4,2); (2,5); (5,5); (1,2)];;

(* whichpath: 'a graph -> 'a option list -> 'a -> 'a -> 'a list *)
(* from_node: 'a list -> 'a -> 'a option list -> 'a list *)
(* from_list: 'a list -> 'a option list -> 'a list *)
let whichpath g pattern_list start goal = 
  let rec from_node visited a pl = 
    if pl = [] then 
      if List.mem a visited then failwith "from_node"
      else if a = goal then [a]
      else a::(from_list (a::visited) [] (successori a g))
    else 
      if checks a (List.hd pl)
      then a::(from_list (a::visited) (List.tl pl) (successori a g)) 
    else failwith "from_node"
    and from_list visited pl = function 
      [] -> failwith "from_list"   
     | x::rest -> 
        try 
          from_node visited x pl
        with _ -> from_list visited pl rest
   in from_node [] start pattern_list;;        

let result = whichpath grafo [Some 1; None; Some 4; Some 3; None] 1 5;;   

result;;