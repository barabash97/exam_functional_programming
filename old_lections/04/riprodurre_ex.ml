(* Lunghezza lista:
a' list -> int 
*)

let rec length lst = 
  if lst=[] then 0
  else 1 + length (List.tl lst);;


  (* Lunghezza lista con pattern:
a' list -> int 
*)
let rec length_pattern = function 
  [] -> 0
  | x::rest -> 1 + length_pattern rest;;


  let estrazioni = 
    [[1; 7; 3]; [5; 4; 8]; [8; 7; 1]; [6; 10; 3]; [4; 2; 3]; [1; 5; 6];
     [8; 3; 3]; [7; 7; 2]; [2; 10; 8]; [3; 5; 6]; [4; 9; 7]; [1; 6; 3];
     [8; 4; 6]; [6; 3; 3]; [5; 6; 8]; [6; 7; 1]; [9; 5; 8]; [8; 1; 2];
     [10; 7; 1]; [7; 4; 6]];;

  let flatten =
    [1; 7; 3; 5; 4; 8; 8; 7; 1; 6; 10; 3; 4; 2; 3; 1; 5; 6; 8; 3; 3; 
     7; 7; 2; 2; 10; 8; 3; 5; 6; 4; 9; 7; 1; 6; 3; 8; 4; 6; 6; 3; 3; 
     5; 6; 8; 6; 7; 1; 9; 5; 8; 8; 1; 2; 10; 7; 1; 7; 4; 6];;

  (* Generazione lista da n a m 
      upto: int -> int -> int list
  *)

  let rec upto n m = 
    if n > m then []
    else n::upto (n+1) m;;

(* Contare le occorrenze di un elemento nella lista  *)

let rec conta n = function 
[] -> 0
| x::rest -> 
    if(x = n) then 1 + conta n rest
    else conta n rest;;


(* sottoproblema 1c: contare le occorrenze di tutti gli elementi di una*)
(* lista contenuti in un'altra lista, restituendo una lista di coppie *)
(*  contatutti : 'a list -> 'a list -> ('a * int) list *)
(* contatutti tutti lista = lista di coppie contenente, per ogni*)
(* elemento x in tutti, una coppia (x,n) dove n e` il numero di*)
(* occorrenze di x in lista *)

let rec contatutti tutti lista = match tutti with 
[] -> []
    | x::rest -> (x,conta x lista)::contatutti rest lista;;

    (* concatenazione di liste: @ infisso:
   (@): 'a list -> 'a list -> 'a list 
   Che differenza c'e` tra @ e :: ? *)
(* Esempio: inserimento di un elemento in coda a una lista
   incoda: 'a -> 'a list -> 'a list
   incoda x [x1;...;xn] = [x1;...;xn;x] *)
let incoda x lst = lst @ x;;

(* come definire l'append *)

let rec append prima dopo = 
  match prima with 
  [] -> dopo
  | x::rest -> x::(append rest dopo);;

  (* sottoproblema 1b: data la lista con le estrazioni del superenalotto
   (ciascuna e' una lista) costruire una lista di interi,
   "schiacciando" la lista di liste in una lista di interi *)
(* flatten : 'a list list -> 'a list *)


let rec flatten = function 
  [] -> []
  | lst::rest -> lst @ (flatten rest);;


  let rec stampa_lista_numeri = function 
  [] -> print_string "\n"
  | x::rest -> print_int(x); print_string("\n"); stampa_lista_numeri rest;;

let minore (_, x) (_, y) = x < y;;

(* algoritmo merge *)


let rec split = function 
    [] -> ([], [])
    | [x] -> ([x], [])
    | x::y::rest -> let (xs, ys) = split rest
                    in (x::xs, y::ys);;



let rec merge xs ys = match (xs, ys) with
([],_) -> ys
| (_, []) -> xs
| x::xs, y::ys -> if minore x y then x::merge xs (y::ys)
                  else y::merge (x::xs) ys;;
                  
let rec sort = function 
[] -> []
| [x] -> [x]
| lst -> let (xs, ys) = split lst
          in merge (sort xs) (sort ys);;


let compare_pairs (_,x)(_,y) = compare x y;;

let rec take n = function 
  [] -> []
  | x::xs -> if(n<=0) then []
    else x::take (n-1) xs;;

let rec primi = function
[] -> []
| (x, _)::rest -> x::primi rest;;

let super estrazioni dim higher = 
  primi (take dim (
    sort 
      (contatutti (upto 1 higher) (flatten estrazioni))
  ));;



(* Pagina 4.18  *)

let upto_it m n = 
  let rec aux result m n = 
    if(m > n) then result
    else aux (n::result) m (n-1)
    in aux [] m n;;

let rec take n = function 
    [] -> []
    | x::rest -> if n <= 0 then []
      else x::take (n-1) rest;;

let take_it n lst = 
  let rec aux result n = function 
    [] -> result
    | x::rest -> if(n<=0) then result
                else aux (x::result) (n-1) rest
                in aux [] n lst;;


let take_it_inserimento_coda n lst = 
  let rec aux result n = function 
   [] -> result 
   | x::rest -> if n<=0 then result 
                else aux (result@[x]) (n-1) rest
   in aux [] n lst;;
   
(* NON CONVIENE MAI INSERIRE IN CODA, CONVIENE INSERIRE IN TESTA E ALLA FINE FARE IL REVERSE *)

let rec reverse = function  
  [] -> []
  | x::rest -> (reverse rest)@[x];;


let reverse_it lst = 
  let rec aux result = function 
  [] -> []
  | x::rest -> aux (x::result) rest
  in aux [] lst;;
  
  