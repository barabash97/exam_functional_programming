
(* length: 'a list -> int *)
(* con uso dei selettori *)
let rec length lst =
  if lst=[] then 0
  else 1 + (length (List.tl lst))
(* usando il pattern matching *)
let rec length = function
    [] -> 0
  | x::rest -> 1 + length rest

(* schedina:
   super: int list list -> int -> int -> int list 
     super estrazioni dim higher:
          estrazioni e` una lista di liste rappresentante le ultime
            estrazioni,
          dim e` il numero di interi di ogni estrazione
          higher e` il massimo numero che puo` essere estratto
     Riporta i dim numeri che sono stati estratti meno volte
*)

(* test assumendo che i numeri estratti siano da 1 a 10 e che ogni 
   estrazione dia 3 numeri *)
let estrazioni = 
  [[1; 7; 3]; [5; 4; 8]; [8; 7; 1]; [6; 10; 3]; [4; 2; 3]; [1; 5; 6];
   [8; 3; 3]; [7; 7; 2]; [2; 10; 8]; [3; 5; 6]; [4; 9; 7]; [1; 6; 3];
   [8; 4; 6]; [6; 3; 3]; [5; 6; 8]; [6; 7; 1]; [9; 5; 8]; [8; 1; 2];
   [10; 7; 1]; [7; 4; 6]];;

(* sottoproblema 1: contare le occorrenze di ciascuno dei numeri 
   in [1;...;10] (o [1;...;90]) nella lista delle estrazioni *)

(* Assumiamo, per il momento di saper risolvere il problema 1b, e di
   aver gia' "appiattito" la lista estrazioni,
         flatten_estra = flatten estrazioni  *)
let flatten_estra =
  [1; 7; 3; 5; 4; 8; 8; 7; 1; 6; 10; 3; 4; 2; 3; 1; 5; 6; 8; 3; 3; 
   7; 7; 2; 2; 10; 8; 3; 5; 6; 4; 9; 7; 1; 6; 3; 8; 4; 6; 6; 3; 3; 
   5; 6; 8; 6; 7; 1; 9; 5; 8; 8; 1; 2; 10; 7; 1; 7; 4; 6]

(* sotto-sottoproblema 1a: generare la lista con i numeri da n a m *)
(*         (lista degli elementi da contare) *)
(* upto : int -> int -> int list *)
let rec upto n m =
  if n>m then []
  else n::upto (n+1) m

(* sottoproblema 1c-1: contare le occorrenze di un elemento in una lista *)
(* conta : 'a -> 'a list -> int *)
let rec conta n = function
    [] -> 0
  | x::rest ->
      if x=n then 1 + conta n rest
      else conta n rest

(* sottoproblema 1c: contare le occorrenze di tutti gli elementi di una*)
(* lista contenuti in un'altra lista, restituendo una lista di coppie *)
(*  contatutti : 'a list -> 'a list -> ('a * int) list *)
(* contatutti tutti lista = lista di coppie contenente, per ogni*)
(* elemento x in tutti, una coppia (x,n) dove n e` il numero di*)
(* occorrenze di x in lista *)
let rec contatutti tutti lista = 
  match tutti with
    [] -> []
  | x::rest -> (x,conta x lista)::contatutti rest lista

(* contatutti (upto 1 10) flatten_estra
   conta le occorrenze dei numeri compresi tra 1 e 10
   nella lista flatten_estra
 
#  contatutti (upto 1 10) flatten_estra;;
- : (int * int) list =
[(1, 7); (2, 4); (3, 9); (4, 5); (5, 5); (6, 9); (7, 8); (8, 8); (9, 2);
 (10, 3)]
*)

(* soluzione del sottoproblema 1b:
   la lista delle estrazioni deve essere trasformata in una int list
   (estrazioni deve essere trasformata in flatten_estra) *)

(* concatenazione di liste: @ infisso:
   (@): 'a list -> 'a list -> 'a list 
   Che differenza c'e` tra @ e :: ? *)
(* Esempio: inserimento di un elemento in coda a una lista
   incoda: 'a -> 'a list -> 'a list
   incoda x [x1;...;xn] = [x1;...;xn;x] *)
let incoda x lst = lst @ [x]

(* come definire l'append *)
let rec append prima dopo =
  match prima with
    [] ->  dopo
  | x::rest ->  x::(append rest dopo)

(* sottoproblema 1b: data la lista con le estrazioni del superenalotto
   (ciascuna e' una lista) costruire una lista di interi,
   "schiacciando" la lista di liste in una lista di interi *)
(* flatten : 'a list list -> 'a list *)

let rec flatten = function
    [] -> []
  | lst::rest -> lst @ (flatten rest)

(* contatutti (upto 1 10) (flatten estrazioni)
   conta le occorrenze dei numeri compresi tra 1 e 10
   nella lista estrazioni *)

(* sottoproblema 2: ordinare la lista di coppie secondo valori crescenti*)
(* del secondo elemento *)

(* slides => Merge Sort *)

(* definizione della relazione "minore" tra coppie *)
(* minore : 'a * 'b -> 'c * 'b -> bool *)
let minore (_,x) (_,y) = x<y

(* split : 'a list -> 'a list * 'a list *)
(* split : 'a list -> 'a list * 'a list *)
(* split lista riporta una coppia di liste, di lunghezza 
     "pressappoco" uguale, nelle quali sono suddivisi gli
     elementi di lista *)
let rec split = function
    [] -> ([],[])
  | [x] -> ([x],[])
  | x::y::rest -> let (xs,ys) = split rest
                  in  (x::xs,y::ys);;

(* merge : 'a list -> 'a list -> 'a list *)
(* fusione di due liste ordinate *)
(* merge : ('a * 'b) list -> ('a * 'b) list -> ('a * 'b) list *)
let rec merge xs ys = match (xs,ys) with
  ([],_) -> ys
| (_,[]) -> xs
| x::xs,y::ys -> if minore x y then x::merge xs (y::ys)
                 else y::merge (x::xs) ys;;

(* sort: 'a list -> 'a list *)
(*  sort : ('a * 'b) list -> ('a * 'b) list *)
let rec sort = function
    [] -> []
  | [x] -> [x]
  | lst -> let (xs,ys) = split lst
           in merge (sort xs) (sort ys);;

(* slides => il modulo list  *)

(* List.sort : ('a -> 'a -> int) -> 'a list -> 'a list
   compare: 'a -> 'a -> int
*)
let compare_pairs (_,x) (_,y) = compare x y
(* compare_pairs : 'a * 'b -> 'c * 'b -> int *)
(*
# List.sort compare_pairs (contatutti (upto 1 10) (flatten estrazioni));;
- : (int * int) list =
[(9, 2); (10, 3); (2, 4); (4, 5); (5, 5); (1, 7); (7, 8); (8, 8); (3, 9);
 (6, 9)
*)
   
(* sottoproblema 3: prendere i primi n elementi di una lista *)

(* take : int -> 'a list -> 'a list *)
(* take n lista= primi n elementi di lista, o lista stessa se non ce ne sono abbastanza *)
let rec take n = function
    [] -> []
  | x::xs -> 
      if n<=0 then [] 
      else  x::take (n-1) xs;;

(* 
# take 3 (sort (contatutti (upto 1 10) (flatten estrazioni)));;
- : (int * int) list = [(9, 2); (10, 3); (2, 4)]
*)

(* sottoproblema 4: 
   prendere tutti i primi elementi di una lista di coppie *)
(* primi : ('a * 'b) list -> 'a list *)
let rec primi = function
    [] -> []
  | (x,_)::rest -> x::primi rest

(* 
# primi (take 3 (sort (contatutti (upto 1 10) (flatten estrazioni) )));;
- : int list = [9; 10; 2]
*)

(* mettiamo insieme i pezzi: *)
(* soluzione del problema principale *)
(* super: int list list -> int -> int -> int list *)
let super estrazioni dim higher =
  primi (take dim
	   (sort 
	      (contatutti (upto 1 higher) (flatten estrazioni))))

(*
# super estra 3 10;;
- : int list = [9; 10; 2]
*)

(* slides => Implementazione di procedimenti iterativi  *) 
(* length iterativo *)
(* len: 'a list -> int *)
let len lst =
  (* aux: int -> 'a list -> int *)
  (* aux n lst = n + lunghezza di lst *)
  let rec aux result = function
         [] -> result
       | _::rest -> aux (result+1) rest
  in aux 0 lst

(* prodof : int list -> int *)
(* versione ricorsiva *)
let rec prodof = function 
    [] -> 1
  | x::xs -> x * prodof xs

(* versione iterativa *)
let prodof_it lst =
    (* aux: int -> int list -> int
       aux n lista = n * prodotto degli interi in lista *)
    let rec aux result = function
         [] -> result
       | x::rest -> aux  (x*result) rest
    in aux 1 lst

(* versione iterativa di upto? *)
(* upto: int -> int -> int list *)
let down_to m n =
  let rec aux result m n =
    if m > n then result
    else aux (m::result) (m+1) n
  in aux [] m n

(* versione iterativa corretta *)
let upto_it m n =
  (* aux: int list -> int -> int -> int list
     aux lista m n = [m;m+1;...;n] @ result *)
  let rec aux result m n =
    if m > n then result
    else aux (n::result) m (n-1)
  in aux [] m n

(* versione iterativa di take? *)
let rtake n lst =
  let rec aux result n = function
      [] -> result
    | x::rest -> if n<=0 then result else 
                 aux (x::result) (n-1) rest
  in aux [] n lst

(* versione iterativa con l'inserimento in coda *)
let take_it n lst =
  (* aux : 'a list -> int -> 'a list -> 'a list
     aux result n lst = result @ (take n lst) *)
  let rec aux result n = function
      [] ->  result
    | x::rest -> if n<=0 then result else 
                 aux (result@[x]) (n-1) rest
  in aux [] n lst


(* rovesciare una lista *)
(* reverse: 'a list -> 'a list *)
(* versione ricorsiva *)
let rec reverse = function
    [] -> []
  | x::xs -> (reverse xs) @ [x]

(* versione iterativa *)
let rev lst =
   (* revto : 'a list -> 'a list -> 'a list *)
   (* revto result lst = (reverse lst) @ result *)
   let rec revto result = function
       [] -> result
     | x::rest -> revto (x::result) rest
   in revto [] lst

(* take iterativo *)
(* versione con il reverse *)
let take_it n lst =
  (* aux : 'a list -> int -> 'a list -> 'a list *)
  (* aux result n list = (rev result) @ (take n lst) *)
  let rec aux result n = function
      [] -> List.rev result
    | x::rest -> if n<=0 then List.rev result else 
                 aux (x::result) (n-1) rest
  in aux [] n lst


(* ========================================================= *)
(* utility *)
(* generazione casuale di una lista con n liste di interi compresi tra*)
(* 1 e higher, ogni sottolista ha dim elementi *)

(* http://caml.inria.fr/pub/docs/manual-ocaml/libref/Random.html *)
(* un'estrazione generata in questo modo potrebbe contenere piu'
   occorrenze dello stesso numero *)
let genera n dim higher =
    (* aux : int -> int list
       aux n genera una lista con n numeri random 
         compresi tra 1 e higher *)
  let rec aux = function
      0 -> []
    | i -> (1+(Random.int higher)):: aux (i-1)
    (* mkall:  int -> int list list
       mkall n genera una lista con n liste, ciascuna
          delle quali contiene dim numeri random *)
  in let rec mkall = function
      0 -> []
    | i -> (aux dim)::mkall(i-1)
  in mkall n

(* 
let ultime = genera 100 6 90;;

super ultime 6 90
*)