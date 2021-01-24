
(** Uso delle liste per rappresentare dizionari *)

(* ricerca: 
   assoc: 'a -> ('a * 'b) list -> 'b *)
   exception NotFound
   let rec assoc k = function
       [] -> raise NotFound
     | (k1,v)::rest -> if k = k1 then v   
                       else assoc k rest
   
   (* inserimento:
      inserisci : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list *)
   let inserisci k v assoc_list =
         (k,v)::assoc_list
   
   (* cancellazione *)
   (* cancella : 'a -> ('a * 'b) list -> ('a * 'b) list *)
   let rec cancella k = function
       [] -> []
     | (k',v)::rest -> 
         if k=k' then cancella k rest
         else (k',v)::cancella k rest
   
   (* slides => Rappresentazione di tipi astratti di dati *)
   
   (** Rappresentazione di insiemi finiti *)
   (*    mem: 'a -> 'a list -> bool
         mem x lst = true se lst contiene x *)
   let rec mem x = function
     [] -> false
     | y::rest ->
          x=y || mem x rest
      (** if x=y then true else mem x rest: NO! *)
   
   (* ma d'ora in poi usiamo List.mem *)
   
   (* l'implementazione di unione, intersezione e differenza insiemistica
      sono lasciate per esercizio *)
   
   
   (*** slides => Backtracking *)
   
   (**** BACKTRACKING ****)
   
   (* PROBLEMA: Dato un insieme S di numeri interi positivi e un intero
   N, determinare un sottoinsieme Y di S tale che la somma degli elementi
   di Y sia uguale a N. *)
   
   (* Criterio per scartare una soluzione parziale: la somma dei suoi elementi e' 
      maggiore di N. 
   utility: somma degli elementi di una lista *)
   (* sumof: int list -> int
      sumof [x1;...xn] = x1+...+xn *)
   let rec sumof = function
       [] -> 0
     | x::rest -> x+sumof rest
   
   (* eccezione per segnalare il fallimento *)
   exception NotFound
   
   (* subset_search : int list -> int -> int list *)
     (* aux : int list ->  int list -> int list *)
     (* funzione ausiliaria che implementa il ciclo *)
     (* aux solution altri = lista@solution, dove lista contiene numeri
           scelti da altri, tali che sumof(lista@solution) = n *)
   let subset_search set n =
     let rec aux solution altri = 
       let somma = sumof solution in 
       (* caso di fallimento *)
       if somma > n then raise NotFound
       else if somma=n then solution (* terminazione con successo *)
            else match altri with
        [] -> (* non si possono aggiungere altri elementi *)
                  raise NotFound
      | x::rest -> 
     (* proviamo ad aggiungere x, se troviamo una soluzione, bene,
        altrimenti proviamo senza x *)
                try aux (x::solution) rest 
                with NotFound -> aux solution rest 
     in aux [] set
   
   (* 
   # subset_search [8;5;1;4] 9;;
   - : int list = [1; 8]
   *)
   
   (* vogliamo avere tutte le soluzioni?
      Per farle stampare una alla volta possiamo forzare il fallimento *)
   
   (* per stampare una soluzione *)
   (* stampa_sol : int list -> unit 
       stampa_sol solution: stampa gli elementi di solution, separati
       da virgole *)
   let rec stampa_sol = function
       [] -> print_string "\n"
     | [x] -> print_int x; print_string "\n"
     | x::y::rest ->
         print_int x; print_string ", ";
         stampa_sol (y::rest)
   
   (* subset_search_printall:  int list -> int -> unit *)
     (* aux : int list ->  int list -> 'a *)
     (* aux solution altri = stampa tutte le liste della forma 
          lista@solution, tali che sumof(lista@solution)= n.
          Fallisce sempre, sollevando NotFound *)
   let subset_search_printall set n =
     let rec aux solution altri = 
       let somma = sumof solution in 
       if somma > n then raise NotFound
       else if somma=n 
            then 
             begin   (* <===== *)
         stampa_sol solution;
         raise NotFound
       end     (* <===== *)
            else match altri with
        [] -> raise NotFound
      | x::rest -> 
                try aux (x::solution) rest 
                with NotFound -> aux solution rest 
     in try aux [] set
        with NotFound -> ()  (* <===== *)
   
   (* ricerca di tutte le soluzioni, che vanno riportate in una lista  *)
   (* non si sollevano mai eccezioni, si riporta sempre una lista - eventualmente
      vuota *)
   
   (* search_all : int list -> int -> int list list *)
     (* aux : int list ->  int list -> int list list *)
     (* aux solution altri = lista contenente tutte le liste della forma 
          lista@solution, tali che sumof(lista@solution)= n. *)
   let search_all set n =
     let rec aux solution altri = 
       let somma = sumof solution in 
       if somma > n then [] (* <=== non ci sono soluzioni: lista vuota *)
       else if somma=n then [solution] (* <=== lista con l'unica 
                                               soluzione trovata*)
            else match altri with
        [] -> [] (* <=== non ci sono soluzioni *)
      | x::rest -> 
     (* concateniamo (con @) le soluzioni che troviamo aggiungendo x
              con quelle che troviamo senza x *)
                 (aux (x::solution) rest) @ (aux solution rest) 
     in aux [] set
   
   (*
   # search_all [8;5;1;4] 9;;
   - : int list list = [[1; 8]; [4; 5]]
   *)
   
   (* =========== Esercizio: raffinamento ============ *)
   (* possiamo evitare di calcolare la somma degli elementi della soluzione,
      se aggiungiamo a aux un parametro intero che rappresenta il valore che 
      manca per raggiungere il totale desiderato. 
      Inizialmente e' uguale a N.
      Quando e' negativo abbiamo sballato *)
   
   (** Vedere sul libro il problema delle n regine **)
   
   (* ================================================= *)
   (* Problema: attraversamento di un labirinto "quadrato", da una casella
      di entrata a una casella di uscita, senza passare per caselle
      che contengono un mostro. 
      Ci si puo' spostare in orizzontale e in diagonale, ma solo verso
      destra. La casella di ingresso e' nella colonna piu' a sinistra. *)
   (**  in modo da non avere necessita' di loop-checking **)
   
   (* slides => matrice e rappresentazione *)
   (*        0  1  2  3  4
          0        M
          1     M     M
          2           M   
          3  M        
          4        M   
   *)
   
   (* Rappresentazione delle caselle contententi mostri *)
   let mostri = [(0,2);(1,1);(1,3);(2,3);(3,0);(4,2)]
   
   (* Labirinto rappresentato da un intero (la sua dimensione) e
      lista delle caselle contenenti mostri *)
   
   (** Ricerca del cammino nel labirinto mediante backtracking **)
   
   (* Un caso di fallimento: siamo fuori matrice *)
   (* sottoproblema: dato l'indice massimo della matrice e una casella 
       (riga,colonna), determinare se la casella e` nella matrice *)
   (*  in_labirinto : int -> int * int -> bool *)
   let in_labirinto dim (r,c) =
     r >= 0 && c >= 0 && r < dim && c < dim
   
   (* Ricerca del percorso, versione 1 *)
   (* path1: int -> (int * int) list -> 
                  int * int -> int * int -> (int * int) list  *)
   (* path1 dim mostri ingresso uscita = path, dove:
      path e' un cammino (lista di caselle) nella matrice di dimensione dim
        che va da ingresso a uscita senza passare da caselle con il mostro
        (elementi della lista mostri) *)
   (* ricerca mediante backtracking *)
   
   (* per segnalare il fallimento *)
   exception NotFound 
   
   (* funzione ausiliaria: *)
     (* cerca_da: int * int  -> (int * int) list *)
     (* cerca_da casella = cammino senza mostri da casella a uscita *)
   let path1 dim mostri ingresso uscita =
     let rec cerca_da ((r,c) as casella) =
         (* casella e` fuori della matrice o contiene un mostro? *)
       if not (in_labirinto dim casella)
           || List.mem casella mostri 
       then raise NotFound
       else (* siamo arrivati? *)
         if casella = uscita 
         then [casella] (* il cammino contiene solo la casella corrente *)
         else (* passiamo per casella e proseguiamo con 
         una delle caselle accessibili *)
     casella:: (* il cammino inizia con la casella corrente, e poi ... *)
     try cerca_da (r,c+1)
     with NotFound ->
       try cerca_da (r+1,c+1)
       with NotFound -> 
         cerca_da (r-1,c+1)
     in cerca_da ingresso
   
   (**
   # path1 5 mostri (1,0) (1,4);;
   - : (int * int) list = [(1, 0); (2, 1); (1, 2); (0, 3); (1, 4)]
   # path1 5 mostri (0,0) (4,4);;
   Exception: NotFound.
   **)
   
   (* Seconda versione:
      andiamo verso una generalizzazione al caso in cui una casella possa 
      avere un numero qualsiasi di vicini (in questo caso particolare, da 1 
      a 3).
      I vicini sono dunque rappresentati da una lista di caselle.
      Possiamo allora gia' "filtrarla" e conservare solo le caselle
      interne al labirinto *)
   (* sottoproblema: "filtrare" una lista di caselle conservando solo
      quelle interne al labirinto *)
   (* filter_vicini: int -> (int * int) list -> (int * int) list *)
   (* filter_vicini dim lista_caselle = elementi di lista_caselle 
                   che sono interne al labirinto di dimensione dim *)
   let rec filter_vicini dim = function
       [] -> []
     | casella::rest ->
         if in_labirinto dim casella
         then casella::filter_vicini dim rest
         else filter_vicini dim rest
   
   (* sottoproblema: data una casella e la struttura di un labirinto, 
      trovare le caselle interne al labirinto ad essa accessibili.  
      Se la casella non e` nel 
      labirinto, sollevare un'eccezione *)
   (* vicini : int -> int * int -> (int * int) list *)
   (* vicini dim casella = lista delle caselle interne al labirinto
                           vicine a casella *)
   let vicini dim (r,c) =
     if in_labirinto dim (r,c)
     then filter_vicini dim [(r,c+1);(r+1,c+1);(r-1,c+1)]
     else raise NotFound
   
   (* Per usare la lista dei vicini, gia` "filtrata"
      eliminando le caselle che non sono nella matrice, dobbiamo
      generalizzare l'espressione:
         try cerca_da (r,c+1) 
         with NotFound ->
           try cerca_da (r+1,c+1) 
           with NotFound -> 
        cerca_da (r-1,c+1)
      in modo che la ricerca possa proseguire a partire da una
      qualsiasi delle caselle in una lista data
   
      cerca_da si applica a una casella, non puo` applicarsi a una
      lista di caselle.  Dovremmo definire una funzione analoga a
      cerca_da che pero` si applichi a liste di caselle e riportare
          casella::cerca_da_lista (vicini dim casella)
   
      cerca_da_lista deve richiamare cerca_da per ogni elemento della 
      lista.  Quindi:
          cerca_da       usa   cerca_da_lista
          cerca_da_lista usa   cerca_da
      cerca_da_lista lst = un cammino fino all'uscita a partire da una
                           qualsiasi casella in lst
   *)
   
   (* ========================================= *)
   (* MUTUA RICORSIONE
         f chiama g, e g chiama f *)
   (* pari/dispari : int -> bool
         definite solo sui naturali *)
   let rec pari n = 
     n<>1 && (n=0 || dispari (n-1))
   and dispari n =
     n<>0 && (n=1 || pari (n-1));;
   (* ========================================= *)
   
   (* ora assumiamo dunque che le caselle visitate siano sicuramente
      interne al labirinto *)
   (* funzione ausiliaria: *)
     (* cerca_da: int * int -> (int * int) list *)
     (* cerca_da casella = cammino senza mostri da casella a uscita *)
   let path2 dim contents ingresso uscita =
     let rec cerca_da casella  =
         (* il primo caso di fallimento sparisce *)
       if has_monster casella contents
       then raise NotFound
       else 
         if casella = uscita 
         then [casella] 
         else casella::cerca_da_lista (vicini dim casella) 
     (* cerca_da_lista: (int * int) list -> (int * int) list
        cerca_da_lista lista_caselle = un cammino senza mostri da una delle
                caselle in lista_caselle fino a uscita *)
     and cerca_da_lista = function 
         [] ->  raise NotFound
       | c::rest -> (* provo a passare per c, ma se fallisco
                       provo ancora con una delle caselle di rest *)
     try cerca_da c 
     with NotFound -> cerca_da_lista rest 
     in let path = cerca_da ingresso
     in (path,raccolti contents path)
   
   (* cosa succederebbe se non si fosse sempre obbligati a spostarsi a*)
   (* destra, ma ci si potesse spostare in orizzontale, verticale e*)
   (* diagonale in tutte le direzioni? *)
   