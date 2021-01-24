type direzione = Su | Giu | Destra | Sinistra;;

type posizione = Pos of int * int * direzione;;



let xcoord (x, _, _) = x;;
let ycoord (_, y, _) = y;;
let dir (_, _, d) = d;;

type azione = Gira | Avanti of int;;

let int_of_act = function 
  Avanti n -> n 
  | _ -> failwith "int_of_act";;

let gira = function 
  Su -> Destra
  | Giu -> Sinistra
  | Destra -> Giu
  | Sinistra -> Su;;

let avanti (x, y, dir) n = 
  match dir with
  Su -> (x, y+n, dir)
  | Giu -> (x, y-n, dir)
  | Sinistra -> (x-n, y, dir)
  | Destra -> (x+n, y, dir);;

let sposta (x,y,dir) act = 
  match act with 
  Gira -> (x,y, gira dir)
  | Avanti n -> avanti (x,y,dir) n;;

let (x,y,dir) =  (1,1,Su);;

let azione_scelta = Avanti 3;;
let seconda_pos = sposta (x,y,dir), azione_scelta;;

let (x,y,z) = seconda_pos;;

print_string("X: ");;
print_int(x);;
print_string(" Y: ");;
print_int(y);;
print_string(" Posizione: ");;