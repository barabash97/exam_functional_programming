type direzione = Su | Giu | Sinistra | Destra;;


let valore = function 
  Su -> 1
  | Giu -> 2
  | Sinistra -> 3
  | Destra -> 4;;

print_int(valore Giu);