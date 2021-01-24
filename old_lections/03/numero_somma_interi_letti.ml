

let rec somma_numeri() = 
  let s = read_line() in 
    if s = "." then (0,0)
    else 
      let (tot,somma) = somma_numeri()
      in (tot+1, somma + (int_of_string s));;


let (totale, somma) = somma_numeri();;
print_string("\n\nTotale: ");
print_int(totale);;
print_string("\n\nSomma: ");
print_int(somma);;

print_string("\n\n\n");;
let rec somma_numeri() = 
  try let n = int_of_string(read_line())
    in let (tot, somma) = somma_numeri() 
    in (tot+1, somma + n)
  with _ -> (0,0);;


let (totale, somma) = somma_numeri();;
print_string("\n\nTotale: ");
print_int(totale);;
print_string("\n\nSomma: ");
print_int(somma);;




















