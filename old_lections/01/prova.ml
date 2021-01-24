let spazio = print_string("\n\n\n\n\n\nspazio");;

let b = (1,100);;
let provapf (n, m) = 
  if n < m
  then 3*111
  else 3*100;;

let variable = provapf b;;

spazio;;
print_int(variable);;
let spazio = print_string("\n\n\n\n\n\nspazio");;
spazio;;


print_int(fst b);;
print_int(snd b);;
print_string("\n\n\n\n\n\n");;
print_int(snd (1,2));;

print_string("\n\n\n\n\n\n");;

let rec stampa_stringa (s,i) = 
  if(i <= String.length s) then print_string(s)
  else stampa_stringa(s, (i+1));;

  let stringa = "ciao mondo";;

 stampa_stringa (stringa, 0);; 