let rec fact = function 0 -> 1
| n -> n * fact(n-1);;
let num_fact = 5;;
print_string("\n\nFACT ");;
print_int(num_fact);
print_string(" = ");;
print_int(fact num_fact);;

let fact_one n =
  let rec aux f = function 
  0 -> f
  | n -> aux (f*n)(n-1)
in aux 1 n;;

let rec aux f = function 
  0 -> f
  | n -> aux (f*n)(n-1);;

print_string("\n\n");
print_int(aux 6 num_fact);
print_string("\n\n");;





let fact_two n = 
  let rec aux f = function
    0 -> f
    | n -> aux (f*n)(n-1)
  in aux 1 n;;


print_int(fact_two 4);


print_string("\n\n\n\n\n\n\n\n\n");;


let numeric c = c >= '0' && c <= '9';;

let conta_digits s = 
  let rec loop i = 
    try if numeric s.[i] then 1 + loop(i+1)
    else loop (1+i)
    with _ -> 0
  in loop 0;;

print_int(conta_digits "1232a22a");;

let conta_digits_iterativo s = 
  let rec loop i result = 
    try if numeric s.[i] then loop (i+1) (result+1)
    else loop (i+1) (result)
    with _ -> (result) (* Si crea un'eccezione, significa stringa terminata *)
      in loop 0 0;;

  print_string("\n\n iterativo \n\n");;
  print_int(conta_digits_iterativo "1232a22a");;

  print_string("\n\n fine iterativo \n\n");;

  let x = read_line();;

  print_string("STAMPA : " ^ x);;

  let rec somma () = 
    let s = read_line()
    in if s="." then 0
    else (int_of_string(s) + somma());;

let res = somma();;

print_int(res);;

let rec somma() = 
  let rec somma_aux res = 
    let s = read_line();
    in 
    if(s = ".") then res
    else somma_aux (int_of_string(s) + res)
    
  in somma_aux 0;;

  let res = somma();;

print_int(res);;