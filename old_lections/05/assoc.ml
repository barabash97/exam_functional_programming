let lst =
  (1, 23), (2, 26);;

exception NotFound;;

let rec find_by_name k = function
[] -> raise NotFound
| (k1, v)::rest -> if(k1 = k) then v
                    else find_by_name k rest;;

let x = find_by_name 1 lst;;

print_int(x);;
