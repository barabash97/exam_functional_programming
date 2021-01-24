type number = Int of int | Float of float;;

let sum = function 
(Int x, Int y) -> Int(x+y)
| (Float x, Int y) -> Float( +. (float y))
| (Int x, Float y) -> Float((float x) +.y)
| (Float x, Float y) -> Float(x +. y);;

let x = Int 3;; 
let y = Int 3;;

let somma = sum (x,y);;

print_string(somma);;