open Language

(* string -> Language.form *)
let parse stringa =
  let lexbuf = Lexing.from_string stringa
  in Parser.formula Lexer.token lexbuf

(* Language.form -> string *)
let rec f2s = function
    True -> "T"
  | False -> "F"
  | Prop p -> p
  | Not f -> "-"^(f2s f)
  | And(f,g) -> String.concat "" ["(";f2s f;"&"; f2s g;")"]
  | Or(f,g) -> String.concat "" ["(";f2s f;"v"; f2s g;")"]
  | Imp(f,g) -> String.concat "" ["(";f2s f;"->"; f2s g;")"]

let rec loop() =
  print_string "\nScrivi una formula: ";
  let stringa=read_line()
  in if stringa="" then ()
  else ((try (print_string(f2s (parse stringa)))
        with Parsing.Parse_error -> 
	  print_string "Syntax error\n");
	loop())

let _=loop()
