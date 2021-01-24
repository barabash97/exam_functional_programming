(* ================== lexer.mll ===================== *)

{
open Parser
} 

rule token = parse
  [' ' '\t' '\n' ] {token lexbuf }
| "T" { TRUE }
| "F" { FALSE }
| '&' { AND }
| '|'  { OR }
| "=>" | "->" { IMP } 
| "<=>" | "<->" {IFF }
| '-'  { NOT }
| '(' {  LPAREN }
| ')' {  RPAREN }
| ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
    { ATOM(Lexing.lexeme lexbuf) }
| eof  { EOF }
| _  { print_string "Token sconosciuto ignorato\n ";
       token lexbuf }


