/* ================= parser.mly ================== */

%{
open Language
%}

%token <string> ATOM
%token TRUE FALSE NOT AND OR IMP IFF RPAREN LPAREN EOF
%left IFF
%left IMP
%left OR
%left AND
%nonassoc NOT 
%start  formula
%type <Language.form>  formula
%%
formula :
    ATOM                       { Prop($1) }
  | TRUE                       { True }
  | FALSE                      { False }
  | NOT formula                { Not $2}
  | formula IFF formula        { And(Imp($1,$3),Imp($3,$1)) }
  | formula IMP formula        { Imp($1,$3) }
  | formula OR formula         { Or($1,$3) }
  | formula AND formula        { And($1,$3) }
  | LPAREN formula RPAREN      { $2 }
;


