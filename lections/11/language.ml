
  type form =
      True
    | False 
    | Prop of string 
    | Not of form
    | And of form * form
    | Or of form * form
    | Imp of form * form

