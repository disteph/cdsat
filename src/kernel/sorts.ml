(*****************************************)
(* This is the collection of known sorts *)
(*****************************************)

open Format

type t = | Prop
         | Rat 
         | Term
         | Fun  of t*(t list)
         | User of string

let rec print_in_fmt fmt = function
  | Prop -> fprintf fmt "{\\sf prop}"
  | Rat  -> fprintf fmt "{\\mathbb Q}"
  | Term -> fprintf fmt "{\\sf term}"
  | Fun(o,i) -> 
    fprintf fmt "(%a\rightarrow %a)"
      print_in_fmt o
      print_in_fmt_list i
  | User s -> fprintf fmt "\"%s\"" s
and print_in_fmt_list fmt = function
  | []   -> fprintf fmt "()"
  | [so] -> fprintf fmt "%a" print_in_fmt so
  | so::l-> fprintf fmt "%a,%a" print_in_fmt so print_in_fmt_list l


let parse = function
  | "Bool" | "bool" | "Prop" | "prop" -> Prop
  | "Rat"  -> Rat
  | "Real" -> Rat
  | "Int"  -> Rat
  | s      -> User s

