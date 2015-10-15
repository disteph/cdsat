(*****************************************)
(* This is the collection of known sorts *)
(*****************************************)

open Format
open Parser

type t = | Prop
         | Rat
         | Array of t*t
         | Fun  of t*(t list)
         | User of string

let rec print_in_fmt fmt = function
  | Prop -> fprintf fmt "{\\sf prop}"
  | Rat  -> fprintf fmt "{\\mathbb Q}"
  | Array(indices,values) -> fprintf fmt "{\\mathbb Ar}(%a,%a)" print_in_fmt indices print_in_fmt values
  | Fun(o,i) ->
    fprintf fmt "(%a\rightarrow %a)"
      print_in_fmt o
      print_in_fmt_list i
  | User s -> fprintf fmt "\"%s\"" s
and print_in_fmt_list fmt = function
  | []   -> fprintf fmt "()"
  | [so] -> fprintf fmt "%a" print_in_fmt so
  | so::l-> fprintf fmt "%a,%a" print_in_fmt so print_in_fmt_list l


let rec parse declared (Sort(s,l)) = match s with
  | "Bool" | "bool" | "Prop" | "prop"
    -> (match l with
    | [] -> Prop
    | _  -> raise (ParsingError("Sort "^s^" is applied to "^string_of_int(List.length l)^" arguments")))
  | "Rat"  | "Real" | "Int"
    -> (match l with
    | [] -> Rat
    | _  -> raise (ParsingError("Sort "^s^" is applied to "^string_of_int(List.length l)^" arguments")))
  | "Array"
    -> (match l with
    | s1::s2::[] -> let so1,so2 = parse declared s1, parse declared s2
                    in Array(so1,so2)
    | _  -> raise (ParsingError("Sort "^s^" is applied to "^string_of_int(List.length l)^" arguments")))
  | s when List.mem s declared -> User s
  | s -> raise (ParsingError("Cannot understand "^s^" as a sort: not declared"))

let allsorts declared = Prop :: Rat :: List.map (fun s-> User s) declared
