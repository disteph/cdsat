(*****************************************)
(* This is the collection of known sorts *)
(*****************************************)

open Format

type t =
  Prop
| Rat
| Array of t*t
| Fun  of t*(t list)
| User of string
| BV of int
            [@@deriving eq, hash, ord]

let rec pp fmt = function
  | Prop -> fprintf fmt "{\\sf prop}"
  | Rat  -> fprintf fmt "{\\mathbb Q}"
  | Array(indices,values) -> fprintf fmt "{\\mathbb Ar}(%a,%a)" pp indices pp values
  | Fun(o,i) ->
    fprintf fmt "(%a\rightarrow %a)"
      pp o
      pp_list i
  | User s -> fprintf fmt "\"%s\"" s
  | BV i -> fprintf fmt "\"BV_{%i}\"" i
and pp_list fmt = function
  | []   -> fprintf fmt "()"
  | [so] -> fprintf fmt "%a" pp so
  | so::l-> fprintf fmt "%a,%a" pp so pp_list l

let show = Dump.stringOf pp

let allsorts declared = Prop :: Rat :: List.map (fun s-> User s) declared
