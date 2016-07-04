(***********************************************)
(* This is the collection of all known symbols *)
(***********************************************)

open Format
open Parser
open Multiary

type arity = Sorts.t*(Sorts.t list)

type t =

| User of string*arity

(* Prop *)
| True | False | Neg | And | Or | Imp | Xor
| Forall of Sorts.t | Exists of Sorts.t
| IsTrue
                                  
(* General *)
| Eq of Sorts.t | NEq of Sorts.t | ITE of Sorts.t

(* LRA *)
| CstRat of Num.num
| Ge | Le | Gt | Lt
| Plus | Minus | Times | Divide | Op

(* Arrays *)
| Select of Sorts.t*Sorts.t | Store of Sorts.t*Sorts.t

let arity = function
  | User(_,ar)                        -> ar
  | True | False                      -> (Sorts.Prop, [])
  | Neg | Forall _ | Exists _         -> (Sorts.Prop, [Sorts.Prop])
  | And | Or | Imp | Xor              -> (Sorts.Prop, [Sorts.Prop;Sorts.Prop])
  | IsTrue                            -> (Sorts.Prop, [Sorts.Prop])
  | ITE so                            -> (so, [Sorts.Prop;so;so])
  | Eq so | NEq so                    -> (Sorts.Prop, [so;so])
  | CstRat i                          -> (Sorts.Rat, [])
  | Plus | Minus | Times | Divide     -> (Sorts.Rat, [Sorts.Rat;Sorts.Rat])
  | Op                                -> (Sorts.Rat, [Sorts.Rat])
  | Ge | Gt | Le | Lt                 -> (Sorts.Prop, [Sorts.Rat;Sorts.Rat])  
  | Select(indices,values)            -> (values, [Sorts.Array(indices,values);indices])
  | Store(indices,values)             -> (Sorts.Array(indices,values), [Sorts.Array(indices,values); indices; values])

(* val multiary  : symbol -> ((('a list->'a list) -> 'a list -> 'a) option) *)
let multiary = function
  | And | Or | Plus | Times -> Some r_assoc
(*   | NEqRat | EqRat -> None (\* ThSig_tools.pairwise *\) *)
  | _ -> None

let print_in_fmt_latex fmt = function
  | User(f,ar)  -> fprintf fmt "\\mbox{\\small %s}" f
  | True        -> fprintf fmt "\\top"
  | False       -> fprintf fmt "\\bot"
  | Neg         -> fprintf fmt "\\neg"
  | Forall s    -> fprintf fmt "\\forall^{%a}" Sorts.print_in_fmt s
  | Exists s    -> fprintf fmt "\\exists^{%a}" Sorts.print_in_fmt s
  | And         -> fprintf fmt "\\wedge"
  | Or          -> fprintf fmt "\\vee"
  | Imp         -> fprintf fmt "\\Rightarrow"
  | Xor         -> fprintf fmt "\\oplus"
  | IsTrue      -> fprintf fmt "\\mbox{\\small isT}"                           
  | ITE so      -> fprintf fmt "\\mbox{if}"
  | Eq so       -> fprintf fmt "=_{%a}" Sorts.print_in_fmt so
  | NEq so      -> fprintf fmt "\\neq_{%a}" Sorts.print_in_fmt so
  | CstRat i    -> fprintf fmt "%s" (Num.string_of_num i)
  | Plus        -> fprintf fmt "+"
  | Minus       -> fprintf fmt "-"
  | Times       -> fprintf fmt "\\times"
  | Divide      -> fprintf fmt "\\div"
  | Op          -> fprintf fmt "-"
  | Ge          -> fprintf fmt "\\geq"
  | Gt          -> fprintf fmt ">"
  | Le          -> fprintf fmt "\\leq"
  | Lt          -> fprintf fmt "<"
  | Select(_,_) -> fprintf fmt "\\mbox{\\small select}"
  | Store(_,_)  -> fprintf fmt "\\mbox{\\small store}"

let print_in_fmt_utf8 fmt = function
  | User(f,ar)  -> fprintf fmt "%s" f
  | True        -> fprintf fmt "⊤"
  | False       -> fprintf fmt "⊥"
  | Neg         -> fprintf fmt "¬"
  | Forall s    -> fprintf fmt "∀"
  | Exists s    -> fprintf fmt "∃"
  | And         -> fprintf fmt "∧"
  | Or          -> fprintf fmt "∨"
  | Imp         -> fprintf fmt "⇒"
  | Xor         -> fprintf fmt "⊻"
  | IsTrue      -> fprintf fmt "isT"
  | ITE so      -> fprintf fmt "if"
  | Eq so       -> fprintf fmt "="
  | NEq so      -> fprintf fmt "≠"
  | CstRat i    -> fprintf fmt "%s" (Num.string_of_num i)
  | Plus        -> fprintf fmt "+"
  | Minus       -> fprintf fmt "-"
  | Times       -> fprintf fmt "×"
  | Divide      -> fprintf fmt "÷"
  | Op          -> fprintf fmt "-"
  | Ge          -> fprintf fmt "≥"
  | Gt          -> fprintf fmt ">"
  | Le          -> fprintf fmt "≤"
  | Lt          -> fprintf fmt "<"
  | Select(_,_) -> fprintf fmt "select"
  | Store(_,_)  -> fprintf fmt "store"

let print_in_fmt fmt = match !Dump.display with
  | Dump.Latex -> print_in_fmt_latex fmt
  | _ -> print_in_fmt_utf8 fmt

let parse decsorts = 
  let allsorts = Sorts.allsorts decsorts in function
    | "true"  -> [True]
    | "false" -> [False]
    | "not"   -> [Neg]
    | "and"   -> [And]
    | "or"    -> [Or]
    | "imp" | "=>" -> [Imp]
    | "xor"   -> [Xor]
    | "<=>"   -> [Eq Sorts.Prop]
    | "=" | "==" | "eq"                -> List.map (fun so -> Eq so) allsorts
    | "!=" | "<>" | "neq" | "distinct" -> List.map (fun so -> NEq so) allsorts
    | "ite"                            -> List.map (fun so -> ITE so) allsorts
    | "+" -> [Plus]
    | "-" -> [Minus;Op]
    | "*" -> [Times]
    | "/" -> [Divide]
    | ">" -> [Gt]
    | "<" -> [Lt]
    | ">=" ->[Ge]
    | "<=" ->[Le]
    | "select" -> let aux ind = List.map (fun so -> Select(ind,so)) allsorts
                  in List.fold_left (fun accu ind -> (aux ind)@accu) [] allsorts
    | "store"  -> let aux ind = List.map (fun so -> Store(ind,so)) allsorts
                  in List.fold_left (fun accu ind -> (aux ind)@accu) [] allsorts
    | s    -> try [CstRat(Num.num_of_string s)] with _ -> []
