(***********************************************)
(* This is the collection of all known symbols *)
(***********************************************)

open Format
open Multiary

type t =

| User of string*(Sorts.t*(Sorts.t list))

(* Prop *)
| True | False | Neg | And | Or | Imp | Xor  (* | ITEProp *)
| Forall of Sorts.t | Exists of Sorts.t

(* General *)
| Eq of Sorts.t | NEq of Sorts.t

(* LRA *)
| CstRat of Num.num
| Ge | Le | Gt | Lt
| Plus | Minus | Times | Divide | Op (* | ITERat *)


let arity = function
  | User(_,ar)                        -> ar
  | True | False                      -> (Sorts.Prop, [])
  | Neg | Forall _ | Exists _         -> (Sorts.Prop, [Sorts.Prop])
  | And | Or | Imp | Xor              -> (Sorts.Prop, [Sorts.Prop;Sorts.Prop])
  (* | ITEProp -> (Sorts.Prop, [Sorts.Prop;Sorts.Prop;Sorts.Prop]) *)
  | Eq so | NEq so                    -> (Sorts.Prop, [so;so])
  | CstRat i                          -> (Sorts.Rat, [])
  | Plus | Minus | Times | Divide     -> (Sorts.Rat, [Sorts.Rat;Sorts.Rat])
  | Op                                -> (Sorts.Rat, [Sorts.Rat])
  (* | ITERat-> (Sorts.Rat, [Sorts.Prop;Sorts.Rat;Sorts.Rat]) *)
  | Ge | Gt | Le | Lt                 -> (Sorts.Prop, [Sorts.Rat;Sorts.Rat])  
  (* | _        -> raise (Match_failure("Could not find connective ",0,0)) *)


(* val multiary  : symbol -> ((('a list->'a list) -> 'a list -> 'a) option) *)
let multiary = function
  | And         -> Some r_assoc
  | Or          -> Some r_assoc
(*   | NEqRat | EqRat -> None (\* ThSig_tools.pairwise *\) *)
  | _ -> None

let print_in_fmt fmt = function
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
  (* | ITEProp -> (Prop, [Prop;Prop;Prop]) *)
  | Eq so       -> fprintf fmt "=_{%a}" Sorts.print_in_fmt so
  | NEq so      -> fprintf fmt "\\neq_{%a}" Sorts.print_in_fmt so
  | CstRat i    -> fprintf fmt "%s" (Num.string_of_num i)
  | Plus        -> fprintf fmt "+"
  | Minus       -> fprintf fmt "-"
  | Times       -> fprintf fmt "\\times"
  | Divide      -> fprintf fmt "\\div"
  | Op          -> fprintf fmt "-"
    (* | ITERat-> (Rat, [Prop;Rat;Rat]) *)
  | Ge          -> fprintf fmt "\\geq"
  | Gt          -> fprintf fmt ">"
  | Le          -> fprintf fmt "\\leq"
  | Lt          -> fprintf fmt "<"


let parse = function
  | "true"  -> [True]
  | "false" -> [False]
  | "not"   -> [Neg]
  | "and"   -> [And]
  | "or"    -> [Or]
  | "imp" | "=>" -> [Imp]
  | "xor"   -> [Xor]
  | "<=>"   -> [Eq Sorts.Prop]
  | "=" | "==" | "eq" ->[Eq Sorts.Prop;Eq Sorts.Rat; Eq (Sorts.User "")]
  | "!=" | "<>" | "neq" | "distinct" ->[NEq Sorts.Prop;NEq Sorts.Rat; NEq (Sorts.User "")]
    (* | "ite"   -> [ITEProp] *)
  | "+" -> [Plus]
  | "-" -> [Minus;Op]
  | "*" -> [Times]
  | "/" -> [Divide]
  | ">" -> [Gt]
  | "<" -> [Lt]
  | ">=" ->[Ge]
  | "<=" ->[Le]
    (* | "ite"->[ITERat] *)
  | s    -> (try [CstRat(Num.num_of_string s)] with _ -> [])
