(***********************************************)
(* This is the collection of all known symbols *)
(***********************************************)

open Format
       
type arity = Sorts.t*(Sorts.t list) [@@deriving eq, hash]

type t =

  | User of string*arity

  (* Prop *)
  | True | False | Neg | And | Or | Imp | Xor
  | Forall of Sorts.t | Exists of Sorts.t
  | IsTrue

  (* General *)
  | Eq of Sorts.t | NEq of Sorts.t | ITE of Sorts.t

  (* LRA *)
  | CstRat of General.Pnum.t
  | Ge | Le | Gt | Lt
  | Plus | Minus | Times | Divide | Op

  (* Arrays *)
  | Select of Sorts.t*Sorts.t | Store of Sorts.t*Sorts.t
                                                   [@@deriving eq, hash]

let arity = function
  | User(_,ar)                        -> ar
  | True | False                      -> Sorts.Prop, []
  | Neg | Forall _ | Exists _         -> Sorts.Prop, [Sorts.Prop]
  | And | Or | Imp | Xor              -> Sorts.Prop, [Sorts.Prop;Sorts.Prop]
  | IsTrue                            -> Sorts.Prop, [Sorts.Prop]
  | ITE so                            -> so, [Sorts.Prop;so;so]
  | Eq so | NEq so                    -> Sorts.Prop, [so;so]
  | CstRat i                          -> Sorts.Rat, []
  | Plus | Minus | Times | Divide     -> Sorts.Rat, [Sorts.Rat;Sorts.Rat]
  | Op                                -> Sorts.Rat, [Sorts.Rat]
  | Ge | Gt | Le | Lt                 -> Sorts.Prop, [Sorts.Rat;Sorts.Rat]
  | Select(indices,values)            -> values, [Sorts.Array(indices,values);indices]
  | Store(indices,values)             -> Sorts.Array(indices,values), [Sorts.Array(indices,values); indices; values]


let print_in_fmt_latex fmt = function
  | User(f,ar)  -> fprintf fmt "\\mbox{\\small %s}" f
  | True        -> fprintf fmt "\\top"
  | False       -> fprintf fmt "\\bot"
  | Neg         -> fprintf fmt "\\neg"
  | Forall s    -> fprintf fmt "\\forall^{%a}" Sorts.pp s
  | Exists s    -> fprintf fmt "\\exists^{%a}" Sorts.pp s
  | And         -> fprintf fmt "\\wedge"
  | Or          -> fprintf fmt "\\vee"
  | Imp         -> fprintf fmt "\\Rightarrow"
  | Xor         -> fprintf fmt "\\oplus"
  | IsTrue      -> fprintf fmt "\\mbox{\\small isT}"                           
  | ITE so      -> fprintf fmt "\\mbox{if}"
  | Eq so       -> fprintf fmt "=_{%a}" Sorts.pp so
  | NEq so      -> fprintf fmt "\\neq_{%a}" Sorts.pp so
  | CstRat i    -> fprintf fmt "%a" General.Pnum.pp i
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
  | CstRat i    -> fprintf fmt "%a" General.Pnum.pp i
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

let pp fmt = match !Dump.display with
  | Dump.Latex -> print_in_fmt_latex fmt
  | _ -> print_in_fmt_utf8 fmt

let show = Dump.stringOf pp
