(***********************************************)
(* This is the collection of all known symbols *)
(***********************************************)

open Format
open General
       
type arity = Sorts.t*(Sorts.t list) [@@deriving eq, hash, ord]


type t =

  | User of Stringhashed.t*arity

  (* Prop *)
  | True | False | Neg | And | Or | Imp | Xor
  | Forall of Sorts.t | Exists of Sorts.t

  (* General *)
  | Eq of Sorts.t | NEq of Sorts.t | ITE of Sorts.t

  (* LRA *)
  | CstRat of Qhashed.t
  | Ge | Le | Gt | Lt
  | Plus | Minus | Times | Divide | Op

  (* Arrays *)
  | Select of {indices:Sorts.t;values:Sorts.t}
  | Store of {indices:Sorts.t;values:Sorts.t}
  | Diff of {indices:Sorts.t;values:Sorts.t}

  (* BitVectors *)
  | BVextract of { hi:int; lo:int; length:int } (* extraction *)
  | BVconc of int*int (* concatenation *)

  | BVcst of Bv_value.t (* constants *)

  (* bitwise operations *)
  | BVnot of int
  | BVand of int
  | BVor  of int
  | BVxor of int

  (* arithmetic operations *)
  | BVneg of int     (* 2^m - x *)
  (* next operation takes 2 bv of same width, output in the same width *)
  | BVadd of int (* addition *)
  | BVmul of int (* multiplication *)
  | BVudiv of int (* division *)
  | BVurem of int (* remainder of division *)
  | BVshl of int (* shift left *)
  | BVshr of int (* shift right *)
  (* produces a Boolean *)
  | BVult of int (* unsigned less than *)
[@@deriving eq, hash, ord]

let arity = function
  | User(_,ar)                        -> ar
  | True | False                      -> Sorts.Prop, []
  | Neg | Forall _ | Exists _         -> Sorts.Prop, [Sorts.Prop]
  | And | Or | Imp | Xor              -> Sorts.Prop, [Sorts.Prop;Sorts.Prop]
  | ITE so                            -> so, [Sorts.Prop;so;so]
  | Eq so | NEq so                    -> Sorts.Prop, [so;so]
  | CstRat i                          -> Sorts.Rat, []
  | Plus | Minus | Times | Divide     -> Sorts.Rat, [Sorts.Rat;Sorts.Rat]
  | Op                                -> Sorts.Rat, [Sorts.Rat]
  | Ge | Gt | Le | Lt                 -> Sorts.Prop, [Sorts.Rat;Sorts.Rat]
  | Select{indices;values}            -> values, [Sorts.Array(indices,values);indices]
  | Store{indices;values}             -> Sorts.Array(indices,values), [Sorts.Array(indices,values); indices; values]
  | Diff{indices;values}              -> indices, [Sorts.Array(indices,values); Sorts.Array(indices,values);]
  | BVextract{hi;lo;length}           -> Sorts.BV (Compare.max [%ord:int] (hi-lo+1) 0), [Sorts.BV length]
  | BVconc(l1,l2)                     -> Sorts.BV(l1+l2), [Sorts.BV l1; Sorts.BV l2]
  | BVcst v                           -> Sorts.BV(Bv_value.width v), []
  | BVnot i
  | BVneg i                           -> Sorts.BV i, [Sorts.BV i]
  | BVand i     
  | BVor i
  | BVxor i
  | BVadd i
  | BVmul i
  | BVudiv i
  | BVurem i
  | BVshl i
  | BVshr i                        -> Sorts.BV i, [Sorts.BV i;Sorts.BV i]
  | BVult i                        -> Sorts.Prop, [Sorts.BV i;Sorts.BV i]


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
  | ITE so      -> fprintf fmt "\\mbox{if}"
  | Eq so       -> fprintf fmt "=_{%a}" Sorts.pp so
  | NEq so      -> fprintf fmt "\\neq_{%a}" Sorts.pp so
  | CstRat i    -> fprintf fmt "%a" Qhashed.pp i
  | Plus        -> fprintf fmt "+"
  | Minus       -> fprintf fmt "-"
  | Times       -> fprintf fmt "\\times"
  | Divide      -> fprintf fmt "\\div"
  | Op          -> fprintf fmt "-"
  | Ge          -> fprintf fmt "\\geq"
  | Gt          -> fprintf fmt ">"
  | Le          -> fprintf fmt "\\leq"
  | Lt          -> fprintf fmt "<"
  | Select _    -> fprintf fmt "\\mbox{\\small select}"
  | Store _     -> fprintf fmt "\\mbox{\\small store}"
  | Diff _      -> fprintf fmt "\\mbox{\\small diff}"
  | BVextract{hi;lo} when hi = lo -> fprintf fmt "\\mbox{\\small bit}_{%i}" hi
  | BVextract{hi;lo} -> fprintf fmt "\\mbox{\\small bits}_{%i:%i}" hi lo
  | BVconc(l1,l2)    -> fprintf fmt "\\circl"
  | BVcst v     -> Bv_value.pp fmt v
  | BVnot _     -> fprintf fmt "~:"
  | BVneg _     -> fprintf fmt "op:"
  | BVand _     -> fprintf fmt "∧:"
  | BVor _      -> fprintf fmt "∨:"
  | BVxor _     -> fprintf fmt "⊻:"
  | BVadd _     -> fprintf fmt "+:"
  | BVmul _     -> fprintf fmt "×:"
  | BVudiv _    -> fprintf fmt "÷:"
  | BVurem _    -> fprintf fmt "urem:"
  | BVshl _     -> fprintf fmt "<<:"
  | BVshr _     -> fprintf fmt ">>:"
  | BVult _     -> fprintf fmt "<:"

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
  | ITE so      -> fprintf fmt "if"
  | Eq so       -> fprintf fmt "="
  | NEq so      -> fprintf fmt "≠"
  | CstRat i    -> fprintf fmt "%a" Qhashed.pp i
  | Plus        -> fprintf fmt "+"
  | Minus       -> fprintf fmt "-"
  | Times       -> fprintf fmt "×"
  | Divide      -> fprintf fmt "÷"
  | Op          -> fprintf fmt "-"
  | Ge          -> fprintf fmt "≥"
  | Gt          -> fprintf fmt ">"
  | Le          -> fprintf fmt "≤"
  | Lt          -> fprintf fmt "<"
  | Select _    -> fprintf fmt "select"
  | Store _     -> fprintf fmt "store"
  | Diff _      -> fprintf fmt "diff"
  | BVextract{hi;lo} when hi = lo -> fprintf fmt "bit[%i]" hi
  | BVextract{hi;lo} -> fprintf fmt "bits[%i:%i]" hi lo
  | BVconc(l1,l2)    -> fprintf fmt "⚬"
  | BVcst v     -> Bv_value.pp fmt v
  | BVnot _     -> fprintf fmt "~:"
  | BVneg _     -> fprintf fmt "op:"
  | BVand _     -> fprintf fmt "∧:"
  | BVor _      -> fprintf fmt "∨:"
  | BVxor _     -> fprintf fmt "⊻:"
  | BVadd _     -> fprintf fmt "+:"
  | BVmul _     -> fprintf fmt "×:"
  | BVudiv _    -> fprintf fmt "÷:"
  | BVurem _    -> fprintf fmt "urem:"
  | BVshl _     -> fprintf fmt "<<:"
  | BVshr _     -> fprintf fmt ">>:"
  | BVult _     -> fprintf fmt "<:"

let pp fmt = match !Dump.display with
  | Dump.Latex -> print_in_fmt_latex fmt
  | _ -> print_in_fmt_utf8 fmt

let show = Print.stringOf pp
