(***********************************************)
(* This is the collection of all known symbols *)
(***********************************************)

type arity = Sorts.t*(Sorts.t list) [@@deriving eq, hash]

type t =

  | User of string*arity

  (* Prop *)
  | True | False | Neg | And | Or | Imp | Xor
  | Forall of Sorts.t | Exists of Sorts.t

  (* General *)
  | Eq of Sorts.t | NEq of Sorts.t | ITE of Sorts.t

  (* LRA *)
  | CstRat of Q.t
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

[@@deriving eq, show, hash, ord]
               
val arity : t -> arity
