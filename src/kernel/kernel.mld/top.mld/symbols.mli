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
  | Select of Sorts.t*Sorts.t | Store of Sorts.t*Sorts.t | Diff of Sorts.t*Sorts.t

  (* BitVectors *)
  | Extract of int*int*int
  | Conc of int*int
  | CstBV of string
               [@@deriving eq, show, hash, ord]
               
val arity : t -> arity
