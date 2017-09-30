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
  (* Extraction of the bits from lo to hi (both included), out of a bitvector of length len *)
  | Extract of { hi:int; lo:int; length:int }
  (* Concatenation of 2 bitvectors of lengths n1 and n2 *)
  | Conc of int*int
  (* BitVector constants, represented as binary strings such as 0110010110 *)
  | CstBV of string
               [@@deriving eq, show, hash, ord]
               
val arity : t -> arity
