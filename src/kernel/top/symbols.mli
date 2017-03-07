(***********************************************)
(* This is the collection of all known symbols *)
(***********************************************)

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
| CstRat of Num.num
| Ge | Le | Gt | Lt
| Plus | Minus | Times | Divide | Op

(* Arrays *)
| Select of Sorts.t*Sorts.t | Store of Sorts.t*Sorts.t
                                                 [@@deriving eq, hash]
                                                 
val arity        : t -> arity
val multiary     : t -> ((('a list->'a list) -> 'a list -> 'a) option)
val print_in_fmt : Format.formatter -> t -> unit
val parse        : string list -> string -> t list
