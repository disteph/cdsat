(*****************************************)
(* This is the collection of known sorts *)
(*****************************************)

type t = | Prop
         | Rat
         | Array of t*t
         | Fun  of t*(t list)
         | User of string
                     [@@deriving eq, hash, show]

val allsorts : string list -> t list
