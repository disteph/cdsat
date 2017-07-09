(*****************************************)
(* This is the collection of known sorts *)
(*****************************************)

type t = | Prop
         | Rat
         | Array of t*t
         | Fun  of t*(t list)
         | User of string
         | BV of int
                     [@@deriving eq, hash, show, ord]

val allsorts : string list -> t list
