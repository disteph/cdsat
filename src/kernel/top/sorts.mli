(*****************************************)
(* This is the collection of known sorts *)
(*****************************************)

type t = | Prop
         | Rat
         | Array of t*t
         | Fun  of t*(t list)
         | User of string
                     [@@deriving eq, hash]

val print_in_fmt : Format.formatter -> t -> unit
val parse        : string list -> Parser.sortType -> t
val allsorts     : string list -> t list
