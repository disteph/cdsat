(**********************)
(* Printing functions *)
(**********************)

val init : (string * int * bool) list -> unit

val toString : ((('a, Format.formatter, unit) format -> 'a) -> 'b) -> string
val stringOf : (Format.formatter -> 'a -> unit) -> 'a -> string
val print : (string * int) list -> ((('a, Format.formatter, unit) format -> 'a) -> 'b) -> unit
                      
val wait : unit -> unit
