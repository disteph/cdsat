(**********************)
(* Printing functions *)
(**********************)

val init     : (string * int * bool) list -> unit
val print    : (string * int) list -> ('a Format.spec -> unit) -> unit
val wait     : unit -> unit
val stringOf : 'a Format.printer -> 'a -> string
