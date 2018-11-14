include module type of MLBDD

val dtrue : t
val dfalse : t
val ithvar : var -> t
  
(* Printing a BDD in Latex.
Takes as parameter the printing function for a BDD node,
which in the implementation is just an int *)
                    
val pp : (Format.formatter -> int -> unit) -> Format.formatter -> t -> unit
val to_string : (Format.formatter -> int -> unit) -> t -> string
