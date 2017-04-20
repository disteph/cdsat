(* This is the register of all parsers in Psyche *)

val all_parsers : (module Parser.Type)list

exception NotFound of string

val get : string -> (module Parser.Type)
