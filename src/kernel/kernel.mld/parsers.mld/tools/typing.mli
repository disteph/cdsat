(***************************************************)
(* Specifications and tools for parsing and typing *)
(***************************************************)
open Top

exception TypingError of string

val forParser :
  (module Specs.ForParsing with type t = 'a)
  -> string list
  -> (module Parser.InterpretType with type t = Sorts.t -> 'a)
