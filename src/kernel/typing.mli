(***************************************************)
(* Specifications and tools for parsing and typing *)
(***************************************************)
open Top
open Parser

exception TypingError of string

val forParser :
  (module Specs.ForParsing with type t = 'a)
  -> string list
  -> (module InterpretType with type t = Sorts.t -> 'a)
