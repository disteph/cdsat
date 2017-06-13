(* This is the register of all parsers in Psyche *)

type t =
  | SMTLib2
  | DIMACS
[@@deriving show, enumerate]

let get : t -> (module Parser.Type) = function
  | SMTLib2 -> (module SMTLib2)
  | DIMACS ->  (module DIMACS)

exception NotFound of string

let parse = function
  | "smtlib2"   -> SMTLib2
  | "dimacs"    -> DIMACS
  | s -> raise (NotFound ("Parser "^s^" does not exist; see -help"))
