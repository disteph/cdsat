(* This is the register of all parsers in Psyche *)

let bank:(module Parser.Type)array= 
  [|
    (module SMTLib2);
    (module DIMACS);
  |]

let all_parsers = Array.to_list bank

exception NotFound of string

let parse = function
  | "smtlib2"   -> 0
  | "dimacs"    -> 1
  | s -> raise (NotFound ("Parser "^s^" does not exist; see -help"))

let get s = bank.(parse s)
