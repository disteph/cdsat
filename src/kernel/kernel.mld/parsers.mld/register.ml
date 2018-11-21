open Top

(* This is the register of all parsers in Psyche *)

type t =
  | SMTLib2
  | DIMACS
[@@deriving show, enumerate]

let get : t -> (module Parser.Type) = function
  | SMTLib2 -> (module SMTLib2)
  | DIMACS ->  (module DIMACS)

exception NotFound of string

let parse_name = function
  | "smtlib2"   -> SMTLib2
  | "dimacs"    -> DIMACS
  | s -> raise (NotFound ("Parser "^s^" does not exist; see -help"))

let parse parser input =
  let (module MyParser) = get parser in
  let aft = MyParser.glance input in
  
  (* Now we parse *)
  let i = (module Typing.ForParsing
                  : Parser.ForParsing with type t = Terms.TermB.t)
  in
  let parsed, expected = MyParser.parse aft (Typing.forParser i) in
  let aux formula = assert(Sorts.equal (Terms.TermB.get_sort formula) Sorts.Prop) in
  List.iter aux parsed;
  (* print_endline("We want to prove: "^List.show Terms.TermB.pp parsed); *)
  MyParser.guessThDecProc aft,parsed,expected
