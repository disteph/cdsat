(* This is the register of all parsers in Psyche *)
open Parsers

let bank:(module Top.Parser.ParserType)array= 
  [|
    (module SMTLib2);
    (module DIMACS);
  |]
