(* This is the register of all parsers in Psyche *)

let bank:(module Parsers.Type)array= 
  [|
    (module SMTLib2);
    (module DIMACS);
  |]

module StringMaps = Map.Make(String)
open StringMaps

let getbyname = 
  let aux = ref empty in
    aux:= add "smtlib2"  bank.(0) !aux;
    !aux