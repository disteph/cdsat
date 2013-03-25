(* This is the register of all theories' decprocs in Psyche *)

let bank:(module Theories.ThDecProc)array = 
  [|
    (module Empty.MyTheory);
    (module LRA.MyTheory)
  |]

module StringMaps = Map.Make(String)
open StringMaps

let getbyname = 
  let aux = ref empty in
    aux:= add "empty" bank.(0) !aux;
    aux:= add "propositional" bank.(0) !aux;
    aux:= add "lra" bank.(1) !aux;
    aux:= add "lia" bank.(1) !aux;
    !aux
