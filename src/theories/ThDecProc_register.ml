(* This is the register of all theories' decprocs in Psyche *)

let bank:(module Theories.ThDecProc)array = 
  [|
    (module Empty.MyTheory);
    (module LRA.MyTheory)
  |]

exception NotFound of string

let getbyname = function
  | "empty" | "propositional"
      -> bank.(0)
  | "lra" | "LRA" | "QF_LRA"
  | "lia" | "LIA" | "QF_LIA"
      -> bank.(1)
  | s -> raise (NotFound ("Theory "^s^" does not exist; see -help"))
