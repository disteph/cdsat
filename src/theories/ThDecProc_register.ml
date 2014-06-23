(* This is the register of all theories' decprocs in Psyche *)

open ThDecProc_tools

let bank:(module Theories.ThDecProc)array = 
  [|
    (module GDecProc2DecProc(Empty.MyTheory));
    (module GDecProc2DecProc(LRA.MyTheory));
    (module GDecProc2DecProc(CC.MyTheory))
  |]

exception NotFound of string

let getbyname = function
  | "empty" | "propositional"
      -> bank.(0)
  | "lra" | "LRA" | "QF_LRA"
  | "lia" | "LIA" | "QF_LIA"
      -> bank.(1)
  | "emptyCC" | "QF_UF"
      -> bank.(2)
  | s -> raise (NotFound ("Theory "^s^" does not exist; see -help"))
