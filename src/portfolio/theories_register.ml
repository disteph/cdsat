(* This is the register of all theories' decprocs in Psyche *)

open Theories
open Theories_tools

let bank:(module Theory.Type)array = 
  [|
    (module ForGround.GTh2Th(Empty.MyTheory));
    (* (module ForGround.GDecProc2DecProc(LRA.MyTheory)); *)
    (* (module ForGround.GDecProc2DecProc(CC.MyTheory)); *)
    (module FirstOrder.MyTheory);
  |]

exception NotFound of string

let getbyname = function
  | "empty" | "propositional"
      -> bank.(0)
  (* | "lra" | "LRA" | "QF_LRA" *)
  (* | "lia" | "LIA" | "QF_LIA" *)
  (*     -> bank.(1) *)
  (* | "emptyCC" | "QF_UF" *)
  (*     -> bank.(2) *)
  | "FO" 
      -> bank.(1)
  | s -> raise (NotFound ("Theory "^s^" does not exist; see -help"))
