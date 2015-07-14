(* This is the register of all theories' decprocs in Psyche *)

let bank (type a) (propds:(module Top.Specs.Semantic with type t = a)) 
:(module Prop.Interfaces_theory.DecProc with type DS.formulaF = a) array = 

  let module M = (val Manager.make propds []) in

  [|
    (module ForGround.GTh2Th(M));
  (* FirstOrder.make propds; *)
  |]

exception NotFound of string

let getbyname = function
  | "prop" | "bool"  -> 0
  | "FO" -> 1
  | s -> raise (NotFound ("Theory "^s^" does not exist; see -help"))
