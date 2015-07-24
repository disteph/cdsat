(* This is the register of all theories' decprocs in Psyche *)
open Kernel

let bank (type a) (propds:(module Top.Specs.Semantic with type t = a))
:(module Prop.Interfaces_theory.DecProc with type DS.formulaF = a) array = 

  let module ThPlugin = Plugins.Basic in
  let module WB = (val Hub.make propds Hub.HandlersMap.empty ThPlugin.datalist) in
  [|
    (module ForGround.GTh2Th(WB)(ThPlugin.Strategy(WB)));
  (* FirstOrder.make propds; *)
  |]

exception NotFound of string

let getbyname = function
  | "prop" | "bool"  -> 0
  | "FO" -> 1
  | s -> raise (NotFound ("Theory "^s^" does not exist; see -help"))
