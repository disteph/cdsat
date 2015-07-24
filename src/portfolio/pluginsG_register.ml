(* This is the register of all generic plugins in Psyche *)

open PluginsG

let bank:(module PluginG.Type)array= 
  [|
    (module Naive.MyPlugin);
    (module DPLL_Pat.MyPlugin);
    (module DPLL_WL.MyPlugin);
    (module Hint.MyPlugin);
  |]

exception NotFound of string

let getbyname = function
  | "naive"    -> bank.(0)
  | "dpll_pat" -> bank.(1)
  | "dpll_wl"  -> bank.(2)
  (* | "restarts" -> let module MyBasePlugin = (val bank.(2)) in *)
  (*                 (module RestartsFunctor.MyPlugin.GenPluginWRestart(MyBasePlugin)) *)
  | "hint"     -> bank.(3)
  | s -> raise (NotFound ("Generic plugin "^s^" does not exist; see -help"))
