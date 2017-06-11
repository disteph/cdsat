(* This is the register of all generic plugins in Psyche *)

let bank:(module PluginG.Type)array= 
  [|
    (module Naive.MyPlugin);
    (module DPLL_Pat.MyPlugin);
    (module DPLL_WL.MyPlugin);
    (module DPLL.MyPlugin);
    (module Hint.MyPlugin);
  |]

exception NotFound of string

let parse = function
  | "naive"    -> 0
  | "dpll_pat" -> 1
  | "dpll_wl"  -> 2
  | "dpll"     -> 3
  (* | "restarts" -> let module MyBasePlugin = (val bank.(2)) in *)
  (*                 (module RestartsFunctor.MyPlugin.GenPluginWRestart(MyBasePlugin)) *)
  | "hint"     -> 4
  | s -> raise (NotFound ("Generic plugin "^s^" does not exist; see -help"))

let get s = bank.(parse s)
