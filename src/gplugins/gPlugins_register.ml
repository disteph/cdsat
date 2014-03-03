(* This is the register of all generic plugins in Psyche *)

let bank:(module Plugins.GenType)array= 
  [|
    (module Naive.MyPlugin.GenPlugin);
    (module DPLL_Pat.MyPlugin.GenPlugin);
    (module DPLL_WL.MyPlugin.GenPlugin);
  |]

exception NotFound of string

let getbyname = function
  | "naive"    -> bank.(0)
  | "dpll_pat" -> bank.(1)
  | "dpll_wl"  -> bank.(2)
  | "restarts" -> let module MyBasePlugin = (val bank.(2)) in
                  (module RestartsFunctor.MyPlugin.GenPluginWRestart(MyBasePlugin))
  | s -> raise (NotFound ("Generic plugin "^s^" does not exist; see -help"))
