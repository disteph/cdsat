(* This is the register of all generic plugins in Psyche *)

let bank:(module Plugins.GenType)array= 
  [|
    (module Naive.MyPlugin.GenPlugin);
    (module DPLL_Pat.MyPlugin.GenPlugin);
    (module DPLL_WL.MyPlugin.GenPlugin);
  |]

module StringMaps = Map.Make(String)
open StringMaps

let getbyname = 
  let aux = ref empty in
    aux:= add "naive"    bank.(0) !aux;
    aux:= add "dpll_pat" bank.(1) !aux;
    aux:= add "dpll_wl"  bank.(2) !aux;
    !aux
