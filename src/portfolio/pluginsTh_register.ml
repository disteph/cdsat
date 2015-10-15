(* This is the register of all generic plugins in Psyche *)

open Kernel.Top.Specs
open PluginsTh
open Kernel.Theories_register

let bank :(module PluginTh.Type) array= 
  [|
    (module Empty_pl1);
    (module Cc_pl1);
    (module Arrays_pl1);
    (module Dejan_pl1);
  |]
    
let get_default : type a. a Sig.t -> (module PluginTh.Type)
  = function
  | Sig.Empty -> bank.(0)
  | Sig.CC -> bank.(1)
  | Sig.Arrays -> bank.(2)
  | Sig.Dejan -> bank.(3)
