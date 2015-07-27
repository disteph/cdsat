(* This is the register of all generic plugins in Psyche *)

open Kernel.Top.Specs
open PluginsTh
open Kernel.Register

let bank :(module PluginTh.Type) array= 
  [|
    (module Empty_pl1);
  |]
    
exception NotFound of string

let parse = function
  | _ -> 0
  (* | s -> raise (NotFound ("Generic plugin "^s^" does not exist; see -help")) *)

let get s = bank.(parse s)

let get_default : type a. a Sig.t -> (module PluginTh.Type)
  = function
  | Sig.Empty -> bank.(0)
