(* This is the register of all generic plugins in Psyche *)

open Kernel.Top.Specs
open PluginsTh

let bank (type a) (ds:(module GTheoryDSType with type TSet.t = a))
    :(module PluginTh.Type with type tset = a) array= 
  let module DS = (val ds) in
  [|
    (module Empty_pl1.Make(DS));
  |]

exception NotFound of string

let getbyname = function
  | _ -> 0
  (* | s -> raise (NotFound ("Generic plugin "^s^" does not exist; see -help")) *)
