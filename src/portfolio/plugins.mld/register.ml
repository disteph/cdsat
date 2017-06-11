(* This is the register of all generic plugins in Psyche *)
open Kernel.Theories.Register

let bank: (module Plugin.API) array= 
  [|
    (module Concur.Main);
  |]

exception NotFound of string

let parse = function
  | _ -> 0
  (* | s -> raise (NotFound ("Plugin "^s^" does not exist; see -help")) *)

let get s = bank.(parse s)
