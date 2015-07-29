(* This is the register of all generic plugins in Psyche *)
open Kernel.Register
open Plugins

let bank: ((unit HandlersMap.t)->(module Plugin.Type)) array= 
  [|
    Concur.make;
  |]

exception NotFound of string

let parse = function
  | _ -> 0
  (* | s -> raise (NotFound ("Plugin "^s^" does not exist; see -help")) *)

let get s = bank.(parse s)
