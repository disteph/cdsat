(* This is the register of all generic plugins in Psyche *)

open Plugins

let bank theories :(module Plugin.Type)array= 
  [|
    Basic.make theories;
  |]

exception NotFound of string

let parse = function
  | _ -> 0
  (* | s -> raise (NotFound ("Plugin "^s^" does not exist; see -help")) *)

let get s theories = (bank theories).(parse s)
