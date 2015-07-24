(* This is the register of all generic plugins in Psyche *)

open Plugins

let bank:(module Plugin.Type)array= 
  [|
    (module Basic);
  |]

exception NotFound of string

let getbyname = function
  | _ -> bank.(0)
  (* | s -> raise (NotFound ("Plugin "^s^" does not exist; see -help")) *)
