(* This is the register of all generic plugins in Psyche *)
open Kernel.Theories.Register

exception NotFound of string

let get : string -> (module Plugin.API) = function
  | _ -> (module Concur.Main)
  (* | s -> raise (NotFound ("Plugin "^s^" does not exist; see -help")) *)
