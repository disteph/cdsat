(* This is the register of all generic plugins in Psyche *)
open Kernel.Theories_register

let bank: ((unit HandlersMap.t)->(module Kernel.Plugin.Type)) array= 
  [|
    Concur.Main.make;
  |]

exception NotFound of string

let parse = function
  | _ -> 0
  (* | s -> raise (NotFound ("Plugin "^s^" does not exist; see -help")) *)

let get s = bank.(parse s)
