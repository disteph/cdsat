(* This is the register of all generic plugins in Psyche *)
open Kernel.Theories_register

exception NotFound of string

val get : string -> (unit HandlersMap.t) -> (module Plugin.Type)
