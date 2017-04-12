(* This is the register of all generic plugins in Psyche *)

exception NotFound of string

val get : string -> (module Kernel.PluginG.Type)
