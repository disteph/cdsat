(* This is the register of all generic plugins in Psyche *)

open Kernel.Theories_register
    
val get_default : 'a Sig.t -> (module PluginTh.Type)
