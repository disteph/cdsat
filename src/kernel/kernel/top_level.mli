(************************************)
(* Main entry point for Psyche runs *)
(************************************)

open General.Sums
  
(* guessThPlug guesses the pair Theory+DecProc + Plugin, taking into
account user input, and argument s, which is a theory name (string),
as possibly indicated by the parser when glancing at the input *)

val parseNrun : (module PluginG.Type) ->
                (unit Theories_register.HandlersMap.t -> (module Plugin.Type)) ->
                string -> bool -> (string, unit) General.Sums.sum option

