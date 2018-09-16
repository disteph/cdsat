(************************************)
(* Main entry point for Psyche runs *)
(************************************)

include module type of Top_level_sig
  
val init : ?withtheories:string list option
           -> ?withouttheories:string list option
           -> Top.Terms.dsKey list
           -> (module Combo.Proof)
           -> parser:Parsers.Register.t
           -> string
           -> (module APIext)
