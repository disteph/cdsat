(************************************)
(* Main entry point for Psyche runs *)
(************************************)

val init : ?withtheories:string list option
           -> ?withouttheories:string list option
           -> Top.Terms.dsKey list
           -> parser:Parsers.Register.t
           -> string
           -> (module Combo.APIext)
