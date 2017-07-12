(************************************)
(* Main entry point for Psyche runs *)
(************************************)

val init : ?withtheories:string list option
           -> ?withouttheories:string list option
           -> parser:Parsers.Register.t
           -> string
           -> (module Export.APIext)
