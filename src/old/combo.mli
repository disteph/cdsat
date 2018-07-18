(*********************)
(* Theory Combinator *)
(*********************)

open Theories.Register

val make : unit HandlersMap.t -> (module Export.API)
