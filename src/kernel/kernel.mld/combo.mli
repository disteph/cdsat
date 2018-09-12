(*********************)
(* Theory Combinator *)
(*********************)

open Top.Terms
open Theories.Register

include module type of Combo_sig

val make : dsKey list -> unit HandlersMap.t -> (module API)
