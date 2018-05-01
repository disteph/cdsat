open General
open Top.Basic

(* module SortSet : Set.S with type elt = Top.Sorts.t *)
(* module SymbSet : Set.S with type elt = Top.Symbols.t *)

module Make(S: sig
                (* val ksorts : Sorts.t -> bool *)
                val known : Top.Symbols.t -> bool
              end) : Termstructure.Type with type (_,'tset) t = 'tset
