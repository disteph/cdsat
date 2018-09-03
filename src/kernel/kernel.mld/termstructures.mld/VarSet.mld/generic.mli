(* module SortSet : Set.S with type elt = Top.Sorts.t *)
(* module SymbSet : Set.S with type elt = Top.Symbols.t *)
open Top.Terms

module Make(S: sig
    (* val ksorts : Sorts.t -> bool *)
    val known : Top.Symbols.t -> bool
    val name  : string
  end) : Termstructure.Type with type t = TSet.t
