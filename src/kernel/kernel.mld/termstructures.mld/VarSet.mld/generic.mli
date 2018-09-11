(* module SortSet : Set.S with type elt = Top.Sorts.t *)
(* module SymbSet : Set.S with type elt = Top.Symbols.t *)
open Top.Terms

module type Arg = sig
  (* val ksorts : Sorts.t -> bool *)
  val known : Top.Symbols.t -> bool
  val name  : string
end

val make : (module Arg) -> TSet.t Key.t
