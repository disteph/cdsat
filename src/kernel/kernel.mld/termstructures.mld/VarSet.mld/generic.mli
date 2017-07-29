open General
open Top.Basic

module IntSortSet : Patricia.PatSet.S with type e = IntSort.t
                                       and type ('v,'i) param = (IntSort.t,'v,int,int,'i) Patricia.poly

(* module SortSet : Set.S with type elt = Top.Sorts.t *)
(* module SymbSet : Set.S with type elt = Top.Symbols.t *)

module Make(S: sig
                (* val ksorts : Sorts.t -> bool *)
                val known : Top.Symbols.t -> bool
              end) : Top.Specs.DataType with type t = IntSortSet.t
