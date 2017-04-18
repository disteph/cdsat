(**************)
(* Misc tools *)
(**************)

open Specs

val get_sort : 'a Specs.termF -> Sorts.t

val fail_state : ('sign,'ts) Specs.slot_machine

module Pairing(B1: DataType)(B2: DataType)
       : (DataType with type t = B1.t*B2.t)
