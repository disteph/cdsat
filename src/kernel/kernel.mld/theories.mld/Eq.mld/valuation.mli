open General
open Patricia_tools

open Top
open Terms
open Sassigns
open Values

(* Valuations are term -> values maps extracted from the Egraph *)
(* We define a notion of valuation,
   as a map from terms to the combined values found in the egraph. *)
include Patricia.Map.S_NH with type keys   = Term.t
                           and type values = CValue.t * (Assign.t*int) Lazy.t
                           and type common = int
                           and type branching = int

val pp : t Format.printer

type 'sign signed

val sempty : _ signed
val sunion : 'a signed -> 'a signed -> 'a signed
val build  : 'a -> t -> 'a signed
val reveal : _ signed -> t
