(*****************************)
(* The functor X => CC(X)    *)
(* producing a ground theory *)
(*****************************)

open Top
open Interfaces_basic
open Messages
open Specs

open Termstructures.Literals

open Interfaces

type sign

module Make
  (DS: sig 
    include GTheoryDSType
    val proj: Term.datatype -> LitF.t
  end)
  (X : SolvableTheory with type VtoTSet.v = DS.TSet.t
                      and  type t = DS.Term.t
                      and  type v = DS.Term.t)
  (U : PersistentUnionFind with type e = X.v) :
sig

  module type SlotMachineCC =
    sig
      type t
      val treated : DS.TSet.t
      val add : DS.TSet.t -> t
      val normalise :
        DS.Term.t ->
        (sign, DS.TSet.t, Top.Messages.straight) message
    end

  type outputCC =
    | UNSAT of (sign, DS.TSet.t, unsat) message
    | SAT of
        (sign, DS.TSet.t, sat) message *
        (module SlotMachineCC with type t = outputCC)

  val init : (module SlotMachineCC with type t = outputCC)

end
