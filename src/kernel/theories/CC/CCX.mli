(*****************************)
(* The functor X => CC(X)    *)
(* producing a ground theory *)
(*****************************)

open Top
open Interfaces_basic
open Messages
open Specs

open Prop
open Literals

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
        (sign, DS.TSet.t, Top.Messages.thStraight) Top.Messages.thsays
    end

  type outputCC =
    | UNSAT of (sign, DS.TSet.t, Top.Messages.thProvable) Top.Messages.thsays
    | SAT of
        (sign, DS.TSet.t, Top.Messages.thNotProvable) Top.Messages.thsays *
        (module SlotMachineCC with type t = outputCC)

  val init : (module SlotMachineCC with type t = outputCC)

end
