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
         (DS: DSproj with type ts = LitF.t)
         (X : SolvableTheory with type VtoAssign.v = DS.Assign.t
                             and  type t = DS.Term.t
                             and  type v = DS.Term.t)
         (U : PersistentUnionFind with type e = X.v) : sig

  module type SlotMachineCC =
    sig
      type t
      val treated : DS.Assign.t
      val add : DS.Assign.t -> t
      val normalise :
        DS.Term.t ->
        (sign, DS.Assign.t, Top.Messages.straight) message
    end

  type outputCC =
    | UNSAT of (sign, DS.Assign.t, unsat) message
    | SAT of
        (sign, DS.Assign.t, sat) message *
        (module SlotMachineCC with type t = outputCC)

  val init : (module SlotMachineCC with type t = outputCC)

end
