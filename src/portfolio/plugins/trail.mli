(*********************)
(* Conflict analysis *)
(*********************)

open Kernel
open Combo
open Top.Messages

open General
open Patricia_interfaces
       
(* This module implements conflict analysis *)

module Make(WB : WhiteBoard) : sig

  open WB.DS

  (* type nature indicates the status of each formula accumulated in the trail so far.
The reason it was added to it was either:
- It was propagated from other formulae already present in the trail
- It was a decision
- It was tried (i.e. a decision that is not on a formula of the finite basis, and therefore can't just be switched when realising this decision leads to conflict)

The bool in propagated indicates whether we know the level of this propagation for certain, i.e. we have exhibited a dependency path from the decision of that level.
   *)
                             
  type nature =
    | Propagated of straight WB.t
    | Decided of both WB.t
    | Tried
    | Original

  include PATMapType with type keys = Term.t
                      and type values = int*int*nature
                      (* and type infos  = D.infos *)
                      (* and type common = I.common *)
                      (* and type branching = I.branching *)
                      and type ('v,'i) param = (Term.t,'v,int,int,'i) Patricia.poly
  (* and type t = (D.keys,int*int*nature,int,int,D.infos) poly *)


  (* First int is level, second int is timestamp*)

  val analyse : t
                -> unsat WB.t
                -> unsat WB.t * int * Term.t
                           
end
