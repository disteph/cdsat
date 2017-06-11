(*********************)
(* Conflict analysis *)
(*********************)

open Async

open Kernel
open Top.Messages

open General
open Patricia_interfaces
open Sums
       
(* This module implements conflict analysis *)

module Make(WB : Export.WhiteBoard) : sig

  open WB.DS

  (* type nature indicates the status of each formula accumulated in the trail so far.
The reason it was added to it was either:
- It was in the original problem
- It was propagated from other formulae already present in the trail
- It was a decision
- It was tried (i.e. a decision that is not on a formula of the finite basis, and therefore can't just be switched when realising this decision leads to conflict)
   *)
                             
  type nature =
    | Original
    | Propagated of straight WB.t
    | Decided of both WB.t
    | Tried

  include PATMapType with type keys = Term.t
                      and type values = int*int*nature
                      (* First int is level, second int is timestamp*)
                      (* and type infos  = D.infos *)
                      (* and type common = I.common *)
                      (* and type branching = I.branching *)
                      and type ('v,'i) param = (Term.t,'v,int,int,'i) Patricia.poly
  (* and type t = (D.keys,int*int*nature,int,int,D.infos) poly *)

  val analyse : t             (* the trail *)
                -> unsat WB.t (* the conflict *)
                -> (unsat WB.t -> Term.t -> Term.t option -> unit Deferred.t)
                (* a function to which we can pass stuff to learn *)
                -> (unsat WB.t,  (* Either a subset of the original formulae are unsat *)
                    int * straight WB.t) (* Or there is a level to backjump to,
                                            with a propagation message to use for the backjump branch *)
                     sum Deferred.t
                           
end
