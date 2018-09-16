(*********************)
(* Conflict analysis *)
(*********************)

open Async

open Kernel
open Top.Sassigns
open Top.Messages

open General
open Patricia
open Sums

open Interfaces
       
(* This module implements conflict analysis *)

module Make(WB : WhiteBoardExt) : sig

  open WB

  (* type nature indicates the status of each formula accumulated in the trail so far.
The reason it was added to it was either:
- It was in the original problem
- It was propagated from other formulae already present in the trail
- It was a decision *)
                             
  type nature =
    | Input
    | Deduction of straight WB.t
    | Decision
[@@deriving show]

  type t

  val level : t -> int
  val chrono: t -> int
  val chrono_incr: t -> t
  val init  : t
  val add   : nature:nature -> SAssign.t -> t -> t option

  (* Type for the result of the conflict analysis *)
  type analysis =
    | InputConflict of unsat WB.t (* Was a conflict of level 0 *)
    | Backjump of { backjump_level : int; (* Which level to backjump to *)
                    (* The propagations to perform in the new branch *)
                    propagations   : straight WB.t list;
                    (* Possible decision to make in new branch (for rule UndoDecide) *)
                    decision       : SAssign.t option }

                                                 
  val analyse : t             (* the trail *)
                -> unsat WB.t (* the conflict *)
                -> (unsat WB.t -> SAssign.t -> SAssign.t option -> unit Deferred.t)
                (* a function to which we can pass stuff to learn *)
                -> analysis Deferred.t
                           
end
