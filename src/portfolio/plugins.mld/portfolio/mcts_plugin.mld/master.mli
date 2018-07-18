(*********************************************************************)
(* Main plugin, implementing the combination of decision procedures
   with concurrency, as provided by Jane Street's Async library.
   
   This is a master-slaves architecture.

   Each slave thread runs the code written in worker.ml, controlling
   the (purely sequential) execution of a decision procedure, and
   exchanging messages with the master thread, whose code is below.  *)
(*********************************************************************)

open Async

open Kernel
open Top.Messages
open Theories.Register

open Tools
open Interfaces

open General.Sums

module Make(WBEH: WhiteBoard4Master) : sig
  open WBEH
  open WBE
  module T : sig
    type analysis =
      | InputConflict of unsat WBE.t (* Was a conflict of level 0 *)
      | Backjump of { backjump_level : int; (* Which level to backjump to *)
                      (* The propagations to perform in the new branch *)
                      propagations   : straight WBE.t list;
                      (* Possible decision to make in new branch (for rule UndoDecide) *)
                      decision       : sassign option }
  end
  type state
  type answer    = (T.analysis,WBE.sat_ans) sum
  type saturation_info = NeedsMove | Leaf of answer
  val saturate : state -> (state*saturation_info) Deferred.t
  exception Trail_fail
  val apply_move : sassign -> state -> (state*saturation_info) Deferred.t
  val moves : state -> (sassign * float) list
  val init_state : H.t -> DS.Assign.t -> state Deferred.t
  val master : H.t -> DS.Assign.t -> (unsat t, sat_ans) sum Deferred.t
end
