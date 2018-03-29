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
  val master : H.t -> DS.Assign.t -> (unsat t, sat_ans) sum Deferred.t
end
