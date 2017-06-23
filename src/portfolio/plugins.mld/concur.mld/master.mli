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

open Interfaces

open General.Sums

module Make(WB: sig
                include WhiteBoardExt
                val theories_fold : (Handlers.t -> 'a -> 'a) -> 'a -> 'a
              end) : sig

  open WB

  module WM : sig

    type t

    val make :
      ('c option -> msg2th Pipe.Reader.t -> msg2pl Pipe.Writer.t -> 'd)
      -> 'c HandlersMap.t
      -> msg2pl Pipe.Reader.t * msg2pl Pipe.Writer.t * 'd list * t

  end

  type state

  val main_worker :
    msg2pl Pipe.Reader.t
    -> msg2pl Pipe.Writer.t
    -> WM.t
    -> DS.Assign.t
    -> (unsat WB.t, sat WB.t) sum Deferred.t
end
