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
              end)
         (EGraph: Theories.Eq.Interfaces.API
          with type sign = Theories.Eq.MyTheory.sign
           and type termdata = WB.DS.Term.datatype
           and type value  = WB.DS.Value.t
           and type cval   = WB.DS.CValue.t
           and type assign = WB.DS.Assign.t) : sig

  open WB

  module WM : sig

    type t

    val make :
      (msg2th Pipe.Reader.t -> msg2pl Pipe.Writer.t -> 'd)
      -> (msg2th Pipe.Reader.t -> msg2pl Pipe.Writer.t -> 'd) HandlersMap.t
      -> msg2pl Pipe.Reader.t * msg2pl Pipe.Writer.t * 'd list * t

  end

  type state

  val master :
    msg2pl Pipe.Reader.t
    -> msg2pl Pipe.Writer.t
    -> WM.t
    -> DS.Assign.t
    -> (unsat WB.t, sat WB.t) sum Deferred.t
end
