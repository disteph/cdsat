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
open Top.Specs
open Theories_register
open Combo

open General.Sums

module Make(WB: sig
                include WhiteBoardExt.Type
                val theories: unit HandlersMap.t
              end) : sig

  open WB
  open DS         
  (* We load the code of the slave workers, generated from the
      Whiteboard *)

  module Mm : sig
    val make :
      msg2th Pipe.Reader.t
      -> msg2pl Pipe.Writer.t
      -> unit Deferred.t
  end

  module W : sig
    val make :
      TSet.t LoadPluginsTh.sslot_machine ->
      msg2th Pipe.Reader.t ->
      msg2pl Pipe.Writer.t ->
      unit Deferred.t
  end

  module WM : sig

    type t

    val make :
      ('c option -> msg2th Pipe.Reader.t -> msg2pl Pipe.Writer.t -> 'd)
      -> 'c HandlersMap.t
      -> msg2pl Pipe.Reader.t * msg2pl Pipe.Writer.t * 'd list * t

  end

  module AS : sig
    type t
    val all : t
  end

  type state

  val select_msg : state -> (say answer * state) Deferred.t

  val main_worker :
    msg2pl Pipe.Reader.t
    -> msg2pl Pipe.Writer.t
    -> WM.t
    -> TSet.t
    -> (unsat WB.t, sat WB.t) sum Deferred.t
end
