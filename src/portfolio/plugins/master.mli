(*********************************************************************)
(* Main plugin, implementing the combination of decision procedures
with concurrency, as provided by Jane Street's Async library.

   This is a master-slaves architecture.

   Each slave thread runs the code written in worker.ml, controlling
   the (purely sequential) execution of a decision procedure, and
   exchanging messages with the master thread, whose code is below.  *)
(*********************************************************************)

open Async.Std

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

  module W : sig
    val make :
      TSet.t LoadPluginsTh.sslot_machine ->
      TSet.t ->
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

  module Mm : sig
    val make :
      TSet.t
      -> msg2th Pipe.Reader.t
      -> msg2pl Pipe.Writer.t
      -> unit Deferred.t
  end

  val clause_listener :  unit Deferred.t
  val clause_listener_kill : unit -> unit Deferred.t

  module AS : sig
    type t
    val all : t
  end

  type state = {
      from_workers : msg2pl Pipe.Reader.t;
      to_plugin  : msg2pl Pipe.Writer.t;
      pipe_map   : WM.t;
      thAnd_list : say answer list;
      thOr_list  : say answer list;
      waiting4   : AS.t;
    }

  val kill_pipes : state -> unit Deferred.t

  val branch :
    state ->
    (state -> 'a Deferred.t) ->
    TSet.t ->
    TSet.t ->
    ('a * (unit -> 'a Deferred.t) * (unit -> unit Deferred.t)) Deferred.t

  val resolve : straight WB.t -> (unsat WB.t, sat WB.t) sum -> (unsat WB.t, sat WB.t) sum Deferred.t

  val select_msg : state -> (say answer * state) Deferred.t

  val main_worker : state -> sat WB.t -> (unsat WB.t, sat WB.t) sum Deferred.t
end
