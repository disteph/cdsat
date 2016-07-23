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
                include WhiteBoard
                val theories: unit HandlersMap.t
              end) : sig

  open WB
  open DS         
  (* We load the code of the slave workers, generated from the
      Whiteboard *)

  module W : sig

    type msg2pl = Msg : 'sign Sig.t * ('sign, 'msg) WB.Msg.t -> msg2pl

    val print_in_fmt : Format.formatter -> msg2pl -> unit

    type msg2th =
      | MsgStraight of TSet.t
      | MsgBranch of TSet.t * TSet.t * msg2th Pipe.Reader.t * msg2pl Pipe.Writer.t

    val print_in_fmt2 : Format.formatter -> msg2th -> unit

    val add : ('a, TSet.t) slot_machine -> TSet.t option -> ('a, TSet.t) output

    val clone : ('a, TSet.t) slot_machine -> ('a, TSet.t) output

    val make :
      msg2th Pipe.Reader.t ->
      msg2pl Pipe.Writer.t ->
      TSet.t LoadPluginsTh.sslot_machine ->
      TSet.t -> unit Deferred.t
  end

  val hdlfold :
    (Handlers.t -> 'a -> 'b -> unit Deferred.t * 'b)
    -> 'a HandlersMap.t -> 'b -> unit Deferred.t list * 'b

  val broadcast : (W.msg2th Pipe.Writer.t -> unit Deferred.t) ->
                  W.msg2th Pipe.Writer.t HandlersMap.t -> unit Deferred.t

  type state = {
      from_workers : W.msg2pl Pipe.Reader.t;
      to_plugin  : W.msg2pl Pipe.Writer.t;
      pipe_map   : W.msg2th Pipe.Writer.t HandlersMap.t;
      thAnd_list : W.msg2pl list;
      thOr_list  : W.msg2pl list;
      waiting4   : unit HandlersMap.t;
    }

  val kill_pipes : state -> unit Deferred.t

  val branch :
    state ->
    (state -> 'a Deferred.t) ->
    TSet.t ->
    TSet.t ->
    ('a * (unit -> 'a Deferred.t) * (unit -> unit Deferred.t)) Deferred.t

  val resolve : straight WB.t -> 'a propa WB.t -> 'a propa WB.t

  val select_msg : state -> (W.msg2pl * state) Deferred.t

  val main_worker : state -> sat WB.t -> (unsat WB.t, sat WB.t) sum Deferred.t
end
