open Async.Std

open Kernel
open Top.Specs
open Theories_register
open Combo

open LoadPluginsTh

module Make(WB: WhiteBoard) : sig
  open WB
  open DS
  type msg2pl =
    Msg : 'sign Kernel.Theories_register.Sig.t *
            ('sign, 'msg) Msg.t -> msg2pl
  val print_in_fmt : Format.formatter -> msg2pl -> unit
  type msg2th =
    | MsgStraight of TSet.t
    | MsgBranch of TSet.t * TSet.t *
                     msg2th Pipe.Reader.t * msg2pl Pipe.Writer.t
  val print_in_fmt2 : Format.formatter -> msg2th -> unit
  val add :
    ('a, TSet.t) slot_machine ->
    TSet.t option -> ('a, TSet.t) output
  val clone :
    ('a, TSet.t) slot_machine ->
    ('a, TSet.t) output
  val make :
    msg2th Pipe.Reader.t ->
    msg2pl Pipe.Writer.t ->
    TSet.t sslot_machine ->
    TSet.t -> unit Deferred.t
end

