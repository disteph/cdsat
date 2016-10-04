open Async.Std

open Kernel
open Top.Specs
open Theories_register
open Combo

open LoadPluginsTh

module Make(WB: WhiteBoardExt.Type) : sig
  open WB.DS

  val add :
    ('a, TSet.t) slot_machine ->
    TSet.t option -> ('a, TSet.t) output

  val clone :
    ('a, TSet.t) slot_machine ->
    ('a, TSet.t) output

  val make :
    TSet.t sslot_machine ->
    WB.msg2th Pipe.Reader.t ->
    WB.msg2pl Pipe.Writer.t ->
    unit Deferred.t
end
