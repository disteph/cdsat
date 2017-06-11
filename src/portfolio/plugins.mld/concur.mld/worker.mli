open Async

open Kernel.Top.Specs

module Make(WB: WhiteBoardExt.Type) : sig
  open WB.DS

  val add :
    ('a, Assign.t) slot_machine ->
    Assign.t option -> ('a, Assign.t) output

  val clone :
    ('a, Assign.t) slot_machine ->
    ('a, Assign.t) output

  val make :
    Assign.t Plugin.sslot_machine ->
    WB.msg2th Pipe.Reader.t ->
    WB.msg2pl Pipe.Writer.t ->
    unit Deferred.t
end

