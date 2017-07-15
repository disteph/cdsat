open Async

open Kernel.Top
open Interfaces

module Make(WB: WhiteBoardExt) : sig
  val make :
    WB.isslot_machine ->
    WB.msg2th Pipe.Reader.t ->
    WB.msg2pl Pipe.Writer.t ->
    unit Deferred.t
end

