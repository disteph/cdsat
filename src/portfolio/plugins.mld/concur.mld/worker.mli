open Async

open Kernel.Top.Specs
open Interfaces

module Make(WB: WhiteBoardExt) : sig
  open WB.DS

  val add : 'a WB.islot_machine -> Assign.t option -> 'a WB.ioutput

  val clone : 'a WB.islot_machine -> 'a WB.islot_machine

  val make :
    WB.isslot_machine ->
    WB.msg2th Pipe.Reader.t ->
    WB.msg2pl Pipe.Writer.t ->
    unit Deferred.t
end

