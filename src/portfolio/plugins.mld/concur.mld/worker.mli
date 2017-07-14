open Async

open Kernel.Top
open Interfaces

module Make(WB: WhiteBoardExt) : sig
  open WB.DS

  val add : 'a WB.islot_machine -> (Term.t* Value.t Values.t) option
            -> 'a WB.ioutput * 'a WB.islot_machine
  (* val clone : 'a WB.islot_machine -> 'a WB.islot_machine *)

  val make :
    WB.isslot_machine ->
    WB.msg2th Pipe.Reader.t ->
    WB.msg2pl Pipe.Writer.t ->
    unit Deferred.t
end

