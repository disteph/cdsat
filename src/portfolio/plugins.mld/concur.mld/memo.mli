open Async

module Make(WB : WhiteBoardExt.Type) : sig
  
  val make :
    WB.msg2th Pipe.Reader.t
    -> WB.msg2pl Pipe.Writer.t
    -> unit Deferred.t

end
