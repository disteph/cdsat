open Async

open Kernel.Theories_register

module Make(W: sig
                type msg2th
                type msg2pl
              end) : sig

  type t

  val make :
    ('c option -> W.msg2th Pipe.Reader.t -> W.msg2pl Pipe.Writer.t -> 'd)
    -> 'c HandlersMap.t
    -> W.msg2pl Pipe.Reader.t * W.msg2pl Pipe.Writer.t * 'd list * t

  val clone :
    (W.msg2th Pipe.Writer.t
     -> W.msg2th Pipe.Reader.t
     -> W.msg2pl Pipe.Writer.t
     -> W.msg2th Pipe.Reader.t
     -> W.msg2pl Pipe.Writer.t
     -> 'd)
    -> t
    -> W.msg2pl Pipe.Reader.t * W.msg2pl Pipe.Writer.t
       * W.msg2pl Pipe.Reader.t * W.msg2pl Pipe.Writer.t
       * 'd list * t * t

  val broadcast :
    (W.msg2th Pipe.Writer.t -> unit Deferred.t) -> t -> unit Deferred.t

end
