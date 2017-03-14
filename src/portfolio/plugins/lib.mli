open Async

val stdin : Reader.t
val pause : unit -> char Reader.Read_result.t Deferred.t

val stdout : Writer.t
val print  : string -> unit Deferred.t

val write : 'a Pipe.Writer.t -> 'a -> unit Deferred.t

val read : 'a Pipe.Reader.t
           -> ?onkill:(unit -> unit Deferred.t)
           -> ('a -> unit Deferred.t)
           -> unit Deferred.t

val dispatch : 'a Pipe.Reader.t
               -> 'a Pipe.Writer.t list
               -> unit Deferred.t
                    
