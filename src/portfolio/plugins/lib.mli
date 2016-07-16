open Async.Std

val stdin : Reader.t
val pause : unit -> char Reader.Read_result.t Deferred.t

val stdout : Writer.t
val print  : string -> unit Deferred.t

val write : 'a Pipe.Writer.t -> 'a -> unit Deferred.t
