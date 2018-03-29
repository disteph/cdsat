open Async

open Kernel.Theories.Register
open Interfaces
  
module Make(WB: WhiteBoardExt) : sig
  open WB
  type t
  (* Spawning a hub *)
  val spawn     : t -> t Deferred.t
  (* Killing the communication pipes between master and slaves *)
  val kill      : t -> unit
  (* Sending a message to all slaves *)
  val broadcast : t -> sassign -> chrono:int -> unit Deferred.t
  (* Sending new shared terms to all slaves *)
  val share     : t -> DS.TSet.t -> chrono:int -> unit Deferred.t
  (* Telling all slaves to kill themselves *)
  val suicide   : t -> Kernel.Top.Messages.unsat WB.t -> sassign -> sassign option
                  -> unit Deferred.t
  (* Calling for decision proposals *)
  val propose   : t -> ?term:term -> int -> chrono:int -> unit Deferred.t
  (* The pipe reader on which the master thread must read *)
  val reader    : t -> msg2pl Pipe.Reader.t
  (* Constructing a hub *)
  val make :
    egraph_init:(egraph ports -> unit Deferred.t)
    -> memo_init:(regular ports -> unit Deferred.t)
    -> other_init:(regular ports -> unit Deferred.t) HandlersMap.t 
    ->  unit Deferred.t * t
end
