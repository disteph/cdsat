open Async

open Kernel.Theories.Register
open Interfaces
  
module Make(WB: WhiteBoardExt) : sig
  open WB
  type t
  (* Cloning a hub into 2 hubs *)
  val clone     : t -> (t*t) Deferred.t
  (* Killing the communication pipes between master and slaves *)
  val kill      : t -> unit
  (* Sending a message to all slaves *)
  val broadcast : t -> sassign -> chrono:int -> unit Deferred.t
  (* Telling all slaves to kill themselves *)
  val suicide   : t -> Kernel.Top.Messages.unsat WB.t -> sassign -> unit Deferred.t
  (* The pipe reader on which the master thread must read *)
  val reader    : t -> msg2pl Pipe.Reader.t
  (* Constructing a hub *)
  val make :
    (egraph ports -> unit Deferred.t)
    -> (regular ports -> unit Deferred.t)
    -> (regular ports -> unit Deferred.t) HandlersMap.t 
    ->  unit Deferred.t * t
end
