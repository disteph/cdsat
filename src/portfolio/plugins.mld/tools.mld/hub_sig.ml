open Async

open Kernel
open Top
open Messages
open Terms
open Sassigns
  
module type S = sig
  type 'a wb
  type msg2pl

  type t
  (* Spawning a hub *)
  val spawn     : t -> t Deferred.t
  (* Killing the communication pipes between master and slaves *)
  val kill      : t -> unit
  (* Sending a message to all slaves *)
  val broadcast : t -> SAssign.t -> level:int -> chrono:int -> unit Deferred.t
  (* Sending new shared terms to all slaves *)
  val share     : t -> TSet.t -> chrono:int -> unit Deferred.t
  (* Telling all slaves to kill themselves *)
  val suicide   : t -> unsat wb -> SAssign.t -> SAssign.t option -> unit Deferred.t
  (* Calling for decision proposals *)
  val propose   : ?whatabout:Term.t -> t -> howmany:int -> chrono:int -> unit Deferred.t
  (* The pipe reader on which the master thread must read *)
  val reader    : t -> msg2pl Pipe.Reader.t
end
