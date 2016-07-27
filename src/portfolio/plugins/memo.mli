open Async.Std

open Kernel

open Combo
open Top
open HCons
open Messages
open Specs

open General
open SetConstructions

open PluginsTh_tools

open LoadPluginsTh

module Make(WB : WhiteBoardExt.Type) : sig
  
  open WB
  open DS

  val make :
    TSet.t
    -> msg2th Pipe.Reader.t
    -> msg2pl Pipe.Writer.t
    -> unit Deferred.t

  val make_listener : unsat WB.t Pipe.Reader.t -> unit Deferred.t
end
