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
    msg2th Pipe.Reader.t
    -> msg2pl Pipe.Writer.t
    -> TSet.t
    -> unit Deferred.t

end
