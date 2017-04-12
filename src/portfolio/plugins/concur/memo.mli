open Async

open Kernel

open Combo
open Top
open HCons
open Messages
open Specs

open General
open SetConstructions

open Tools.PluginsTh

open LoadPluginsTh

module Make(WB : WhiteBoardExt.Type) : sig
  
  open WB
  open DS

  val make :
    msg2th Pipe.Reader.t
    -> msg2pl Pipe.Writer.t
    -> unit Deferred.t

end
