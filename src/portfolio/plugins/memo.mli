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

module Make(WB : WhiteBoard) : sig
  
  open WB
  open DS

  val make :
    unsat WB.t Pipe.Reader.t
    -> (unsat WB.t * Term.t option) Pipe.Writer.t
    -> TSet.t option -> unit Deferred.t                  

end
