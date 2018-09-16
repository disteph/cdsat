open Async

open Kernel.Top
open Interfaces
open PluginsTh.PluginTh

module Make(WB: WhiteBoardExt) : sig
  open WB
  val make : sslot_machine -> regular ports -> unit Deferred.t
end

