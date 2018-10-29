open Async

open Kernel.Top
open PluginsTh.PluginTh
       
module Make(WB: WhiteBoardExt.S) : sig
  open WhiteBoardExt
  open WB
  val make : sslot_machine -> regular ports -> unit Deferred.t
end

