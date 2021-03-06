open Async

open Interfaces
       
module Make(WB : WhiteBoardExt) : sig
  open WB
  val make  : regular ports -> unit Deferred.t
  val clear : unit -> unit
end
