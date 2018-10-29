open Async

module Make(WB : WhiteBoardExt.S) : sig
  open WhiteBoardExt
  open WB
  val make  : regular ports -> unit Deferred.t
  val clear : unit -> unit
end
