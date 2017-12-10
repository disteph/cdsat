open Async

open Kernel.Top
open Interfaces

module Make(WB: WhiteBoardExt) : sig
  open WB
  val make : isslot_machine -> regular ports -> unit Deferred.t
end

