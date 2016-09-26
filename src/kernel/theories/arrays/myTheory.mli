open Top
open Messages
open Specs

type sign

module Make(DS: GTheoryDSType) : sig

  val init: (sign,DS.TSet.t) slot_machine
  val clear: unit -> unit

end
