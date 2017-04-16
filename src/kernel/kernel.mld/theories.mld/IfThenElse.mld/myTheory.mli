open Top
open Messages
open Specs

open Termstructures.Literals

type sign

module Make
  (DS: sig 
    include GTheoryDSType
    val proj: Term.datatype -> LitF.t
  end) : sig

  val init: (sign,DS.TSet.t) slot_machine
  val clear: unit -> unit

end
