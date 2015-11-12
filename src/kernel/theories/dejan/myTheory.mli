open Top
open Messages
open Specs

type sign

module ThDS : DataType

module Make(DS: sig 
  include GTheoryDSType
  val proj: Term.datatype -> ThDS.t
end) : sig

  val init: (sign,DS.TSet.t) slot_machine

end
