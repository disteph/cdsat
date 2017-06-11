open Kernel
open Top.Specs
open Theories.Register

module type Type = sig

  type sign
  val hdl: sign Sig.t

  module ThDS: DataType

  module Make(DS: sig
    include GTheoryDSType
    val proj: Term.datatype -> ThDS.t
  end) : sig
    val init: (sign,DS.TSet.t) slot_machine
    val clear: unit -> unit
  end
    
end

