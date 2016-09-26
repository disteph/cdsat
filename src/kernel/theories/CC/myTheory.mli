open Top
open Specs
open Messages

open Prop
open Literals

type sign

module Make(DS: sig 
  include GTheoryDSType
  val proj: Term.datatype -> LitF.t
end) :  sig

  module type SlotMachineCC = sig
    type t
    val treated : DS.TSet.t
    val add : DS.TSet.t -> t
    val normalise : DS.Term.t -> (sign, DS.TSet.t, straight) message
  end

  type outputCC =
    | UNSAT of (sign, DS.TSet.t, unsat) message
    | SAT of
        (sign, DS.TSet.t, sat) message *
        (module SlotMachineCC with type t = outputCC)

  val init : (module SlotMachineCC with type t = outputCC)

end
