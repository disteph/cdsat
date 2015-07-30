open Kernel
open Top.Specs
open Theories_register
open Types

module type Type = sig

  module ThDS: Semantic

  module Make(DS: sig
    include GTheoryDSType
    val proj: Term.datatype -> ThDS.t
  end) : sig
    val search: DS.TSet.t -> DS.TSet.t slot_machine
  end
    
end

