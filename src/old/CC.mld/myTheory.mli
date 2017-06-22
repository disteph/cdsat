open Top
open Specs
open Messages

type sign

module type API = sig

  type termdata
  type value
  type assign

  module type SlotMachineCC = sig
    type t
    val treated : assign
    val add : assign -> t
    val normalise : termdata termF -> (sign, assign, straight) message
  end

  type outputCC =
    | UNSAT of (sign, assign, unsat) message
    | SAT of
        (sign, assign, sat) message *
        (module SlotMachineCC with type t = outputCC)

  val init : (module SlotMachineCC with type t = outputCC)

end

include Theory.Type with type ts = Termstructures.Literals.LitF.t
                     and type values = has_no_values
                     and type ('t,'v,'a) api
                              = (module API with type termdata = 't
                                             and type value    = 'v
                                             and type assign   = 'a)
                                  
