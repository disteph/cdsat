open Top
open Messages
open Specs
open Sassigns
       
type sign

module type API = sig
  type termdata
  type value
  type assign
  module TSet : Set.S with type elt = termdata termF
  type state
  type output = 
    | Sat   of (sign, assign*(termdata termF*bool),sat) message
    | Propa of (sign, assign*(termdata termF*bool),straight) message

  val add: (termdata termF, value) sassign -> state -> state
  val what_now: state -> output option * state
  val wondering: state -> TSet.t
  val init: state
end


include Theory.Type with type ts = unit
                     and type values = has_no_values
                     and type ('t,'v,'a) api = (module API with type termdata = 't
                                                            and type value = 'v
                                                            and type assign = 'a)

