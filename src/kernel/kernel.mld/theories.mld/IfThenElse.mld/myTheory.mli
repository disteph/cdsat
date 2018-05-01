open Top
open Messages
open Specs
open Sassigns
       
type sign

module type API = sig
  type termdata
  type value
  type assign
  type tset
  type state
  type output = 
    | Sat   of (sign, assign*(termdata termF,value)bassign*tset,sat) message
    | Propa of (sign, assign*(termdata termF,value)bassign*tset,straight) message

  val add: (termdata termF, value) sassign -> state -> state
  val share: tset -> state -> state
  val what_now: state -> output option * state
  val wondering: state -> tset
  val init: state
end


include Theory.Type with module TS = Termstructures.VarSet.ITE
                     and type values = has_no_values
                     and type ('t,'v,'a,'s) api = (module API with type termdata = 't
                                                               and type value = 'v
                                                               and type assign = 'a
                                                               and type tset = 's)
