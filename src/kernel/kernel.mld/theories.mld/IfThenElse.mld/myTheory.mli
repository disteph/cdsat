open Top
open Messages
open Terms
open Sassigns

open Theory
type sign

module type API = sig
  type state
  type output = 
    | Sat   of (sign, sat) message
    | Propa of (sign, straight) message

  val add: SAssign.t -> state -> state
  val share: TSet.t -> state -> state
  val what_now: state -> output option * state
  val wondering: state -> TSet.t
  val init: state
end

val hdl : (sign*(module API)) Tags.t
