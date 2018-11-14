open General

open Top
open Terms
open Sassigns
open Messages
    
open Theory

type sign
type state

module Monad : Monads.Monad with type 'a t = state -> 'a * state

module type API = sig
  val add   : SAssign.t option -> (sign,sat) message option Monad.t
  val share : TSet.t -> (sign,sat) message option Monad.t
  val init  : state
end

val hdl : (sign*(module API)) Tags.t
