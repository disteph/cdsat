(**************)
(* Misc tools *)
(**************)

open Specs

module FVSubst : sig
  open Variables
  type t = (FreeVar.t*World.t) list [@@deriving eq, hash, show]
  val get_arity : t -> World.t
  val get: int -> t -> FreeVar.t*World.t
end

module SAssign(DS:GlobalDS) : sig
  open DS
  type t = Term.t * Value.t Values.t [@@deriving eq, show]
end
       
val fail_state : (_,_,_) Specs.slot_machine

module Pairing(B1: DataType)(B2: DataType)
       : (DataType with type t = B1.t*B2.t)
