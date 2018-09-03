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
       
val fail_state : _ Specs.slot_machine
