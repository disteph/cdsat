(**************)
(* Misc tools *)
(**************)

module FVSubst : sig
  open Variables
  type t = (FreeVar.t*World.t) list [@@deriving eq, hash, show]
  val get_arity : t -> World.t
  val get: int -> t -> FreeVar.t*World.t
end
