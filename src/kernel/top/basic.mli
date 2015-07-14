(*****************)
(* Basic modules *)
(*****************)

open Interfaces_basic

module IntSort : sig
  include PHCons
  val reveal : t -> int*Sorts.t
  val build  : int*Sorts.t -> t
  val buildH : int*Sorts.t -> t
  val isNeg  : t -> bool
end

module IntMap : Map.S with type key = int

module HashedTypeFromHCons(M: sig
  type t
  val id: t -> int
end): Hashtbl.HashedType with type t = M.t

module IdMon : MonadType with type 'a t = 'a
