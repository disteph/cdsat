(*****************)
(* Basic modules *)
(*****************)

module IntSort : sig
  include Interfaces_basic.PHCons
  val reveal : t -> int*Sorts.t
  val build  : int*Sorts.t -> t
  val isNeg  : t -> bool
end

module IntMap : Map.S with type key = int
