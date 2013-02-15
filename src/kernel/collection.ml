module type CollectImplem = sig
  type e
  type t
  val is_empty: t -> bool
  val is_in: e -> t -> bool
  val empty: t
  val add: e -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val remove: e -> t -> t
  val next: t -> e*t
  val fold : (e -> 'a -> 'a) -> t -> 'a -> 'a
  val toString: t -> string
  val hash: t -> int
  val equal: t->t->bool
end
