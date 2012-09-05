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
  val toString: t -> string
  val hash: t -> int
  val equal: t->t->bool
end

open Formulae

module type ACollectImplem = sig
  include CollectImplem with type e = Atom.t
  val filter : bool -> Atom.Predicates.t -> t -> t
end
