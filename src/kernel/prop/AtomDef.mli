open Top
open Interfaces_basic
open Basic

type ('leaf,'datatype) atom

module type S = sig
  type leaf
  module Term : TermDef.S with type leaf := leaf
  type t = (leaf,Term.datatype) atom
  val reveal  : t -> bool * Symbol.t * (Term.t list)
  val id      : t -> int
  val compare : t -> t -> int
  val build    : bool * Symbol.t * Term.t list -> t
  val clear    : unit -> unit
  val print_in_fmt: Format.formatter -> t -> unit
  val negation : t -> t
  module Homo(Mon: MonadType) : sig
    val lift :
      ('a -> leaf Mon.t) -> ('a,Term.datatype) atom -> t Mon.t
  end
end

module Make
  (Leaf : PHCons)
  (Data : Semantic with type leaf := Leaf.t)
  : S with type leaf := Leaf.t
      and  type Term.datatype = Data.t



