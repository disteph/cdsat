module type TermType = sig
  type fsymb
  type leaf
  type t
  type ('a,'b) term = V of 'a | C of fsymb * ('b list)
  val reveal : t -> (leaf,t) term
  val build : (leaf,t) term -> t
  val bV : leaf -> t
  val bC : fsymb -> t list -> t
  val subst : (leaf -> leaf) -> t -> t
  val id     : t -> int
  val print_in_fmt: Format.formatter -> t -> unit
  val clear : unit -> unit
end


module TermMake(Leaf: Kernel.Interfaces_I.PHCons) : 
TermType

module Predicates : sig
  type t
  val compare : t -> t -> int
end

module Atom(Leaf: Kernel.Interfaces_I.PHCons) : sig
  type t
  module Term : TermType with type leaf = Leaf.t and type fsymb = string
  val reveal : t -> bool * Predicates.t * Term.t list
  val build  : bool * Predicates.t * Term.t list -> t
  val bbuild : bool * string * Term.t list -> t
  val id : t -> int 
  val negation : t -> t
  val print_in_fmt: Format.formatter -> t -> unit
  val compare : t -> t -> int
  val clear   : unit -> unit
end
