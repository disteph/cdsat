module Term : sig
  type variables (*= string*)
  type fsymb (*= string*)
  type term = V of variables | XV of variables | C of fsymb * t list
  and t
  val reveal : t -> term
  val build : term -> t
  val id : t -> int
  val toString : t -> string
  val printtl : t list -> string
  val clear : unit -> unit
end

module Atom : sig
  module Predicates :
    sig type t val compare : t -> t -> int val id : t -> int end
  type t
  val reveal : t -> bool * Predicates.t * Term.t list
  val build : bool * Predicates.t * Term.t list -> t
  val bbuild : bool * string * Term.t list -> t
  val id : t -> int 
  val negation : t -> t
  val toString : t -> string
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val clear : unit -> unit
end

type 'a form =
    Lit of Atom.t
  | AndP of 'a * 'a
  | OrP of 'a * 'a
  | AndN of 'a * 'a
  | OrN of 'a * 'a

module type FormulaImplem = sig
  type t
  val reveal : t -> t form
  val build : t form -> t
end

module PrintableFormula : functor (F : FormulaImplem) -> sig
  type t = F.t
  val toString : F.t -> string
  val negation : F.t -> F.t
  val lit : bool * string * Term.t list -> F.t
  val andN : F.t * F.t -> F.t
  val andP : F.t * F.t -> F.t
  val orN : F.t * F.t -> F.t
  val orP : F.t * F.t -> F.t
end

