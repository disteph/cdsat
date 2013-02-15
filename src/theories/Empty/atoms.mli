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
