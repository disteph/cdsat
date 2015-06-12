open Kernel.Interfaces_basic
open Kernel.Interfaces_theory

module type Semantic = sig
  type t
  type leaf
  val leaf     : leaf -> t
  val semantic : int -> Symbol.t -> t list -> t
end

module TermDef : sig 

  type ('a,'b) xterm = V of 'a | C of Symbol.t * ('b list)
  type ('leaf,'datatype) term

  val reveal : ('leaf,'datatype) term -> ('leaf,('leaf,'datatype) term) xterm
  val data   : ('leaf,'datatype) term -> 'datatype
  val id     : ('leaf,'datatype) term -> int

  module type S = sig
    type leaf
    type datatype
    type t = (leaf,datatype) term
    val bV : leaf -> t
    val bC : Symbol.t -> t list -> t
    val clear : unit -> unit
    val print_in_fmt: Format.formatter -> t -> unit
    module Homo(Mon: MonadType) : sig
      val lift : 
        ('a -> leaf Mon.t) -> ('a,datatype) term -> (leaf,datatype) term Mon.t
      val lifttl : 
        ('a -> leaf Mon.t) -> ('a,datatype) term list -> (leaf,datatype) term list Mon.t
    end
    val subst : ('a -> leaf) -> ('a,datatype) term -> (leaf,datatype) term
  end

  module Make
    (Leaf : PHCons)
    (Data : Semantic with type leaf := Leaf.t)
    : S with type leaf = Leaf.t
        and  type datatype = Data.t

end


module AtomDef : sig

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
    (Leaf : Kernel.Interfaces_basic.PHCons)
    (Data : Semantic with type leaf := Leaf.t)
    : S with type leaf := Leaf.t
        and  type Term.datatype = Data.t

end

module StandardDSData
  (Leaf : PHCons)
  (Data : sig
    type t
    val leaf     : Leaf.t -> t
    val semantic : int -> Symbol.t -> t list -> t
  end) :
sig

  module Atom : AtomDef.S with type leaf := Leaf.t
                          and  type Term.datatype = Data.t

  module ForParsingWOEx(F: Kernel.Formulae.Formula.S with type lit = Atom.t) :
  sig
    include Theory.ForParsingType with type leaf := Leaf.t
    val toForm  : t -> F.t
    val examples: ((unit->F.t)*bool) list
  end
end

module EmptyData(Leaf : PHCons)
  : Semantic with type leaf := Leaf.t

module StandardDS(Leaf : PHCons):
sig

  module Atom : AtomDef.S with type leaf := Leaf.t

  module ForParsingWOEx(F: Kernel.Formulae.Formula.S with type lit = Atom.t) :
  sig
    include Theory.ForParsingType with type leaf := Leaf.t
    val toForm  : t -> F.t
    val examples: ((unit->F.t)*bool) list
  end
end
