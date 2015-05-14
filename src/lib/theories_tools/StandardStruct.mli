open Kernel.Interfaces_basic
open Kernel.Interfaces_theory

module TermDef : sig 

  type ('a,'b) xterm = V of 'a | C of Symbol.t * ('b list)
  type ('leaf,'datatype) term

  val reveal : ('leaf,'datatype) term -> ('leaf,('leaf,'datatype) term) xterm
  val data   : ('leaf,'datatype) term -> 'datatype

  module type Type = sig
    type leaf
    type datatype
    type t = (leaf,datatype) term
    val bV : leaf -> t
    val bC : Symbol.t -> t list -> t
    val id : t -> int
    val print_in_fmt: Format.formatter -> t -> unit
    val clear : unit -> unit
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
    (Data : Theory.ForParsingType with type leaf := Leaf.t) 
    : Type with type leaf = Leaf.t
           and  type datatype = Data.t

end

module Predicates : sig
  type t
  val reveal  : t -> Symbol.t
  val compare : t -> t -> int
end


module AtomDef : sig

  type ('leaf,'datatype) atom
  val reveal : ('leaf,'datatype) atom -> bool * Predicates.t * (('leaf,'datatype) TermDef.term list)
  val data   : ('leaf,'datatype) atom -> 'datatype

  module type Type = sig
    type leaf
    type datatype
    type t = (leaf,datatype) atom
    module Term : TermDef.Type with type leaf = leaf
                               and  type datatype = datatype
    val build  : bool * Predicates.t * Term.t list -> t
    val bbuild : bool * Symbol.t * Term.t list -> t
    val id : t -> int 
    val negation : t -> t
    val print_in_fmt: Format.formatter -> t -> unit
    val compare : t -> t -> int
    val clear   : unit -> unit
    module Homo(Mon: MonadType) : sig
      val lift : 
        ('a -> leaf Mon.t) -> ('a,datatype) atom -> t Mon.t
    end
  end

  module Make
    (Leaf : Kernel.Interfaces_basic.PHCons)
    (Data : Theory.ForParsingType with type leaf := Leaf.t)
    : Type with type leaf = Leaf.t
           and  type datatype = Data.t

end

module StandardDSData
  (Leaf : PHCons)
  (Data : Theory.ForParsingType with type leaf := Leaf.t) :
sig

  module Atom : AtomDef.Type with type leaf = Leaf.t
                             and  type datatype = Data.t

  module ForParsingWOEx(F: Kernel.Formulae.FormulaType with type lit = Atom.t) :
  sig
    include Theory.ForParsingType with type leaf = Leaf.t
    val toForm  : t -> F.t
    val examples: ((unit->F.t)*bool) list
  end
end

module EmptyData(Leaf : PHCons)
  : Theory.ForParsingType with type leaf = Leaf.t
                          and  type t = unit option

module StandardDS(Leaf : PHCons):
sig

  module Atom : AtomDef.Type with type leaf = Leaf.t
                             and  type datatype = unit option

  module ForParsingWOEx(F: Kernel.Formulae.FormulaType with type lit = Atom.t) :
  sig
    include Theory.ForParsingType with type leaf = Leaf.t
    val toForm  : t -> F.t
    val examples: ((unit->F.t)*bool) list
  end
end
