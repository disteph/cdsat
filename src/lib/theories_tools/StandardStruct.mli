open Kernel.Interfaces_basic
open Kernel.Interfaces_theory

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
    (Data : Theory.ForParsingType with type leaf := Leaf.t) 
    : S with type leaf := Leaf.t
        and  type datatype := Data.t

end


module AtomDef : sig

  type ('leaf,'datatype) atom

  module type S = sig
    type leaf
    type datatype
    type t = (leaf,datatype) atom

    module Term : TermDef.S with type leaf := leaf
                            and  type datatype = datatype

    val reveal  : t -> bool * Symbol.t * (Term.t list)
    val id      : t -> int 
    val compare : t -> t -> int
    val build : bool * Symbol.t * Term.t list -> t
    val clear   : unit -> unit
    val print_in_fmt: Format.formatter -> t -> unit
    val negation : t -> t
    module Homo(Mon: MonadType) : sig
      val lift : 
        ('a -> leaf Mon.t) -> ('a,datatype) atom -> t Mon.t
    end
  end

  module Make
    (Leaf : Kernel.Interfaces_basic.PHCons)
    (Data : Theory.ForParsingType with type leaf := Leaf.t)
    : S with type leaf := Leaf.t
        and  type datatype := Data.t

end

module StandardDSData
  (Leaf : PHCons)
  (Data : Theory.ForParsingType with type leaf := Leaf.t) :
sig

  module Atom : AtomDef.S with type leaf := Leaf.t
                          and  type datatype := Data.t

  module ForParsingWOEx(F: Kernel.Formulae.Formula.S with type lit = Atom.t) :
  sig
    include Theory.ForParsingType with type leaf := Leaf.t
    val toForm  : t -> F.t
    val examples: ((unit->F.t)*bool) list
  end
end

module EmptyData(Leaf : PHCons)
  : Theory.ForParsingType with type leaf := Leaf.t
                          and  type t = unit option

module StandardDS(Leaf : PHCons):
sig

  module Atom : AtomDef.S with type leaf := Leaf.t
                          and  type datatype = unit option

  module ForParsingWOEx(F: Kernel.Formulae.Formula.S with type lit = Atom.t) :
  sig
    include Theory.ForParsingType with type leaf := Leaf.t
    val toForm  : t -> F.t
    val examples: ((unit->F.t)*bool) list
  end
end
