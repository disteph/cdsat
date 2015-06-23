open Kernel
open Kernel.Interfaces_basic
open Kernel.Interfaces_theory

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
