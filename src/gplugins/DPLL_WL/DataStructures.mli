open Lib
open Kernel

open Interfaces
open Sums
open Patricia
open Common.SetInterface

module Generate(Atom:AtomType) : sig

  module ASet : sig
    include CollectImplemExt with type e=Atom.t
    val diff     : t -> t -> t
    val cardinal : t->int
    val negations: t->t
    val latest   : t -> e option
    val choose   : t -> Atom.t
    val clear    : unit->unit
  end

  module F : sig
    include FormulaImplem with type lit=Atom.t
    val aset   : t-> ASet.t
    val fset   : t-> bool
    val compare: t->t-> int
    val clear  : unit->unit
  end

  module FSet : sig
    include CollectImplemExt with type e=F.t
    module UT:Intern with type keys=F.t
    val iter    : (F.t -> unit) -> t -> unit
    val choose  : t -> F.t
    val rchoose : ASet.t -> t -> (F.t,F.t option)sum
    val clear   : unit->unit
  end

end
