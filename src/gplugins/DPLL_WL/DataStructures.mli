open Lib
open Kernel

open Interfaces_I
open Formulae
open Sums
open Patricia
open Common.SetInterface

module Generate(IAtom:IAtomType) : sig

  module MyIAtomNeg : sig
    val negation : IAtom.t -> IAtom.t
  end

  module ASet : sig
    include CollectImplemExt with type e=IAtom.t
    val diff     : t -> t -> t
    val cardinal : t->int
    val negations: t->t
    val latest   : t -> e option
    val choose   : t -> e
    val clear    : unit->unit
  end

  module F : sig
    include FormExtraInfo with type lit = IAtom.Atom.t
    val aset   : (t,lit) GForm.t * IAtom.DSubst.t -> ASet.t
    val fset   : (t,lit) GForm.t * IAtom.DSubst.t -> bool
  end

  module FSet : sig
    include CollectImplemExt with type e = (F.t,F.lit) GForm.t * IAtom.DSubst.t
    module UT: sig
      include Intern with type keys = e
      val compare : e -> e -> int
    end
    val iter    : (e -> unit) -> t -> unit
    val choose  : t -> e
    val rchoose : ASet.t -> t -> (e,e option)sum
    val clear   : unit->unit
  end

end
