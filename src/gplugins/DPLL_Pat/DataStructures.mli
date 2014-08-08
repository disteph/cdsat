open Lib
open Kernel

open Interfaces_I
open Formulae
open Sums
open Patricia
open Common.SetInterface

module Generate(IAtom:IAtomType) : sig

  module ASet : sig
    include CollectImplemExt with type e=IAtom.t
    val diff     : t -> t -> t
    val cardinal : t->int
    val negations: t->t
    val choose   : t -> e
    val clear    : unit->unit
  end

  module F : FormExtraInfo with type lit = IAtom.Atom.t

  module FSet : sig
    include CollectImplemExt with type e = (F.t,F.lit) GForm.t * IAtom.DSubst.t
    val choose  : t -> e
    val schoose : ASet.t -> t -> (e,e option)sum
    val rchoose : ASet.t -> t -> (e,e option)sum
    val clear   : unit->unit
  end

end
