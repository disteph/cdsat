open General
open Kernel

open Interfaces_theory
open Formulae
open Sums
open Gplugins_tools.SetInterface

module Generate(DS:TheoryDSType) : sig

  open DS

  module UASet : sig
    include CollectImplemExt with type e=IAtom.t
    val diff     : t -> t -> t
    val cardinal : t->int
    val negations: t->t
    val choose   : t -> e
    val clear    : unit->unit
  end

  module UF : Formula.Extra with type lit = Atom.t

  module UFSet : sig
    include CollectImplemExt with type e = (UF.lit,UF.t) Formula.generic * DSubst.t
    val choose  : t -> e
    val schoose : UASet.t -> t -> (e,e option)sum
    val rchoose : UASet.t -> t -> (e,e option)sum
    val clear   : unit->unit
  end

end
