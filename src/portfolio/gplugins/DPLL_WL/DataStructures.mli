open General
open Kernel

open Interfaces_theory
open Formulae
open Sums
open Patricia_interfaces
open Gplugins_tools.SetInterface

module Generate(DS:TheoryDSType) : sig

  open DS

  module UASet : sig
    include CollectImplemExt with type e=IAtom.t
    val diff     : t -> t -> t
    val cardinal : t->int
    val negations: t->t
    val latest   : t -> e option
    val choose   : t -> e
    val clear    : unit->unit
  end

  module UF : sig
    include FormExtra with type lit = Atom.t
    val fset : (t,lit) GForm.t * DSubst.t -> bool
  end

  module UFSet : sig
    include CollectImplemExt with type e = (UF.t,UF.lit) GForm.t * DSubst.t
    module UT: sig
      include Intern
      val compare : keys -> keys -> int
    end
    type mykeys = UT.keys
    val aset    : mykeys -> UASet.t
    val form    : mykeys -> e
    val iter    : (mykeys -> unit) -> t -> unit
    val choose  : t -> e
    val rchoose : UASet.t -> t -> (e,e option)sum
    val clear   : unit->unit
  end

end
