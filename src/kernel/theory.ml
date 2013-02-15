open Formulae

module type Type = sig

  module Atom: AtomType

  module DecProc(ASet: Collection.CollectImplem with type e = Atom.t)
    :sig
      val consistency: ASet.t -> ASet.t option
      val goal_consistency: ASet.t -> Atom.t -> ASet.t option
    end

  module Parser(F:FormulaImplem with type lit = Atom.t)
    :sig
      val parse: string -> F.t
    end

end
