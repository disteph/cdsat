open Kernel

open Formulae
open Interfaces

module type Type = sig

  (* A plugin should provide an implementation of formulae, an
     implementation of sets of formulae, and an implementation of
     sets of atoms *)

  type literals
  module UF: FormulaImplem with type lit = literals
  module UFSet: CollectImplem with type e = UF.t
  module UASet: CollectImplem with type e = literals

  (* A plugin should provide a strategy: given the datastructures of a
     FrontEnd (implementation of sequents, answers, outputs, etc), the
     strategy provides a function solve that should convert a
     temporary answer (output) into a final answer (t).  See the
     default implementation in module MyNaive *)

  module Strategy(FE:FrontEndType with type litType     = literals
				  and  type formulaType = UF.t
				  and  type fsetType    = UFSet.t
				  and  type asetType    = UASet.t)
    : sig
      (* A user can embark his own data on the proof-search.
	 Set it to unit if not used *)
      type data
      val initial_data : data
      val solve        : data FE.output -> FE.t
    end
end


module type GenType =
  functor(Atom: AtomType) 
    -> sig
      include Type with type literals = Atom.t
    end
