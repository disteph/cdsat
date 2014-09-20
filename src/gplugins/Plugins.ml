open Kernel

open Interfaces_I
open Formulae
open Interfaces_II

exception PluginAbort of string

module type Type = sig

  (* A plugin should provide an implementation of formulae, an
     implementation of sets of formulae, and an implementation of
     sets of atoms *)

  type literals
  type iliterals
  type delsubsts

  module UF   : FormExtraInfo with type lit = literals
  module UFSet: CollectImplem with type e = (UF.t,literals) GForm.t * delsubsts
  module UASet: CollectImplem with type e = iliterals

  (* A plugin should provide a strategy: given the datastructures of a
     FrontEnd (implementation of sequents, answers, outputs, etc), the
     strategy provides a function solve that should convert a
     temporary answer (output) into a final answer (t).  See the
     default implementation in module MyNaive *)

  module Strategy(FE:FrontEndType with type Form.lit    = literals
				  and  type Form.datatype = UF.t
				  and  type fsetType    = UFSet.t
				  and  type asetType    = UASet.t
				  and  type ilit        = iliterals
				  and  type dsubsts     = delsubsts)
    : sig
      (* A user can embark his own data on the proof-search.
	 Set it to unit if not used *)
      type data
      val initial_data : FE.Seq.t->data
      val solve        : data FE.output -> FE.answer
    end
end


module type GenType =
  functor(IAtom: IAtomType)
    -> sig
      include Type with type literals  = IAtom.Atom.t
                   and  type iliterals = IAtom.t
                   and  type delsubsts = IAtom.DSubst.t
    end
