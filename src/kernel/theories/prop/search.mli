open Top
open Interfaces_basic
open Interfaces_theory
open Formulae
open Interfaces_plugin

module ProofSearch(PlDS: PlugDSType) : sig

  module Semantic : Specs.DataType with type t = PlDS.UF.t FormulaF.generic

  module Make(MyTheory: DecProc with type DS.formulae = PlDS.UF.t FormulaF.generic)
    : sig
      module FE : (FrontEndType  with type IForm.datatype = PlDS.UF.t
			         and  type FSet.ps     = PlDS.UFSet.t
			         and  type ASet.ps     = PlDS.UASet.t)
      val machine : FormulaB.t -> (FE.Seq.t -> 'a FE.address) -> 'a FE.output
    end                                                                                    
end
