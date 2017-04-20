open Top
open Interfaces_basic
open Formulae
open APIplugin

module ProofSearch(PlDS: PlugDSType) : sig

  module Semantic : Specs.DataType with type t = PlDS.UF.t FormulaF.generic

  module Make(MyTheory: Specs.DSproj with type ts = PlDS.UF.t FormulaF.generic)
         : API with type FE.constraints = FirstOrder.Constraint.t
                and type FE.IForm.datatype = PlDS.UF.t
		and type FE.FSet.ps     = PlDS.UFSet.t
		and type FE.ASet.ps     =  PlDS.UASet.t

end

