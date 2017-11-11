open Top.Specs
open Interfaces
       
type sign

type ts = Termstructures.VarSet.Generic.IntSortSet.t

module Make(DS: sig include GlobalDS val proj : Term.datatype -> ts end)
       : API with type sign     := sign
              and type termdata := DS.Term.datatype
              and type value    := DS.Value.t
              and type cval     := DS.CValue.t
              and type assign   := DS.Assign.t
              and type tset     := DS.TSet.t
