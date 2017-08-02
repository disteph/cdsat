open Top.Specs
open Interfaces
       
type sign

module Make(DS: GlobalDS) : API with type sign     = sign
                                 and type termdata = DS.Term.datatype
                                 and type value    = DS.Value.t
                                 and type cval     = DS.CValue.t
                                 and type assign   = DS.Assign.t
                                 and type tset     = DS.TSet.t
