open Async

open Kernel
open Top.Specs
open Interfaces
       
module Make(WB: Export.WhiteBoard)
       : (Extra with type 'a t := 'a WB.t
                              and type datatypes = WB.DS.Term.datatype*WB.DS.Value.t*WB.DS.Assign.t
                              and type sassign = WB.DS.Term.t * WB.DS.Value.t Top.Values.t)
