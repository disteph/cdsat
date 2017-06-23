open Async

open Kernel
open Interfaces
       
module Make(WB: Export.WhiteBoard)
       : (Extra with type 'a t := 'a WB.t
                              and type assign := WB.DS.Assign.t
                                             and type sassign := WB.DS.SAssign.t)
