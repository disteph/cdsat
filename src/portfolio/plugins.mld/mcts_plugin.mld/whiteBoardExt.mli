open Async

open Kernel
open Top.Specs
open Interfaces
       
module Make(WB: Export.WhiteBoard)
       : (Extra with type 'a t := 'a WB.t
                              and type termdata := WB.DS.Term.datatype
                                               and type value := WB.DS.Value.t
                                                             and type assign := WB.DS.Assign.t
                                                                            and type cval := WB.DS.CValue.t
                                                                                         and type tset := WB.DS.TSet.t)
