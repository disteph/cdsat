open Async

open Kernel
open Interfaces
       
module Make(WB: Combo.WhiteBoard) : (Extra with type 'a t := 'a WB.t)
