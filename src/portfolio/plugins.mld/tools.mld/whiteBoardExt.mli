open Async

open Kernel

include module type of WhiteBoardExt_sig
       
module Make(WB: Combo.WhiteBoard) : (Extra with type 'a t := 'a WB.t)
