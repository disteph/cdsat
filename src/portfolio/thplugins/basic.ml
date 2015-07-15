open Kernel
open Top
open Hub

type agglodata = unit
let datalist = NoData

module Strategy(WB: Interfaces.WhiteBoard)
  = struct
    open WB.DS
    let consistency = WB.consistency
    let goal_consistency = WB.goal_consistency
  end

