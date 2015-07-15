open Kernel
open Top
open Hub

type agglodata = unit
let datalist = NoData

module Strategy(WB: Interfaces.WhiteBoard)
  = struct
    open WB.DS
    let solve = function
      | WB.Jackpot ans -> ans
      | WB.InsertCoin _ -> failwith "NotImplemented"
  end

