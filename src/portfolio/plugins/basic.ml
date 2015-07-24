open Kernel
open Top
open Hub
open PluginTh
open PluginsTh_register

type agglodata = unit
let datalist = NoData

module Strategy(WB: Interfaces.WhiteBoard)
  = struct
    let solve tset = WB.check(WB.PlNotProvable(tset,[]))
  end

