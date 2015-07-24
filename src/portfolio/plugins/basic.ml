open Kernel
open Top
open Register
open Combo
open PluginTh

let make (theories: unit HandlersMap.t)
    :(module Plugin.Type) =

  (module struct

    type agglodata = unit
    let datalist = NoData

    module Strategy(WB: Interfaces.WhiteBoard)
      = struct
        let solve tset = WB.check(WB.PlNotProvable(tset,[]))
      end

  end)
