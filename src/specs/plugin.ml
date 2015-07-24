open Kernel
open Top.Messages
open Interfaces
open Hub

module type Type = sig
    
  type agglodata
  val datalist : agglodata dataList

  module Strategy(WB: WhiteBoard)
    : sig
      val solve : WB.DS.TSet.t -> WB.answer
    end
end
