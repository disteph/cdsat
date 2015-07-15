open Kernel
open Top.Messages
open Interfaces
open Hub

module type Type = sig
    
  type agglodata
  val datalist : agglodata dataList

  module Strategy(WB: WhiteBoard)
    : sig
      open WB.DS
      val solve : WB.output -> WB.answer
    end
end
