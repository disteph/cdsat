open Kernel
open Top.Messages
open Interfaces
open Combo

module type Type = sig

  type agglodata
  val datalist : agglodata dataList

  module Strategy(WB: WhiteBoard)
    : sig
      val solve : WB.DS.TSet.t -> WB.answer
    end
end
