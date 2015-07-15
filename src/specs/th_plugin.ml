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
      val consistency     :           TSet.t -> WB.answer
      val goal_consistency: Term.t -> TSet.t -> WB.answer
    end
end
