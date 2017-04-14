open Combo
open Top.Messages
open General.Sums
       
module type DataList = sig
  type agglo
  val dataList : agglo dataList
end

module type Type = sig

  include DataList

  module Strategy(WB: sig
    include WhiteBoard
    val projList: (DS.Term.datatype,agglo) projList
  end)
    : sig
      val solve : WB.DS.TSet.t -> (unsat WB.t, sat WB.t) sum 
      val clear : unit -> unit
  end
end
