open Kernel
open Interfaces
open Combo
open Types

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
      val solve : WB.DS.TSet.t -> WB.answer
    end
end
