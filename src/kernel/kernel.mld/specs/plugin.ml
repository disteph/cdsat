open Whiteboard
open Top.Messages
open General.Sums

type _ dataList =
  | NoData : unit dataList
  | ConsData: (module Top.Specs.DataType with type t = 'a) * 'b dataList -> ('a*'b) dataList

(* Now, we shall be given a list of the above form, which we shall
   aggregate into datatype, but we shall also have to provide a list
   of projections from the aggregated datatype into each plugin's
   datatype. That list of projections (of the same length of the input
   list) is again an indexed list, of the type below: *)

type (_,_) projList =
  | NoProj  : (_,unit) projList
  | ConsProj: ('t -> 'a) * ('t,'b) projList -> ('t,'a*'b) projList

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
      val solve : WB.DS.Assign.t -> (unsat WB.t, sat WB.t) sum 
      val clear : unit -> unit
  end
end
