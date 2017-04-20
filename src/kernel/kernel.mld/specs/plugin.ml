open General.Sums
open Top
open Basic
open Interfaces_basic
open Variables
open Messages
open Theories
open Register
open Specs
       
module type WhiteBoard = sig

  module DS : sig
    module Term : TermF

    module Value  : sig
      type t [@@deriving eq, ord, hash, show]
    end

    module Assign : sig
      include Collection with type e = Term.t
                          and type t = (Term.t, unit, int, int, unit)
                                         General.Patricia.poly
      val id : t -> int
    end
  end

  module Msg : sig
    type ('sign,'a) t = ('sign,DS.Assign.t,'a) Messages.message
    val pp : Format.formatter -> (_,_)t -> unit
  end

  type 'a t = private WB of unit HandlersMap.t * (unit,'a) Msg.t
  val pp       : Format.formatter -> 'a t -> unit
  val stamp    : 'a Sig.t -> ('a, 'b) Msg.t -> 'b t
  val sat_init : DS.Assign.t -> sat t
  val sat      : sat t -> sat t -> sat t
  val resolve  : straight t -> 'b propa t -> 'b propa t
  val both2straight: ?side:bool -> both t -> unsat t -> straight t
  val curryfy: DS.Assign.t -> unsat t -> straight t
end


module type WhiteBoard_ThModules = sig
  type u
  module WB : WhiteBoard
  open WB.DS
  module PropModule : Prop.APIplugin.API
  val th_modules : (Term.datatype*Value.t*Assign.t) Modules.t list
end





                                     
(* Obsolete ? *)
                                     
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
