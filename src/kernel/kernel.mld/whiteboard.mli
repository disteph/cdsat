(*********************)
(* Theory Combinator *)
(*********************)

open Top
open Basic
open Interfaces_basic
open Variables
open Messages
open Theories.Register
open Specs
       
(* This is the module type that we are going to produce at the end of this file *)

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
  module WB : WhiteBoard
  open WB.DS
  val mdles : (Term.datatype*Value.t*Assign.t) Theories.Register.Modules.t list
end

                                     
module Make(Ths: sig val theories: unit HandlersMap.t end)
         (InitState : Combo.State)
       : WhiteBoard_ThModules
