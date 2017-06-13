open General.Sums

open Top
open Interfaces_basic
open Messages
open Theories
open Register
open Specs
       
module type WhiteBoard = sig

  module DS : sig
    module Term : Term

    module Value  : sig
      type t [@@deriving eq, ord, hash, show]
    end

    module Assign : sig
      include Collection with type e = Term.t
                          and type t = (Term.t, unit, int, int, unit)
                                         General.Patricia.poly
      val id : t -> int
    end
    val makes_sense : Term.t -> Variables.World.t -> bool
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


module type API = sig
  type u
  module WB : WhiteBoard
  open WB.DS
  module PropModule : Prop.APIplugin.API
  val th_modules : (Term.datatype*Value.t*Assign.t) Modules.t list
  val problem    : Assign.t
  val expected   : bool option
  type answer = private
              | UNSAT of unsat WB.t
              | SAT of sat WB.t
              | NotAnsweringProblem
  val answer : (unsat WB.t, sat WB.t) sum -> answer
end
