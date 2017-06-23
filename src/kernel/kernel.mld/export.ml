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

    module CValue : CValue with type value := Value.t

    type bassign = Term.t * bool [@@deriving eq, ord, hash, show]

    module SAssign : sig
      include PHCons
      val reveal : t -> Term.t*(Value.t Values.t)
      val build  : Term.t*(Value.t Values.t) -> t
    end
                       
    module Assign : sig
      include Collection with type e = Term.t*(Value.t Values.t)
                          and type t = (SAssign.t, unit, int, int, unit)
                                     General.Patricia.poly
      val id : t -> int
    end
    val makes_sense : Term.t -> Variables.World.t -> bool
  end

  open DS

  module Msg : sig
    type ('sign,'a) t = ('sign,Assign.t*bassign,'a) Messages.message
    val pp : Format.formatter -> (_,_)t -> unit
  end

  type 'a t = private WB of unit HandlersMap.t * (unit,'a) Msg.t
  val pp       : Format.formatter -> 'a t -> unit
  val stamp    : (_*('a*_*_*_)) Tags.t -> ('a, 'b) Msg.t -> 'b t
  val sat_init : Assign.t -> sat t
  val sat      : sat t -> sat t -> sat t
  val resolve  : straight t -> 'b propa t -> 'b propa t
  val both2straight: ?side:bool -> both t -> unsat t -> straight t
  val curryfy  : Assign.t -> unsat t -> straight t
end

type (_,_) proj =
  | Proj : ('cv -> 'v Values.t option) -> ('cv,'v has_values) proj
  | NoProj : ('cv,has_no_values) proj

module type API = sig
  type uaset
  type uf
  type ufset
  module WB : WhiteBoard
  open WB.DS

  module PropModule : Prop.APIplugin.API with type FE.IForm.datatype = uf
			                  and type FE.FSet.ps = ufset
			                  and type FE.ASet.ps = uaset

  module EGraph : Eq.Interfaces.API with type sign = Eq.MyTheory.sign
                                     and type termdata = Term.datatype
                                     and type value  = Value.t
                                     and type cval   = CValue.t
                                     and type assign = Assign.t

  val th_modules : (Term.datatype*Value.t*Assign.t) Modules.t list
  val vproj      : (_ * (_ * _ * 'd * _)) Theories.Register.Tags.t
                   -> (CValue.t, 'd) proj
  val parse : Parsers.Register.t -> string -> Term.t list
end

module type APIext = sig
  include API
  val problem    : WB.DS.Assign.t
  val expected   : bool option
  type answer = private
              | UNSAT of unsat WB.t
              | SAT of sat WB.t
              | NotAnsweringProblem
  val answer : (unsat WB.t, sat WB.t) sum -> answer
end
