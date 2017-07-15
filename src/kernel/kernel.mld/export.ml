open General.Sums

open Top
open Interfaces_basic
open Messages
open Theories
open Register
open Specs
       
module type WhiteBoard = sig

  module DS : sig
    type sassign_hashconsed
    include GlobalDS with type Assign.t = (sassign_hashconsed, unit, int, int, unit)
                                            General.Patricia.poly
    module SAssign : sig
      include PHCons with type t = sassign_hashconsed
      val reveal : t -> Term.t*(Value.t Values.t)
      val build  : Term.t*(Value.t Values.t) -> t
    end
  end
  open DS

  type 'a t = private WB of unit HandlersMap.t * (unit,'a) Msg.t
  val pp       : Format.formatter -> 'a t -> unit
  val stamp    : (_*('a*_*_*_)) Tags.t -> ('a, 'b) Msg.t -> 'b t
  val stamp_Eq : (Eq.MyTheory.sign, 'b) Msg.t -> 'b t
  val sat_init : Assign.t -> sat t
  val sat      : sat t -> sat t -> sat t
  val resolve  : straight t -> 'b propa t -> 'b propa t
  val curryfy  : ?assign:Assign.t -> ?flip:Term.t*bool -> unsat t -> straight t
end

type (_,_) proj =
  | Proj : ('cv -> 'v Values.t option) -> ('cv,'v has_values) proj
  | NoProj : ('cv,has_no_values) proj

module type API = sig
  module WB : WhiteBoard
  open WB.DS

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
