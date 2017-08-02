open General
open Sums
open Patricia
open Patricia_tools
       
open Top
open Interfaces_basic
open Messages
open Theories
open Register
open Specs
open Sassigns
       
module type GlobalImplem = sig
  type sassign_hconsed
  include GlobalDS with type Assign.t = (sassign_hconsed,unit,int,int,EmptyInfo.infos) poly
  module SAssign : sig
    include PHCons with type t = sassign_hconsed
    val reveal : t -> sassign
    val build  : sassign -> t
  end
end

       
module type WhiteBoard = sig

  module DS : GlobalImplem

  open DS

  type 'a t = private WB of unit HandlersMap.t * (unit,'a) Msg.t
  val pp       : Format.formatter -> 'a t -> unit
  val stamp    : (_*('a*_*_*_)) Tags.t -> ('a, 'b) Msg.t -> 'b t
  val stamp_Eq : (Eq.MyTheory.sign, 'b) Msg.t -> 'b t
  val sat_init : Assign.t -> sat t
  val sat      : sat t -> sat t -> sat t
  val unsat    : straight t -> unsat t
  val resolve  : straight t -> 'b propa t -> 'b propa t
  val curryfy  : ?assign:Assign.t -> ?flip:bassign -> unsat t -> straight t
end

type (_,_) proj =
  | Proj : ('cv -> 'v values option) -> ('cv,'v has_values) proj
  | NoProj : ('cv,has_no_values) proj

module type API = sig
  module WB : WhiteBoard
  open WB.DS

  module EGraph : Eq.Interfaces.API with type sign = Eq.MyTheory.sign
                                     and type termdata = Term.datatype
                                     and type value  = Value.t
                                     and type cval   = CValue.t
                                     and type assign = Assign.t
                                     and type tset   = TSet.t

  val th_modules : (Term.datatype*Value.t*Assign.t*TSet.t) Modules.t list
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
