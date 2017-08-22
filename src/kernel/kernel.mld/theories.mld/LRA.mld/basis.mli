open General
open Patricia
open Patricia_interfaces
open Patricia_tools

open Top
open Basic
open Messages
open Specs
open Sassigns
       
open Termstructures.Rationals
       

module Make(DS: DSproj with type ts = TS.t and type values = Q.t has_values) : sig

  open DS

  val vinj: Q.t -> Value.t
  val vproj: Value.t -> Q.t option
                                    
  module Model : sig
      type t
      val empty : t
      val add : sassign -> t -> t
      val map : t -> (IntSort.t,Q.t*sassign,int,int,EmptyInfo.infos) poly
  end

  module Constraint : sig
    include FromHConsed
    val make : bassign -> t
    val bassign : t -> bassign
    val scaling : t -> Q.t
    val nature : t -> TS.nature
    val coeffs : t -> TS.VarMap.t
    val constant : t -> Q.t
    val watchable  : t -> IntSort.t list
    val justif  : t -> Assign.t
    val simplify: Model.t->t->t
    val pp : Format.formatter -> t -> unit
  end

end
