open General
open Patricia

open Top
open Terms
open Values
open Sassigns
       
open Termstructures

val vinj : Q.t -> Value.t
val vproj: Value.t -> Q.t option
val proj : Term.t -> Rationals.t
  
module QMap : Map.S_NH with type keys = Term.t
                        and type values = Q.t * SAssign.t
                        and type common = int
                        and type branching = int

module Model : sig
  type t
  val empty : t
  val add : SAssign.t -> t -> t
  val map : t -> QMap.t
end

module Simpl : sig
  type t [@@deriving show]
  val term    : t -> Term.t
  val scaling : t -> Q.t
  val nature  : t -> Rationals.nature
  val coeffs  : t -> Rationals.VarMap.t
  val constant  : t -> Q.t
  val watchable : t -> Term.t list
  val justif  : t -> Assign.t
  val simplify: Model.t-> t-> t
  val make    : Term.t -> t
end
