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
       
module Make
    (DS: GlobalDS) (* (DS: DSproj with type ts = TS.t and type values = Q.t has_values) *)
    (Proj: sig
       val proj: DS.Term.datatype -> (DS.Term.datatype,DS.TSet.t) TS.t
       val conv: (Q.t has_values,DS.Value.t) conv
     end) : sig

  open DS

  val vinj: Q.t -> Value.t
  val vproj: Value.t -> Q.t option

  module VarMap : PatMap with type keys = int
                          and type values = Q.t*sassign
                          and type ('v,'i) param = (int,'v,int,int,'i) poly

  module Model : sig
      type t
      val empty : t
      val add : sassign -> t -> t
      val map : t -> VarMap.t
  end

  module Simpl : sig
    type t [@@deriving show]
    val term    : t -> Term.t
    val scaling : t -> Q.t
    val nature  : t -> nature
    val coeffs  : t -> Termstructures.Rationals.VarMap.t
    val constant  : t -> Q.t
    val watchable : t -> int list
    val justif  : t -> Assign.t
    val simplify: Model.t-> t-> t
    val make    : Term.t -> t
  end

end
