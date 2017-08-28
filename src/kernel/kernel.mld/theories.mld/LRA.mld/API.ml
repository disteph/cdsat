open General
open Patricia
open Patricia_interfaces
open Patricia_tools
open Sums       
       
open Top
open Basic
open Messages
open Specs
open Sassigns

open Termstructures.Rationals

module type API = sig
  type sign
  type assign
  type termdata
  type value
  type tset
  type nonrec bassign = (termdata termF,value) bassign
  type nonrec sassign = (termdata termF,value) sassign

  val vinj : Q.t -> value
  val vproj: value -> Q.t option

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
    val term    : t -> termdata termF
    val scaling : t -> Q.t
    val nature  : t -> TS.nature
    val coeffs  : t -> TS.VarMap.t
    val constant  : t -> Q.t
    val watchable : t -> int list
    val justif  : t -> assign
    val simplify: Model.t-> t-> t
    val make    : termdata termF -> t
  end

  type state
  val init : state

  type eval =
    | Beval   of (sign, assign*bassign*tset, straight) message
    | Qeval   of termdata termF * Q.t
    | Unit    of { var         : int;
                   nature      : TS.nature;
                   is_coeff_pos: bool;
                   rhs_cst     : Q.t }
    | ToWatch of int list

  exception IdontUnderstand

  val eval  : Simpl.t -> eval
  val fm    : bassign -> bassign -> int -> (sign, assign*bassign*tset, straight) message
  (* Outputs sat message if all terms to satisfy in state have been satisfied *)
  val sat   : Model.t -> state -> state
              * (sign, assign*bassign*tset, sat) message option

  val add   : sassign -> state -> state * (Simpl.t, Q.t) Sassigns.sassign option
  val share : tset -> state -> Simpl.t list * state
  val clear : unit -> unit
end
