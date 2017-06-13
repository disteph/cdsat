open Top
open Messages
open Specs

open General
open SetConstructions

open Termstructures
open Literals
open Clauses
       
module type Type = sig

  type term
  type value
  type assign         
  type sign

  val proj : term -> Clauses.TS.t
         
  module Constraint: sig
    include FromHConsed
    val make : term -> t
    val verysimpl: t -> (LSet.t, term option) Sums.sum
    val pp : Format.formatter -> t -> unit
  end

  type fixed

  val simplify: fixed -> Constraint.t -> Constraint.t

  type straight = (sign,assign,Messages.straight) message
  type stop = straight list * ((sign,assign,unsat) message)

  val init_fixed : fixed

  (* type used in the following functions *)
  type result =
    | UNSAT     of stop
    | Propagate of fixed * LitF.t list

  val constreat : Constraint.t -> fixed -> result
  val fix : term -> fixed -> result
    
  type msg =
    | Msg : (sign,assign,_) message -> msg
    | SplitBut : (term,unit) LSet.param -> msg

  val extract_msg: fixed -> (msg * fixed) option

  val split : LitF.t -> (sign,assign,both) message
  val clear : unit->unit
end
