open Top
open Messages
open Specs

open Prop.Literals

open General
open SetConstructions

open MyStructures
       
type sign

module Make
  (DS: sig
       include GTheoryDSType
       val proj: Term.datatype -> ThDS.t
     end) : sig
  
  open DS
         
  module Constraint: sig
    include FromHConsed
    val make : Term.t -> t
    val verysimpl: t -> (LSet.t, Term.t option) Sums.sum
    val print_in_fmt : Format.formatter -> t -> unit
  end

  type fixed

  val simplify: fixed -> Constraint.t -> Constraint.t

  type straight = (sign,TSet.t,Messages.straight) message
  type stop = straight list * ((sign,TSet.t,unsat) message)

  val init_fixed : fixed

  (* type used in the following functions *)
  type result =
    | UNSAT     of stop
    | Propagate of fixed * LitF.t list

  val constreat : Constraint.t -> fixed -> result
  val fix : Term.t -> fixed -> result
    
  type msg =
    | Msg : (sign,TSet.t,_) message -> msg
    | SplitBut : (Term.t,unit) LSet.param -> msg

  val extract_msg: fixed -> (msg * fixed) option

  val split : LitF.t -> (sign,TSet.t,both) message
  val clear : unit->unit
end
