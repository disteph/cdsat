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
  end

  type fixed

  val simplify: fixed -> Constraint.t -> Constraint.t
  val pick_another: Constraint.t -> LitF.t -> (LitF.t option)

  type straight = (sign,TSet.t,thStraight) thsays
  type msg =
    | Msg : (sign,TSet.t,_) thsays -> msg
    | SplitBut : (Term.t,unit) LSet.param -> msg
  type stop = straight list * ((sign,TSet.t,thProvable) thsays) * Term.t

  val init_fixed : fixed

  (* type used in the following function *)
  type result =
    | UNSAT     of stop
    | Propagate of fixed * LitF.t list
    | Meh   of fixed
    | Watch of fixed * LitF.t * LitF.t

  val constreat  : Constraint.t -> fixed -> result

  val extract_msg: fixed -> (msg * fixed) option

  val split : LitF.t -> (sign,TSet.t,thAnd) thsays
  val unfold : Term.t -> (sign,TSet.t,thStraight) thsays option
end
