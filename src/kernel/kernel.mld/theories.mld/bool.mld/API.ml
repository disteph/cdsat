open General
open Patricia
open Patricia_interfaces
open Patricia_tools
open Sums       

open Top
open Messages
open Specs
open Sassigns
open Termstructures.Literals
open Termstructures.Clauses


module type API = sig
  type sign
  type assign
  type termdata
  type value
  type tset
  type nonrec bassign = (termdata termF,value) bassign
  type nonrec sassign = (termdata termF,value) sassign

  module LMap : PatMap with type keys = LitF.t
                       and  type values = bassign
                       and  type ('v,'i) param = (LitF.t,'v,I.common,I.branching,'i) poly
  module Model : sig
    type t
    val empty : t
    val add : bassign -> t -> (LitF.t*t, (sign,assign*bassign*tset,unsat) message) sum
    val map : t -> LMap.t (* The model as an LMap from true lits to bassign *)
  end

  module Constraint : sig
    include FromHConsed
    val make    : bassign -> t
    val bassign : t -> bassign
    (* Get simplified form of a constraint:
         seeing it as a clause,
         is None if clause is simplified to True,
         is Some(lset,watched) if lset are unassigned literals of original literals
                               and watched are the first 2 of them (or 0, or 1) *)
    val simpl: t -> (LSet.t * LitF.t list) option
    val simplify : Model.t->t->t
    val pp : Format.formatter -> t -> unit
  end

  type state
  val init : state

  type interesting =
    | Falsified of (sign,assign*bassign*tset, unsat) message
    | Unit of (sign, assign*bassign*tset, straight) message
    | Satisfied
    | ToWatch   of LSet.t * LitF.t list

  (* Looks at simplified form of constraint and outputs
     - Satisfied f if clause is true
       ( (f state) will remove the term from the terms yet to satisfy in state )
     - Falsified msg if no lit can be watched, where msg is the unsat message
     - Unit msg if only one lit can be watched, where msg is the propagation message
       This only happens if the clause is not identical to the propagated literal,
       otherwise Satisfied is produced.
     - Nothing2say if at least 2 lits can be watched *)
  val infer : Constraint.t -> interesting

  (* Outputs sat message if all terms to satisfy in state have been satisfied *)
  val sat   : Model.t -> state -> state
              * (sign, assign*bassign*tset, sat) message option

  (* Adds new assignment to the state. Outputs the new state, together with
     - either None if the Boolean assignment has been recorded as needing to be satisfied
     - or a list of implied Boolean assignments (technically, the propagation messages)
       that have been recorded as needing to be satisfied
     (If there is nothing to do, it will be (Some []),
      e.g. if the assignment is not Boolean) *)
  val add   : sassign -> state -> state
              * ((sign,assign*bassign*tset,straight) message list,Constraint.t) sum
  val share : tset -> state -> state
  val clear: unit -> unit
end
