open General
open Patricia
open Patricia_tools
open Sums       

open Top
open Messages
open Terms
open Sassigns
open Termstructures


module type API = sig
  type sign

  module BMap : Map.S_NH with type keys = Term.t
                          and type values = bool * SAssign.t
                          and type common = int
                          and type branching = int

  module Model : sig
    type t
    val empty : t
    val add : SAssign.t -> t -> (t, (sign,unsat) message) sum
    val map : t -> BMap.t
  end

  module Constraint : sig
    type t [@@deriving show]
    val id: t -> int
    val make : BAssign.t -> t
    val bassign : t -> BAssign.t
    (* Get simplified form of a constraint:
         seeing it as a clause,
         is None if clause is simplified to True,
         is Some(lset,watched) if lset are unassigned literals of original literals
                               and watched are the first 2 of them (or 0, or 1) *)
    val simpl: t -> (Clauses.VarMap.t * BAssign.t list) option
    (* Returns the assignments that contribute 
         to making the constraint simplify to the above *)
    val justif: t -> Assign.t
    (* Actually performs the simplification. Note that it is performed lazily:
         as soon as 2 literals can be watched, the next literals are not even scanned,
         even if one of them would simplify it to True, for instance *)
    val simplify : t->Model.t->t
  end

  type state
  val init : state

  type interesting =
    | Falsified of (sign, unsat) message
    | Unit of (sign, straight) message
    | Satisfied
    | ToWatch   of Clauses.VarMap.t * BAssign.t list

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
  val sat   : Model.t -> state -> state * (sign, sat) message option

  (* Adds new assignment to the state. Outputs the new state, together with
     - either None if the Boolean assignment has been recorded as needing to be satisfied
     - or a list of implied Boolean assignments (technically, the propagation messages)
       that have been recorded as needing to be satisfied
     (If there is nothing to do, it will be (Some []),
      e.g. if the assignment is not Boolean) *)
  val add   : SAssign.t -> state
    -> state * ((sign,straight) message list,Constraint.t) sum
  val share : TSet.t -> state -> state
end
