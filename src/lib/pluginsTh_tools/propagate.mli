(**********************************************************************)
(* This module builds on the 2-watched literals technique implemented
   in twoWatchedLits.ml

   It provides a mechanism for performing all propagations that follow
   the addition of a constraint, outputing the result in the form of
   kernel messages. *)
(**********************************************************************)

open General
open SetConstructions
open Kernel.Top.Messages

module type Config = sig

  (* We require everything that was needed for the implementation of
  the 2-watched literals *)
  include TwoWatchedLits.Config

  (* We require a type sign for signing the messages to be output, a
  type tset for sets of terms, again to be used in the messages. *)
  type sign
  type tset

  (* Type constraints is the type of datastructures that record the
     effect of unit constraints on the values that indeterminate
     variables can still accept. Typically, it contains a map from
     variables to (the implementation of) a range.

     For instance in arithmetic, variables will be mapped to
     intervals. The effect of a unit constraint such as x<3 may be to
     shrink that interval. *) 
  type constraints

  (* init_fixed is the initial structure recording which variables are
     fixed; at that point: none of them *)
  val init_fixed : fixed

  (* init_constraints is the initial structure recording the range of
     possible values for all variables, before these are restricted by
     constraints *)
  val init_constraints : constraints

  (* type used in the following function *)
  type result = 
    | UNSAT of (sign,tset,thProvable) thsays
    | Propagate of fixed * Var.t
    | Meh

  (* constreat constraint
     treats the addition of constraint.
     2 cases occur: 
     - either there are at least 2 indeterminate variables in
       constraint, in which case we just output 2 of them;
     - or constraint is unit or constant, in which case we provide a
       function that affects a constraints map, returning a new one,
       together with one of 3 cases:
     -- the range of a variable has become empty, we return UNSAT msg,
        where msg is the message announcing the conflict;
     -- the range of a variable has become a singleton, we return
        Propagate(fixed,var), where var is the variable whose value
        has become forced and fixed is the new structure recording the
        determined variables (now including var);
     -- we have recorded the unit or constant constraint, and no
        conflict or propagation has been triggered, we return Meh.
  *)
  val constreat : 
    Constraint.t -> (Var.t*Var.t, constraints->(constraints*result)) Sums.sum

  (* extract_msg constraints
     extracts from constraints the message declaring the propagations
     that have been performed *)
  val extract_msg: constraints -> (sign,tset,thStraight) thsays option * constraints

end

(* Given such a configuration module C, Make(C) provides an
implementation of the propagation technique: *)

module Make(C: Config) : sig

  (* Type of the dtatstructures for the propagation *)
  type t 
  (* Initial state of this datastructure *)
  val init  : t
  (* treat constraint state
     
     treats the addition of a new constraint:
     - either a conflict is detected, in which case we get Sums.A msg,
     where msg is the conflict message
     - or we return a new state, where all propagations have been
     performed without generating a conflict
     In both cases, we may also get a message indicating the
     propagations that we have performed. 
  *)
  val treat :
    C.Constraint.t -> t ->
    (C.sign, C.tset, thStraight) thsays option *
      ((C.sign, C.tset, thProvable) thsays, t) Sums.sum
end
