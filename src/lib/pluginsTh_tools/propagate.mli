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

  (* We require a type stop for the data to return when we find UNSAT, a
  type tset for sets of terms, again to be used in the messages. *)
  type stop
  type msg

  (* We are using type fixed from TwoWatchedLits.Config
     in order to, e.g.:

     Record the effect of unit constraints on the values that
     indeterminate variables can still accept. Typically, it contains
     a map from variables to (the implementation of) a range.

     For instance in arithmetic, variables will be mapped to
     intervals. The effect of a unit constraint such as x<3 may be to
     shrink that interval. *) 

  (* init_fixed is the initial structure recording which variables are
     fixed (at that point: none of them), as well as the range of
     possible values for all variables, before these are restricted by
     constraints *)
  val init_fixed : fixed

  (* type used in function constreat *)

  type result =
    | UNSAT     of stop
    | Propagate of fixed * Var.t list
    | Meh       of fixed

  (* constreat constraint fixed
     treats a constraint that has popped up as not being able to watch enough
     variables that aren't fixed in fixed.
     In the 2-watched case, constraint is unit or constant, in which case 
     we provide one of three results:
     -- the range of a variable has become empty, we return UNSAT msg,
        where typically msg is the message announcing the conflict;
     -- the range of a variable has become a singleton, we return
        Propagate(fixed,varlist), where varlist are the variables whose value
        have become forced and fixed is the new structure recording the
        determined variables (now including varlist);
     -- we have recorded the unit or constant constraint, and no
        conflict or propagation has been triggered, we return Meh.
  *)
  val constreat  : Constraint.t -> fixed -> result


  (* extract_msg fixed
     extracts from fixed a message declaring a propagation
     that has been performed *)
  val extract_msg: fixed -> (msg * fixed) option

end

(* Given such a configuration module C, Make(C) provides an
implementation of the propagation technique: *)

module Make(C: Config) : sig

  (* Type of the datastructures for the propagation *)
  type t 
  (* Initial state of this datastructure *)
  val init  : t
  (* treat constraint state     
     treats the addition of a new constraint:
     - either a conflict is detected, in which case we get Sums.A stop,
     where stop is the data resulting from the conflict
     - or we return a new state, where all propagations have been
     performed without generating a conflict, together with some messages describing the propagations that we have performed. 
  *)
  val treat : C.Constraint.t -> int -> t -> (C.stop, t) Sums.sum
  val extract_msg: t -> C.msg option * t
end