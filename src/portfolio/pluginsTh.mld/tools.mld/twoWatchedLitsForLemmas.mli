(******************************************************************)
(* This is a module based on the 2-watched literals technique in
   SAT-solving, which can be generalised and used for an arbitrary
   notion of "constraint" on variables, and n variables to watch
   instead of 2.

   In SAT-solving, a constraint would be a clause, the variables being
   boolean variables; in linear arithmetic, a constraint would be an
   inequation, and variables would be arithmetic variables.

   If n=2, the goal is to quickly identify which constraints become
   "unit constraints" or "constant constraints", i.e. contraints where
   only one variable is left indeterminate, or zero. Unit or constant
   constraints usually lead to an action to perform: in SAT-solving, a
   clause with only one indeterminate literal forces this literal to
   be true; in arithmetic, an inequation with only one indeterminate
   variable imposes a lower or upper bound on the values that this
   variable may accept.

   The technique consists in making every constraint with at least n
   indeterminate variables "watch" n of these. As soon as one of the n
   watched variables becomes determined, the constraint needs to pick
   another indeterminate variable to watch: if it is impossible, it
   means the constraint has strictly fewer than n indeterminate
   variables, and the algorithm reports the situation. *)
(******************************************************************)

open General
open Sums
open Patricia_tools

module type Config = sig
  (* Provides the datastructure for constraints *)
  module Constraint: sig
    type t [@@deriving show]
    val id : t -> int
  end              
  (* Provides the datastructure for variables *)
  module Var: sig
    type t [@@deriving ord,show]
  end
  (* Type of the data-structures recording which variables are determined *)
  type fixed

  (* simplify fixed c

     simplifies constraint c according to currently fixed variables
     ATTENTION: the output and the input should have the same id *)

  val simplify: fixed -> Constraint.t -> Constraint.t

  (* pick_another fixed constraint number previous var

     looks for (number) variables that constraint can watch (not fixed by fixed):
     - previous is the previous list of variables that contraint watched
     (its length might be different from (number), typically for a new 
     constraint, previous will be the empty list).

     The output provides a new list of variables to watch,
     of length (number) if possible, or as close to (number) as possible if not.

     NOTE: simplify is systematically called before pick_another,
     so when writing pick_another,
     you can assume the constraint is simplified w.r.t fixed
 *)

  val pick_another: fixed
                    -> Constraint.t
                    -> int
                    -> Var.t list
                    -> Var.t list
end

(* Given such a configuration module C, Make(C) provides an
implementation of the n-watched literals technique: *)

module Make(C : Config) : sig
  open C

  (* Type of the datastructures recording who watches what *)
  type t

  (* Initial datatstructure where nobody whatches nobody *)
  val init : t

  (* Flushing the queue of constraints whose watch list must be updated *)
  val flush : t -> t

  (* fix var whowatcheswhat

     declares in whowatcheswhat that var should no longer be watched.
     Whichever constraint was watching it is declared as "needing to
     watch another variable" instead. *)
  val fix  : Var.t -> t -> t

  (* addconstraint constraint ~watched whowatcheswhat

     adds constraint to the datastructure whowatcheswhat, watching watched *)
  val addconstraint : Constraint.t -> watched:(Var.t list) -> t -> t

  (* addconstraint constraint ~ifpossible whowatcheswhat

     Same as above but schedules constr for new watched variable picking,
     if possible reusing ifpossible *)
  val addconstraintNflag : Constraint.t -> ?ifpossible:(Var.t list) -> t -> t                                                                   

  (* next fixed ?howmany whowatcheswhat

     triggers computation: constraints in whowatcheswhat that were
     declared as "needing to watch new variables" will effectively
     pick (?howmany) new variables to watch that are not in fixed.
     If (?howmany) is not specified, it will be the same number as 
     the constraint used to watch.

     The first clause that fails to do so, call it constraint, stops
     the computation and is output as Some(constraint,watched);
     where watched is the (unchanged) list of variables that constraint 
     watched; if all constraints manage to do so we get None. 
     In both cases, the new state of whowatcheswhat is also output. *)
  val next : fixed -> ?howmany:int -> t
             -> (Var.t list,Constraint.t*Var.t list) sum * t

  (* incrscore constr whowatcheswhat *)
  val incrscore : Constraint.t -> t -> t

  (* getscore constr whowatcheswhat *)
  val getscore : Constraint.t -> t -> int
    
end

val pick_another_make
    : is_empty:('varset -> bool) ->
      mem     :('var -> 'varset -> bool) ->
      next    :('varset -> 'var * 'varset) ->
      remove  :('var -> 'varset -> 'varset) ->
      'varset ->
      int ->
      'var list ->
      'var list
