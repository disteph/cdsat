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
open Monads

module type Config = sig

  module M : Monad

  (* Provides the datastructure for constraints *)
  module Constraint: sig
    type t [@@deriving show]
    val id : t -> int
  end              
  (* Provides the datastructure for variables *)
  module Var: sig
    type t [@@deriving ord,show]
  end

  (* simplify c

     simplifies constraint c according to currently fixed variables
     ATTENTION: the output and the input should have the same id *)

  val simplify: Constraint.t -> Constraint.t M.t

  (* pick_another constraint number previous var

     looks for (number) variables that constraint can watch (not fixed):
     - previous is the previous list of variables that contraint watched
     (its length might be different from (number), typically for a new 
     constraint, previous will be the empty list).

     The output provides a new list of variables to watch,
     of length (number) if possible, or as close to (number) as possible if not.

     NOTE: simplify is systematically called before pick_another,
     so when writing pick_another, you can assume the constraint is simplified
 *)

  val pick_another: Constraint.t -> int -> Var.t list -> Var.t list M.t
end

(* Given such a configuration module C, Make(C) provides an
implementation of the n-watched literals technique as a module of type S: *)

module type S = sig

  module C : Config
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
  val next : ?howmany:int -> t -> ((Var.t list,Constraint.t*Var.t list) sum * t) M.t

end
