(******************************************************************)
(* This is a module based on the 2-watched literals technique in
   SAT-solving, which can be generalised and used for an arbitrary
   notion of "constraint" on variables.

   In SAT-solving, a constraint would be a clause, the variables being
   boolean variables; in linear arithmetic, a constraint would be an
   inequation, and variables would be arithmetic variables.

   The goal is to quickly identify which constraints become "unit
   constraints", i.e. contraints where only one variable is left
   indeterminate. Unit constraints usually lead to an action to
   perform: in SAT-solving, a clause with only one indeterminate
   literal forces this literal to be true; in arithmetic, an
   inequation with only one indeterminate variable imposes a lower or
   upper bound on the values that this variable may accept.

   The technique consists in making every constraint with at least 2
   indeterminate variables "watch" 2 of these. As soon as one of the 2
   watched variables becomes determined, the constraint needs to pick
   another indeterminate variable to watch: if it is impossible, it
   means the constraint has become a unit constraint. *)
(******************************************************************)

open General
open SetConstructions

module type Config = sig
  (* Provides the datastructure for constraints *)
  module Constraint: FromHConsed
  (* Provides the datastructure for variables *)
  module Var: Map.OrderedType
  (* Type of the data-structures recording which variables are determined *)
  type fixed

  (* simplify fixed c

     simplifies constraint c according to currently fixed variables
     ATTENTION: the output and the input hould have the same id
   *)
  val simplify: fixed -> Constraint.t -> Constraint.t

  (* pick_another constraint number previous var

     looks for (number) new variables that constraint can watch:
     - previous is the previous list of variables that contraint watched
     (its length might be different from (number), typically for a new 
     constraint, previous will be the empty list).

     The output provides (Some varlist), if varlist is the new list of length
     (number) of variable to watch, or None, if (number) could not be reached. *)
  val pick_another: fixed
                    -> Constraint.t
                    -> int
                    -> Var.t list
                    -> Var.t list option
end

(* Given such a configuration module C, Make(C) provides an
implementation of the 2-watched literals technique: *)

module Make(C : Config) : sig
  open C

  (* Type of the datastructures recording who watches what *)
  type t

  (* Initial datatstructure where nobody whatches nobody *)
  val init : t

  (* fix var whowatcheswhat

     declares in whowatcheswhat that var should no longer be watched.
     Whichever constraint was watching it is declared as "needing to
     watch another variable" instead. *)
  val fix  : Var.t -> t -> t

  (* addconstraint constraint varlist whowatcheswhat

     adds, to the datastructure whowatcheswhat, a constraint
     constraint watching varlist. *)

  val addconstraint : Constraint.t -> Var.t list -> t -> t

  (* addconstraintNflag constraint varlist whowatcheswhat

     same as above, but also flags constraint for scheduling new
     watched variables picking *)
  val addconstraintNflag : Constraint.t -> Var.t list -> t -> t

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
  val next : fixed -> ?howmany:int -> t -> (Constraint.t*Var.t list) option * t

end

val pick_another_make
    : is_empty:('varset -> bool) ->
      mem     :('var -> 'varset -> bool) ->
      next    :('varset -> 'var * 'varset) ->
      remove  :('var -> 'varset -> 'varset) ->
      'varset ->
      int ->
      'var list ->
      'var list option
