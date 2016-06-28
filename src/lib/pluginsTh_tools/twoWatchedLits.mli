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
   *)
  val simplify: fixed -> Constraint.t -> Constraint.t

  (* pick_another constraint var

     looks for another variable that constraint can watch:
     - var is the other variable that constraint was watching (which
     may or may not, at this point, be determined, but in any case
     it should not be picked).

     The output provides (Some var'), if var' is the new variable to
     watch, or None, if no variable could be picked (in which case the
     constraint is unit or constant). *)
  val pick_another: Constraint.t -> Var.t -> (Var.t option)
end

(* Given such a configuration module C, Make(C) provides an
implementation of the 2-watched literals technique: *)

module Make(C : Config) : sig
  open C

  (* Type of the datastructures recording who watches what *)
  type t

  (* Initial datatstructure where nobody whatches nobody *)
  val init : t

  (* addconstraint constraint var1 var2 whowatcheswhat

     adds, to the datastructure whowatcheswhat, a constraint
     constraint, that watches var1 and var2.
     We assume that constraint does not already exist in
     whowatcheswhat, and that var1 and var2 are distinct. *)
  val addconstraint : Constraint.t -> Var.t -> Var.t -> t -> t

  (* fix var whowatcheswhat

     declares in whowatcheswhat that var should no longer be watched.
     Whichever constraint was watching it is declared as "needing to
     watch another variable" instead. *)
  val fix  : Var.t -> t -> t

  (* next fixed whowatcheswhat

     triggers computation: constraints in whowatcheswhat that were
     declared as "needing to watch another variable" will effectively
     pick a new variable to watch that is not in fixed.

     The first clause that fails to do so, call it constraint, stops
     the computation and is output as (Some constraint); if all
     constraints manage to do so we get None. In both cases, the new
     state of whowatcheswhat is also output. *)
  val next : fixed -> t -> Constraint.t option * t
end
