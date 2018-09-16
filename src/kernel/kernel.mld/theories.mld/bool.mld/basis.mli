open General
open Patricia
open Patricia_tools
open Sums
       
open Top
open Basic
open Terms
open Sassigns
open Messages

open Termstructures

val dskey : Clauses.t Key.t

val proj : Term.t -> Clauses.t

(* Module for Boolean models. 
   We keep a bit more information than just "which lits are true/false",
   as we record 2 maps:
   - first map: maps a literal to a term whose assumption makes it true
   - second map: maps a literal to a term whose assumption makes it false
   If l is mapped to t in one map, (not l) is mapped to t in the other map.
   In (add l t), we create the simplest term, call it term, abstracted as l
   and add to the first map of t a mapping from l to term
   and to the second map of t a mapping from (not l) to term.
   We return the new pair of map, plus the term term that we created. *)

module BMap : Map.S_NH with type keys = Term.t
                        and type values = bool * SAssign.t
                        and type common = int
                        and type branching = int
  
module Model : sig
  type t
  val empty : t
  val add : SAssign.t -> t -> (t, (unit,unsat) message) sum
  val map : t -> BMap.t
end

val clause : BAssign.t -> Clauses.VarMap.t option
val cube   : BAssign.t -> Clauses.VarMap.t option

(*******************************************************************)
(* These are the ingredients to feed the 2-watched literals module *)
(*******************************************************************)

(* Constraints are clauses, implemented as a record with 2 fields:
   - term: the original term representing the clause
   - simpl: represents the simplified version of the clause
           according to the current model,
           and is either Case1(lset, modelstack) or Case2(termoption).
    Case1(lset, modelstack) is for a clause, in which no literal is yet set to true.
    lset is the set of literals in the clause whose truth-value is undetermined,
    i.e. the literals in the clause that were set to false have been removed.
    modelstack is a stack of models:
    every time we have simplified the clause, we have pushed on the stack
    the model corresponding to the current trail, so as to keep
    track of why the clause was simplified in that way.
    Case2(termoption) is for a clause that has simplified to true.
    termoption is None if the clause was the true clause to start with
    or it is (Some term) if term is the term
    whose assumption in the trail makes the clause true *)

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
  val simplify : Model.t->t->t
end

