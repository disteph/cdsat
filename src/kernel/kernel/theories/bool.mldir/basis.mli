open Top.Specs

open Prop.Literals

open General
open Patricia
open Patricia_interfaces
open SetConstructions

open MyStructures
       
module Make(DS: sig 
                include GTheoryDSType
                val proj: Term.datatype -> ThDS.t
              end) : sig

  open DS
         
  module LMap : PATMapType with type keys = LitF.t
                           and  type values = Term.t
                           and  type infos  = unit
                           and  type ('v,'i) param = (LitF.t,'v,I.common,I.branching,'i) poly
                           and  type t = (LitF.t,Term.t,I.common,I.branching,unit) poly

  (* From a literal, give me the simplest term
     that is abstracted as that literal.
     Can only have 1 negation at the top-level. *)
                           
  val litAsTerm : LitF.t -> Term.t

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
                           
  module Model : sig
      type t
      val reveal : t -> LMap.t*LMap.t
      val empty : t
      val add : LitF.t -> t -> (Term.t*t)
  end

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
      include FromHConsed
      val make : Term.t -> t
      val term : t -> Term.t
      val simpl: t -> (LSet.t*(Model.t list), Term.t option) Sums.sum
      val verysimpl: t -> (LSet.t, Term.t option) Sums.sum
      val simplify : Model.t->t->t
      val print_in_fmt : Format.formatter -> t -> unit
  end  

  (* This is a type abbreviation for those propoagation messages that
  we will send to the outside world. *)

  val clear : unit -> unit

end
