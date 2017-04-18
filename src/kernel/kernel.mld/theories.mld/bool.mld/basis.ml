open Top
open Specs

open General
open Patricia
open Patricia_interfaces
open SetConstructions

open Termstructures
open Literals
open Clauses
       
module Make(DS: sig 
                include GTheoryDSType
                val proj: Term.datatype -> Clauses.TS.t
              end) = struct

  open DS
         
  (* Module for maps from literals to terms *)

  module DMap = struct
    type keys      = LitF.t
    let kcompare   = LitF.compare
    type values    = Term.t
    type infos     = unit
    let info_build = empty_info_build
    let treeHCons  = None (* Some(LitF.id,Terms.id,Terms.equal) *)
  end

  module LMap = PATMap.Make(DMap)(I)

  (* From a literal, give me the simplest term
     that is abstracted as that literal.
     Can only have 1 negation at the top-level. *)
                           
  let litAsTerm l =
    let b,i = LitF.reveal l in
    if b then Term.term_of_id i
    else Term.bC Symbols.Neg [Term.term_of_id i]

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
  end = struct
    type t       = LMap.t*LMap.t
    let reveal t = t
    let empty    = LMap.empty,LMap.empty
    let add l (t,tn) =
      Dump.print ["bool",2]
        (fun p->p "Setting this term to true: %a" Term.pp (litAsTerm l));
      let ln = LitF.negation l in
      let dejavu = function
        | Some _ -> failwith "Literal already determined!"
        | None -> litAsTerm l
      in
      let t = LMap.add l dejavu t in
      let tn = LMap.add ln dejavu tn in
      LMap.find l t, (t,tn)
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
      val pp : Format.formatter -> t -> unit
  end = struct

    type t = {
        term : Term.t;
        simpl: (LSet.t*(Model.t list), Term.t option) Sums.sum;
      }

    let make t = {
        term = t;
        simpl =
          match (proj(Terms.data t)).asclause with
          | None      -> Sums.Case2 None
          | Some lset -> Sums.Case1(lset,[])               
      }

    let id c = Term.id c.term
    let term  c = c.term
    let simpl c = c.simpl
    let verysimpl c = match c.simpl with
      | Sums.Case1(lset,_) -> Sums.Case1 lset
      | Sums.Case2 s -> Sums.Case2 s

    (* simplify model c
       simplifies clause c according to the
       currently fixed literals, given as model *)

    let simplify model c =
      match c.simpl with

      (* If clause was already simplified to true, we do not touch it: *)
      | Sums.Case2 _ -> c

      (* If clause was not already simplified to true: *)
      | Sums.Case1(set,j) ->
         let astrue,asfalse = Model.reveal model in
         let inter = LMap.inter_poly (fun _ v () -> v) astrue set in
         if LMap.is_empty inter
         then
           (* If fixed literals do not make the clause true: *)

           let updated_clause = LSet.diff_poly set asfalse in
           { c with simpl = Sums.Case1(updated_clause, model::j ) }
         else
           (* If clause gets simplified to true: *)

           let _,term = LMap.choose inter in
           {c with simpl = Sums.Case2(Some term)}

    let pp fmt t =
      Format.fprintf fmt "%a" Term.pp t.term

    let show = Dump.stringOf pp
  end
       

  (* This is a type abbreviation for those propoagation messages that
  we will send to the outside world. *)

  let clear() = LSet.clear();LMap.clear()

end
