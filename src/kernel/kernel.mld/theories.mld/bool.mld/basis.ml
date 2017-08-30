open General
open Patricia
open Patricia_interfaces
open Patricia_tools
open Sums
       
open Top
open Specs
open Sassigns
open Messages
       
open Termstructures.Literals
open Termstructures.Clauses
       
module Make(DS: DSproj with type ts = TS.t) = struct

  open DS
         
  (* Module for maps from literals to Boolean assignments *)
  module Arg = struct
    include LitF
    let pp_i fmt i = Format.fprintf fmt "<%a>" Term.pp (Term.term_of_id i)
    let pp fmt t = print_in_fmt ~print_atom:pp_i fmt t
    type values   = bassign
    include EmptyInfo
    let treeHCons = None (* Some(LitF.id,Terms.id,Terms.equal) *)
  end
  module LMap = struct
    include PatMap.Make(Arg)(I)
    type binding = Arg.t*bassign [@@deriving show]
    let pp fmt t = print_in_fmt pp_binding fmt t
  end

  (* Module for Boolean models. 
     We keep a bit more information than just "which lits are true":
     we map a true literal to the Boolean assignment that made it true *)
                           
  module Model : sig
    type t
    val empty : t
    val add : bassign -> t -> (LitF.t*t, (unit,unsat) Msg.t) sum
    val map : t -> LMap.t
  end = struct

    type t = LMap.t [@@deriving show]

    let empty = LMap.empty

    let add ((term,Values.Boolean b) as bassign) m =
      Print.print ["kernel.bool",2]
        (fun p->p "kernel.bool records Boolean assignment %a in Boolean model %a "
                  pp_bassign bassign pp m);
      let l  = LitF.build (b,Term.id term) in
      let nl = LitF.negation l in
      if LMap.mem nl m
      then
        begin
          Print.print ["kernel.bool",5]
            (fun p->p "Lit %a already set to true!" Arg.pp nl);
          let sassign1 = SAssign bassign in
          let sassign2 = SAssign (LMap.find nl m) in
          Case2(unsat () (Assign.add sassign1 (Assign.singleton sassign2)))
        end
      else
        begin
          Print.print ["kernel.bool",5]
            (fun p->p "Lit %a was not already set to true" Arg.pp nl);
          let dejavu _ = bassign in
          let m = LMap.add l dejavu m in
          Case1(l,m)
        end
          
      let map m = m

  end

  let clause (t,Values.Boolean b) =
    let data = proj (Terms.data t) in
    if b then data.asclause else data.ascube

  let cube (t,Values.Boolean b) =
    let data = proj (Terms.data t) in
    if b then data.ascube else data.asclause

          
  (*******************************************************************)
  (* These are the ingredients to feed the 2-watched literals module *)
  (*******************************************************************)

  (* Constraints are Boolean assignments, implemented as a record:
    - bassign: the original Boolean assignment.
    For brevity, what is below assumes the boolean is true
    (when it is false, switch clause for cube, disjunction for conjunctions, etc)
    - simpl: represents the simplified version of the term (seen as a clause)
             according to the current model, and is either (Some lset) or None.
      It is Some(lset,watchable) if no literal in the clause is yet set to true.
      lset is the set of literals in the clause whose truth-value is undetermined,
      i.e. the literals in the clause that were set to false have been removed.
      watchable is a list of 2 (or a maximum number of) literals from lset to be watched
      It is None if one of the literals was set to true.
    - simpl_just is the collection of assignments 
      that have contributed to the simplification of the clause
   *)

  module Constraint : sig
    type t [@@deriving show]
    val id: t -> int
    val make : bassign -> t
    val bassign : t -> bassign
    val simpl   : t -> (LSet.t * LitF.t list) option
    val justif  : t -> Assign.t
    val simplify: Model.t->t->t
  end = struct

    type t = { bassign : bassign;
               simpl   : (LSet.t * LitF.t list) option;
               justif  : Assign.t; }

    (* Picking 2 lits in a clause, or the maximum thereof *)
    let pick2 model c watched =
      LSet.fold_monad
        ~return:(fun x-> Some(c,x),Assign.empty)
        ~bind:(fun reccall todo res ->
          match res with
          | Some(_,watched),_ when List.length watched < 2 -> reccall todo watched
          | _ -> res)
        (fun lit watched ->
          let l = LitF.negation lit in
          if LMap.mem l model
          then None,Assign.singleton(SAssign(LMap.find l model))
          else Some(c,lit::watched),Assign.empty)
        c
        watched
        
    let make ((t,Values.Boolean b) as bassign) =
      let simpl,justif =
        match clause bassign with
        | Some c -> pick2 LMap.empty c []
        | None -> None, Assign.empty
      in
      { bassign = bassign;
        simpl = simpl;
        justif = justif }

    let id c =
      let t,Values.Boolean b = c.bassign in
      2*(Term.id t)+(if b then 1 else 0)

    let bassign c = c.bassign
    let simpl c   = c.simpl
    let justif c  = c.justif

    (* simplify model c
       simplifies clause c according to the
       currently fixed literals, given as model.
       This operation selects 2 literals to watch and stops as soon as 2 have been found.
       the assignments that have falsified the literals seen along the way
       (and not selected for the watch list), are added to c.simpl_justif
     *)

    let action model =
      LMap.Fold2.{
          sameleaf = (fun lit () bassign watched ->
            (* Literal lit is set to false in the model, can't pick it to watch *)
            Some(LSet.empty,watched),
            Assign.singleton(SAssign bassign));

          (* No literals in this part of the exploration *)
          emptyfull= (fun _ watched -> Some(LSet.empty,watched), Assign.empty);

          (* All literals in this part of the clause are unassigned.
             we try to complete watched to 2: *)
          fullempty= pick2 model;
          
          combine = make_combine LSet.empty LMap.empty
                      (fun treat rset rmap ((res1,justif1) as ans1) ->
                        (* Clause is split in two, ans1 is the result from the left exploration.
               (treat rset rmap) is the job to do for the right exploration. *)
                        match res1 with
                        | Some(lset1,watched1) when List.length watched1 < 2
                          -> begin match treat rset rmap watched1 with
                             | Some(lset2,watched2),justif2
                               -> Some(LSet.union lset1 lset2,watched2),
                                  Assign.union justif1 justif2
                             | ans2 -> ans2
                             end
                        | Some(lset1,watched1) ->
                        (* If we already have 2 literals, don't look any further *)
                           Some(LSet.union lset1 rset,watched1), justif1
                        | None -> ans1 ) 
      }
                
    let simplify model constr =
      match constr.simpl with
      | None -> constr
      | Some(c,watched) ->
         let simpl, justif =
           LMap.fold2_poly (action (Model.map model)) c (Model.map model) []
         in
         match simpl with
         | Some(c',watched) ->
            Print.print ["kernel.bool",4] (fun p ->
                p "kernel.bool: Constraint %a is simplified to %a watching %a (justified by %a, adding to %a)"
                  pp_bassign constr.bassign
                  (List.pp Arg.pp) (LSet.elements c')
                  (List.pp Arg.pp) watched
                  Assign.pp justif
                  Assign.pp constr.justif );
            { constr with simpl; justif = Assign.union constr.justif justif }
         | None -> { constr with simpl; justif }

    let pp fmt t = Format.fprintf fmt "%a" pp_bassign t.bassign
    let show = Print.stringOf pp
  end

  let clear() = LSet.clear();LMap.clear()

end
