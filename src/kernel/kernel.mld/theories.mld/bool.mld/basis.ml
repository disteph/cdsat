open General
open Patricia
open Patricia_interfaces
open Patricia_tools
open Sums
       
open Top
open Specs
open Messages
       
open Termstructures.Literals
open Termstructures.Clauses
       
module Make(DS: DSproj with type ts = TS.t) = struct

  open DS
         
  (* Module for maps from literals to Boolean assignments *)
  module DMap = struct
    include LitF
    let pp fmt t = print_in_fmt fmt t
    type values   = bassign
    include EmptyInfo
    let treeHCons = None (* Some(LitF.id,Terms.id,Terms.equal) *)
  end
  module LMap = struct
    include PatMap.Make(DMap)(I)
    type binding = DMap.t*bassign [@@deriving show]
    let pp fmt t = print_in_fmt pp_binding fmt t
  end

  (* Module for Boolean models. 
     We keep a bit more information than just "which lits are true/false",
     as we record 2 maps:
     - first map: maps a literal to the Boolean assignment that made it true
     - second map: maps a literal to the Boolean assignment that made it false
     If l is mapped to bassign in one map, (not l) is mapped to bassign in the other map.
   *)
                           
  module Model : sig
    type t
    val empty : t
    val add : bassign -> t -> (LitF.t*LitF.t*t, (unit,unsat) Msg.t) sum
    val agree : bool -> t -> LMap.t
  end = struct

    type t = Model of LMap.t*LMap.t [@@deriving show]

    let empty = Model(LMap.empty,LMap.empty)

    let add ((term,b) as bassign) (Model(t,tn) as m) =
      Dump.print ["kernel.bool",2]
        (fun p->p "Recording this Boolean assignment (%a) in the Boolean model %a "
                  pp_bassign bassign pp m);
      let l_term  = (proj (Terms.data term)).aslit in
      let l,nl = if b then l_term, LitF.negation l_term
                 else LitF.negation l_term, l_term
      in
      if LMap.mem nl t
      then
        begin
          Dump.print ["kernel.bool",5]
            (fun p->p "Lit nl = %a already set to true!" DMap.pp nl);
          let sassign1 = Values.boolassign bassign in
          let sassign2 = Values.boolassign(LMap.find nl t) in
          Case2(unsat () (Assign.add sassign1 (Assign.singleton sassign2)))
        end
      else
        begin
          Dump.print ["kernel.bool",5]
            (fun p->p "Lit nl = %a was not already set to true" DMap.pp nl);
          let dejavu _ = bassign in
          let t = LMap.add l dejavu t in
          let tn = LMap.add nl dejavu tn in
          Case1(l,nl,Model(t,tn))
        end
          
      let agree b (Model(astrue,asfalse)) = if b then astrue else asfalse

  end

  let clause (t,b) =
    let data = proj (Terms.data t) in
    if b then data.asclause else data.nasclause

  let cube (t,b) =
    let data = proj (Terms.data t) in
    if b then data.nasclause else data.asclause

          
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
      include FromHConsed
      val make : bassign -> t
      val bassign : t -> bassign
      val simpl   : t -> (LSet.t * LitF.t list) option
      val justif  : t -> Assign.t
      val simplify: Model.t->t->t
      val pp : Format.formatter -> t -> unit
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
          if LMap.mem lit model
          then None,Assign.singleton(Values.boolassign(LMap.find lit model))
          else Some(c,lit::watched),Assign.empty)
        c
        watched
        
    let make ((t,b) as bassign) =
      let simpl,justif =
        match clause bassign with
        | Some c -> pick2 LMap.empty c []
        | None -> None, Assign.empty
      in
      { bassign = bassign;
        simpl = simpl;
        justif = justif }

    let id c =
      let t,b = c.bassign in
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
            Assign.singleton(Values.boolassign bassign));

          (* No literals in this part of the exploration *)
          emptyfull= (fun _ watched -> Some(LSet.empty,watched), Assign.empty);

          (* All literals in this part of the clause are unassigned.
             we try to complete watched to 2: *)
          fullempty= pick2 model;
          
          combine = (fun ((res1,justif1) as ans1) todo ->
            (* Clause is split in two, ans1 is the result from the left exploration.
               todo is the job to do for the right exploration. *)
            match res1 with
            | Some(lset1,watched1) when List.length watched1 < 2
              -> (match todo watched1 with
                 | Some(lset2,watched2),justif2
                   -> Some(LSet.union lset1 lset2,watched2), Assign.union justif1 justif2
                 | ans2 -> ans2 )
            | _ -> ans1 ) (* If we already have 2 literals, don't look any further *)
      }
                
    let simplify model constr =
      let _,b = constr.bassign in
      match constr.simpl with
      | None -> constr
      | Some(c,watched) ->
         let simpl, justif =
           LMap.fold2_poly (action (Model.agree (not b) model)) c (Model.agree b model) []
         in
         { constr with simpl = simpl; justif = Assign.union constr.justif justif }

    let pp fmt t =
      Format.fprintf fmt "%a" pp_bassign t.bassign
  end

  let clear() = LSet.clear();LMap.clear()

end
