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

let dskey = Clauses.key
let proj  = Terms.proj dskey

(* Module for Boolean models. 
   We keep a bit more information than just "which lits are true":
   we map a true literal to the Boolean assignment that made it true *)

module Arg = struct
  include Term
  include TypesFromHConsed(Term)
  include EmptyInfo
  type values = bool * SAssign.t
end

module BMap = struct
  include Map.MakeNH(Arg)
  let pp_lit fmt (var,(b,_)) =
    Format.fprintf fmt "%s%a" (if b then "" else "~") Term.pp var
  let pp = print_in_fmt ~wrap:("{","}") pp_lit
end

module Model : sig
  type t
  val empty : t
  val add : SAssign.t -> t -> (t, (unit,unsat) message) sum
  val map : t -> BMap.t
end = struct

  type t = BMap.t
             
  let empty = BMap.empty

  let add sassign m =
    let SAssign(term,v) = SAssign.reveal sassign in
    match v with
    | Values.NonBoolean  _ -> Case1 m
    | Values.Boolean b ->
      Print.print ["kernel.bool",2]
        (fun p->p "kernel.bool records Boolean assignment %a in Boolean model %a "
            SAssign.pp sassign BMap.pp m);
      if BMap.mem term m
      then
        let b',sassign' = BMap.find term m in
        if [%eq:bool] b b'
        then failwith "Trying to record the same assignment twice"
        else
          begin
            Print.print ["kernel.bool",5]
              (fun p->p "Term %a already set to %b!" Term.pp term (not b));
            Case2(unsat () (Assign.add sassign (Assign.singleton sassign')))
          end
      else
        begin
          Print.print ["kernel.bool",5]
            (fun p->p "Term %a not alredy set" Term.pp term);
          Case1(BMap.add term (fun _ -> b,sassign) m)
        end

  let map m = m

end

let clause (t,Values.Boolean b) =
  t |> proj |> if b then Clauses.asclause else Clauses.ascube

let cube (t,Values.Boolean b) =
  t |> proj |> if b then Clauses.ascube else Clauses.asclause


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
  val simpl   : t -> (Clauses.VarMap.t * bassign list) option
  val justif  : t -> Assign.t
  val simplify: Model.t->t->t
end = struct

  type simpl = (Clauses.VarMap.t * bassign list) option
  type t = { bassign : bassign;
             simpl   : simpl;
             justif  : Assign.t; } [@@deriving fields]

  (* Picking 2 vars in a sum, or the maximum thereof *)
  let pick2 =
    Clauses.VarMap.fold_monad
      ~return:(fun watchable -> watchable)
      ~bind:(fun reccall todo watchable ->
          if List.length watchable < 2
          then reccall todo watchable
          else watchable)
      (fun var b watchable -> (var, Values.Boolean b)::watchable)

  let make ((t,Values.Boolean b) as bassign) =
    let simpl = clause bassign |> Opt.map (fun c -> c,pick2 c []) in
    { bassign; simpl; justif = Assign.empty }

  let id c =
    let t,Values.Boolean b = c.bassign in
    2*(Term.id t)+(if b then 1 else 0)


  (* simplify model c
     simplifies clause c according to the
     currently fixed literals, given as model.
     This operation selects 2 literals to watch and stops as soon as 2 have been found.
     the assignments that have falsified the literals seen along the way
     (and not selected for the watch list), are added to c.simpl_justif
  *)

  let action =
    Clauses.VarMap.Fold2.{
      sameleaf = (fun term b (b',sassign) watched ->
          (if [%eq:bool] b b'
           then (* Literal is set to false in the model, can't pick it to watch *)
             Some(Clauses.VarMap.empty,watched)
           else (* Literal is set to true in the model, clause is true *)
             None),
          Assign.singleton sassign);

      (* No literals in this part of the exploration *)
      emptyfull= (fun _model watched -> Some(Clauses.VarMap.empty,watched), Assign.empty);

      (* All literals in this part of the clause are unassigned.
         we try to complete watched to 2: *)
      fullempty= (fun lits watched -> Some(lits,pick2 lits watched), Assign.empty);

      combine =
        let disjoint _ _ _ = failwith "Should be disjoint union" in
        make_combine ~empty1:Clauses.VarMap.empty ~empty2:BMap.empty
          (fun ~reccall rclause rmodel ((res1,justif1) as ans1) ->
             (* Clause is split in two, ans1 is the result from the left exploration.
                (reccall rclause rmodel) is the job to do for the right exploration. *)
             match res1 with
             | Some(lset1,watched1) when List.length watched1 < 2
               -> begin match reccall rclause rmodel watched1 with
                   | Some(lset2,watched2),justif2
                     -> Some(Clauses.VarMap.union disjoint lset1 lset2,watched2),
                        Assign.union justif1 justif2
                   | ans2 -> ans2
                 end
             | Some(lset1,watched1) ->
               (* If we already have 2 literals, don't look any further *)
               Some(Clauses.VarMap.union disjoint lset1 rclause,watched1), justif1
             | None -> ans1 ) 
    }

  let simplify model constr =
    match constr.simpl with
    | None -> constr
    | Some(c,watched) ->
      let simpl, justif = Clauses.VarMap.fold2_poly action c (Model.map model) [] in
      match simpl with
      | Some(c',watched) ->
        Print.print ["kernel.bool",4] (fun p ->
            p "kernel.bool: Constraint %a is simplified to %a watching %a (justified by %a, adding to %a)"
              pp_bassign constr.bassign
              Clauses.VarMap.pp c'
              (List.pp pp_bassign) watched
              Assign.pp justif
              Assign.pp constr.justif );
        { constr with simpl; justif = Assign.union constr.justif justif }
      | None -> { constr with simpl; justif }

  let pp fmt t = Format.fprintf fmt "%a" pp_bassign t.bassign
  let show = Print.stringOf pp
end
