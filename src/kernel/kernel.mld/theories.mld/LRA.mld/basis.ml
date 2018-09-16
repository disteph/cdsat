open General
open Patricia
open Patricia_tools

open Top
open Basic
open Messages
open Terms
open Values
open Sassigns

open Termstructures

let vkey  = Values.Key.make(module struct include Qhashed let name = "Qvalue" end)     
let vinj  = Value.inj vkey
let vproj = Value.proj vkey

let proj = proj Rationals.key

(* We first define a notion of LRA model, as a map from LRA variables to (value,sassign),
   where value is a rational value,
   and sassign is the single assignment that gave that variable that value. *)
module Arg = struct
  include Term
  include TypesFromHConsed(Term)
  include EmptyInfo
  type values = Q.t * SAssign.t
end

module QMap = struct
  include Map.MakeNH(Arg)
  let pp_pair fmt (term,(v,_)) = pp_sassign Term.pp Qhashed.pp fmt (SAssign(term,Values.NonBoolean v))
  let pp = print_in_fmt ~wrap:("{","}") pp_pair
end

module Model = struct
  include QMap
  type binding = Arg.t*SAssign.t [@@deriving show]
  let map t = t
  let add sassign m =
    let SAssign(term,v) = SAssign.reveal sassign in
    match v with
    | Values.Boolean b -> m
    | Values.NonBoolean v ->
      Print.print ["kernel.bool",2]
        (fun p->p "kernel.bool records Boolean assignment %a in LRA model %a "
            SAssign.pp sassign QMap.pp m);
      match vproj v with
      | None -> m
      | Some q ->
        let aux = function
          | None -> q,sassign
          | Some _ -> failwith "Already have a value"
        in
        QMap.add term aux m
end

module Simpl = struct

  type simpl = { coeffs    : Rationals.VarMap.t;
                 constant  : Q.t;
                 watchable : Term.t list;
                 justif    : Assign.t; }

  type t = { term : Term.t;
             scaling : Q.t;
             nature  : Rationals.nature;
             simpl   : simpl } [@@deriving fields]

  let coeffs t    = t.simpl.coeffs
  let constant t  = t.simpl.constant
  let watchable t = t.simpl.watchable
  let justif t    = t.simpl.justif

  (* Picking 2 vars in a sum, or the maximum thereof *)
  let pick2 =
    Rationals.VarMap.fold_monad
      ~return:(fun watchable -> watchable)
      ~bind:(fun reccall todo watchable ->
          if List.length watchable < 2
          then reccall todo watchable
          else watchable)
      (fun var _ watchable -> var::watchable)


  (* simplify model c
     simplifies constraint c according to the currently fixed vars, given as model.
     This operation selects 2 vars to watch and stops as soon as 2 have been found.
     the assignments that have simplified the constraint, are added to c.justif
  *)

  let action =
    (* Variable var is assigned in the model, can't pick it to watch *)
    let sameleaf var coeff (value,sassign) watchable =
      { coeffs   = Rationals.VarMap.empty;
        constant = Q.(value * coeff);
        watchable;
        justif = Assign.singleton sassign }
    in
    (* No variable in this part of the exploration *)
    let emptyfull _ watchable =
      { coeffs   = Rationals.VarMap.empty;
        constant = Q.zero;
        watchable;
        justif   = Assign.empty }
    in
    (* All vars in this part of the constraint are unassigned.
           we try to complete watchable to 2: *)
    let fullempty coeffs watchable =
      { coeffs;
        constant  = Q.zero;
        watchable = pick2 coeffs watchable;
        justif    = Assign.empty }
    in
    (* Constraint is split in two, ans1 is the result from the left exploration.
             (treat rset rmap) is the job to do for the right exploration. *)
    let snh _ _ = failwith "should not happen" in
    let combine ~reccall rconstraint rmodel ans1 =
      if List.length ans1.watchable < 2
      then
        let ans2 = reccall rconstraint rmodel ans1.watchable in
        { coeffs    = Rationals.VarMap.union snh ans1.coeffs ans2.coeffs;
          constant  = Q.(ans1.constant + ans2.constant);
          watchable = ans2.watchable;
          justif    = Assign.union ans1.justif ans2.justif }
      else
        { ans1 with coeffs = Rationals.VarMap.union snh ans1.coeffs rconstraint }
    in
    Rationals.VarMap.Fold2.{ sameleaf; emptyfull; fullempty;
                             combine = make_combine
                                 ~empty1:Rationals.VarMap.empty
                                 ~empty2:Model.empty
                                 combine }

  let simplify model c =
    let simpl = Rationals.VarMap.fold2_poly action c.simpl.coeffs model [] in
    { c with simpl = { simpl with
                       constant = Q.(c.simpl.constant + simpl.constant);
                       justif = Assign.union c.simpl.justif simpl.justif } }

  let make term =
    let Rationals.{coeffs;constant;scaling; nature} = proj term in
    let simpl = { coeffs; constant;
                  watchable = pick2 coeffs [];
                  justif    = Assign.empty }
    in
    { term; scaling; nature; simpl }

  let pp fmt c =
    Rationals.VarMap.pp ~constant:(constant c) ~scaling:(scaling c) () fmt (coeffs c)
  let show = Print.stringOf pp
end
