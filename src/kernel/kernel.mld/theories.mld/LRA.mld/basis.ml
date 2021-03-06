open General
open Patricia
open Patricia_interfaces
open Patricia_tools

open Top
open Basic
open Messages
open Specs
open Sassigns
       
open Termstructures.Rationals
       
module Make(DS: DSproj with type ts = TS.t and type values = Q.t has_values) = struct

  open DS
  let HasVconv{vinj;vproj} = conv

  (* We first define a notion of LRA model, as a map from LRA variables to (value,sassign),
     where value is a rational value,
     and sassign is the single assignment that gave that variable that value. *)
  module Arg = struct
    type t = int [@@deriving ord]
    let id i = i
    let pp fmt i = Format.fprintf fmt "<%a>" Term.print_of_id i
    type values = Q.t * sassign
    let pp_binding fmt (i,_) = pp fmt i
    include EmptyInfo
    let treeHCons = None (* Some(LitF.id,Terms.id,Terms.equal) *)
  end

  module VarMap = PatMap.Make(Arg)(TypesFromHConsed(Arg))

  module Model = struct
    include VarMap
    type binding = Arg.t*sassign [@@deriving show]
    let map t = t
    let add sassign model =
      match sassign with
      | SAssign(_,Values.Boolean _) -> model
      | SAssign(t,Values.NonBoolean v) ->
         match vproj v with
         | None -> model
         | Some q ->
            let aux = function
              | None -> q,sassign
              | Some _ -> failwith "Already have a value"
            in
            add (Term.id t) aux model
  end

  module Simpl = struct

    type simpl = { coeffs    : TS.VarMap.t;
                   constant  : Q.t;
                   watchable : int list;
                   justif    : Assign.t; }

    type t = { term : Term.t;
               scaling : Q.t;
               nature  : TS.nature;
               simpl   : simpl }

    let term t      = t.term
    let scaling t   = t.scaling
    let nature t    = t.nature
    let coeffs t    = t.simpl.coeffs
    let constant t  = t.simpl.constant
    let watchable t = t.simpl.watchable
    let justif t    = t.simpl.justif

    (* Picking 2 vars in a sum, or the maximum thereof *)
    let pick2 =
      TS.VarMap.fold_monad
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
        { coeffs   = TS.VarMap.empty;
          constant = Q.(value * coeff);
          watchable;
          justif = Assign.singleton sassign }
      in
      (* No variable in this part of the exploration *)
      let emptyfull _ watchable =
        { coeffs   = TS.VarMap.empty;
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
      let combine treat rconstraint rmodel ans1 =
        if List.length ans1.watchable < 2
        then
          let ans2 = treat rconstraint rmodel ans1.watchable in
          { coeffs    = TS.VarMap.union snh ans1.coeffs ans2.coeffs;
            constant  = Q.(ans1.constant + ans2.constant);
            watchable = ans2.watchable;
            justif    = Assign.union ans1.justif ans2.justif }
        else
          { ans1 with coeffs = TS.VarMap.union snh ans1.coeffs rconstraint }
      in
      TS.VarMap.Fold2.{ sameleaf; emptyfull; fullempty;
                        combine = make_combine TS.VarMap.empty Model.empty combine }
                
    let simplify model c =
      let simpl = TS.VarMap.fold2_poly action c.simpl.coeffs model [] in
      { c with simpl = { simpl with
                         constant = Q.(c.simpl.constant + simpl.constant);
                         justif = Assign.union c.simpl.justif simpl.justif } }

    let make term =
      let data = proj(Terms.data term) in
      let simpl = { coeffs    = data.TS.coeffs;
                    constant  = data.TS.constant;
                    watchable = pick2 data.TS.coeffs [];
                    justif    = Assign.empty }
      in
      { term; scaling=data.TS.scaling; nature=data.TS.nature; simpl }

    let pp fmt c =
      let pp_var fmt i = Term.pp fmt (Term.term_of_id i) in
      TS.pp pp_var fmt (proj(Terms.data(term c)))
    let show = Print.stringOf pp
  end

end
