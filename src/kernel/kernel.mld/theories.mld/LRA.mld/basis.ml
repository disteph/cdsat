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

  module Arg = struct
    include IntSort
    let pp fmt is =
      let i,_ = IntSort.reveal is in
      Format.fprintf fmt "<%a>" Term.pp (Term.term_of_id i)
    type values = Q.t * sassign
    include EmptyInfo
    let treeHCons = None (* Some(LitF.id,Terms.id,Terms.equal) *)
  end

  module Model = struct
    include PatMap.Make(Arg)(TypesFromHConsed(IntSort))
    type binding = Arg.t*sassign [@@deriving show]
    let pp fmt t = print_in_fmt pp_binding fmt t
    let map t = t
    let add sassign model =
      match sassign with
      | SAssign(t,Values.Boolean v) -> model
      | SAssign(t,Values.NonBoolean v) ->
         match vproj v with
         | None -> model
         | Some q ->
            let var = IntSort.build (Term.id t,Term.get_sort t) in
            let aux = function
              | None -> q,sassign
              | Some _ -> failwith "Already have a value"
            in
            add var aux model
  end



  module Constraint = struct

    type simpl = { coeffs : TS.VarMap.t;
                   constant: Q.t;
                   watchable  : IntSort.t list;
                   justif   : Assign.t; }
                   
    type t = { bassign : bassign;
               scaling : Q.t;
               nature  : TS.nature;
               simpl   : simpl }

    (* Picking 2 vars in a sum, or the maximum thereof *)
    let pick2 c watchable =
      TS.VarMap.fold_monad
        ~return:(fun watchable -> watchable)
        ~bind:(fun reccall todo watchable ->
          if List.length watchable < 2
          then reccall todo watchable
          else watchable)
        (fun var _ watchable -> var::watchable)
        c
        watchable

    exception IdontUnderstand
        
    let make ((t,Values.Boolean b) as bassign) =
      let data = proj(Terms.data t) in
      let scaling, nature =
        let open TS in
        match data.nature,b with
        | Lt,true -> data.scaling, Lt
        | Le,true -> data.scaling, Le
        | Lt,false -> Q.(minus_one * data.scaling), Le
        | Le,false -> Q.(minus_one * data.scaling), Lt
        | Eq,true  | NEq,false -> data.scaling, Eq
        | NEq,true | Eq,false  -> data.scaling, NEq
        | _ -> raise IdontUnderstand
      in
      let simpl = { coeffs    = data.TS.coeffs;
                    constant  = data.TS.constant;
                    watchable = pick2 data.TS.coeffs [];
                    justif    = Assign.empty }
      in
      { bassign; scaling; nature; simpl }
          

    let id c =
      let t,Values.Boolean b = c.bassign in
      2*(Term.id t)+(if b then 1 else 0)

    let bassign t   = t.bassign
    let scaling t   = t.scaling
    let nature t    = t.nature
    let coeffs t    = t.simpl.coeffs
    let constant t  = t.simpl.constant
    let watchable t = t.simpl.watchable
    let justif t    = t.simpl.justif

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
          justif = Assign.empty }
      in
      (* All vars in this part of the constraint are unassigned.
             we try to complete watchable to 2: *)
      let fullempty coeffs watchable =
        { coeffs;
          constant = Q.zero;
          watchable = pick2 coeffs watchable;
          justif = Assign.empty }
      in
      (* Constraint is split in two, ans1 is the result from the left exploration.
               (treat rset rmap) is the job to do for the right exploration. *)
      let snh _ _ = failwith "should not happen" in
      let combine treat rconstraint rmodel ans1 =
        if List.length ans1.watchable < 2
        then
          let ans2 = treat rconstraint rmodel ans1.watchable in
          { coeffs   = TS.VarMap.union snh ans1.coeffs ans2.coeffs;
            constant = Q.(ans1.constant + ans2.constant);
            watchable = ans2.watchable;
            justif = Assign.union ans1.justif ans2.justif }
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

    let pp fmt c = Format.fprintf fmt "%a" pp_bassign c.bassign
  end

                   
end
