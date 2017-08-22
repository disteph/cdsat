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
       
type sign = unit

(* Values are  *)
include Theory.HasValues(Qhashed)

(* Our alternative term representation *)
type ts = TS.t
let ts = Termstructures.Register.Rationals

module Make(DS: DSproj with type ts = ts and type values = values) = struct

  open DS
  type datatypes = Term.datatype*Value.t*Assign.t*TSet.t
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



  module Constraint : sig
    include FromHConsed
    val make : bassign -> t
    val bassign : t -> bassign
    val s_coeffs : t -> TS.VarMap.t
    val s_constant : t -> Q.t
    val justif  : t -> Assign.t
    val simplify: Model.t->t->t
    val pp : Format.formatter -> t -> unit
  end = struct

    type simpl = { s_coeffs : TS.VarMap.t;
                   s_constant: Q.t;
                   watched  : IntSort.t list;
                   justif   : Assign.t; }
                   
    type t = bassign * simpl

    (* Picking 2 vars in a sum, or the maximum thereof *)
    let pick2 c watched =
      TS.VarMap.fold_monad
        ~return:(fun watched -> watched)
        ~bind:(fun reccall todo watched ->
          if List.length watched < 2
          then reccall todo watched
          else watched)
        (fun var _ watched -> var::watched)
        c
        watched
        
    let make ((t,Values.Boolean _) as bassign) =
      let data = proj(Terms.data t) in
      bassign, { s_coeffs = data.TS.coeffs;
                 s_constant = data.TS.constant;
                 watched = pick2 data.TS.coeffs [];
                 justif = Assign.empty }

    let id (bassign,c) =
      let t,Values.Boolean b = bassign in
      2*(Term.id t)+(if b then 1 else 0)

    let bassign (bassign,_) = bassign
    let s_coeffs (_,s)      = s.s_coeffs
    let s_constant (_,s)    = s.s_constant
    let justif (_,s)        = s.justif

    (* simplify model c
       simplifies constraint c according to the currently fixed vars, given as model.
       This operation selects 2 vars to watch and stops as soon as 2 have been found.
       the assignments that have simplified the constraint, are added to c.justif
     *)

    let action =
      (* Variable var is assigned in the model, can't pick it to watch *)
      let sameleaf var coeff (value,sassign) watched =
        { s_coeffs   = TS.VarMap.empty;
          s_constant = Q.(value * coeff);
          watched;
          justif = Assign.singleton sassign }
      in
      (* No variable in this part of the exploration *)
      let emptyfull _ watched =
        { s_coeffs   = TS.VarMap.empty;
          s_constant = Q.zero;
          watched;
          justif = Assign.empty }
      in
      (* All vars in this part of the constraint are unassigned.
             we try to complete watched to 2: *)
      let fullempty coeffs watched =
        { s_coeffs   = coeffs;
          s_constant = Q.zero;
          watched = pick2 coeffs watched;
          justif = Assign.empty }
      in
      (* Constraint is split in two, ans1 is the result from the left exploration.
               (treat rset rmap) is the job to do for the right exploration. *)
      let snh _ _ = failwith "should not happen" in
      let combine treat rconstraint rmodel ans1 =
        if List.length ans1.watched < 2
        then
          let ans2 = treat rconstraint rmodel ans1.watched in
          { s_coeffs   = TS.VarMap.union snh ans1.s_coeffs ans2.s_coeffs;
            s_constant = Q.(ans1.s_constant + ans2.s_constant);
            watched = ans2.watched;
            justif = Assign.union ans1.justif ans2.justif }
        else
          { ans1 with s_coeffs = TS.VarMap.union snh ans1.s_coeffs rconstraint }
      in
      TS.VarMap.Fold2.{ sameleaf; emptyfull; fullempty;
                        combine = make_combine TS.VarMap.empty Model.empty combine }
                
    let simplify model (bassign,constr) =
      let simpl = TS.VarMap.fold2_poly action constr.s_coeffs model [] in
      bassign,
      { simpl with
        s_constant = Q.(constr.s_constant + simpl.s_constant);
        justif = Assign.union constr.justif simpl.justif }

    let pp fmt (bassign,_) = Format.fprintf fmt "%a" pp_bassign bassign
  end


                                                    
  let add_myvars term myvars =
    let aux var _ = var |> IntSort.reveal |> fun (i,_) -> TSet.add (Term.term_of_id i) in
    TS.(VarMap.fold aux (DS.proj(Terms.data term)).coeffs myvars)

  type state = { assign : Assign.t;
                 sharing: TSet.t;
                 myvars : TSet.t Lazy.t }
                                           
  let rec machine state =
    let add = function
      | None ->
         Print.print ["kernel.LRA",2] (fun p ->
             p "kernel.LRA receiving None");
         Silence, machine state
      | Some sassign ->
         Print.print ["kernel.LRA",2] (fun p ->
             p "kernel.LRA receiving Some(%a)" pp_sassign sassign);
         let assign = Assign.add sassign state.assign in
         let SAssign(term,_) = sassign in
         let myvars = lazy(add_myvars term (Lazy.force state.myvars)) in
         let state = { state with assign; myvars } in
         Msg(sat () state.assign ~sharing:state.sharing ~myvars ),
         machine state
    in
    let share tset = 
      Print.print ["kernel.LRA",2] (fun p ->
          p "kernel.LRA notified than %a are shared" TSet.pp tset);
      let sharing = TSet.union tset state.sharing in
      let myvars = lazy(TSet.fold add_myvars tset (Lazy.force state.myvars)) in
      let state = { state with sharing; myvars } in
      Msg(sat () state.assign ~sharing ~myvars),
      machine state
    in
    let clone () = machine state in
    let suicide _ = () in
    Specs.SlotMachine { add; share; clone; suicide }

  let init = machine { assign=Assign.empty; sharing=TSet.empty; myvars=lazy TSet.empty }
  let clear () = ()
                   
end

module type API = sig
  type datatypes
  val init: (sign,datatypes) slot_machine
  val clear: unit -> unit
end

type ('t,'v,'a,'s) api = (module API with type datatypes = 't*'v*'a*'s)

let make (type t v a s)
      ((module DS): (ts,values,t,v,a,s) dsProj)
    : (t,v,a,s) api =
  (module Make(DS))
