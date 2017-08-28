open General
open Sums
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
  type nonrec sign = sign
  type termdata = Term.datatype
  type value  = Value.t
  type assign = Assign.t
  type tset   = TSet.t
  type nonrec bassign = bassign
  type nonrec sassign = sassign

  include Basis.Make(DS)
                                                    
  let add_myvars term myvars =
    let aux var _ = TSet.add (Term.term_of_id var) in
    TS.(VarMap.fold aux (DS.proj(Terms.data term)).coeffs myvars)

  (* state type:
     in order to produce the message Sat(seen),
     one must satisfy each constraint in todo. *)
  type state = { seen      : Assign.t;
                 sharing   : TSet.t;
                 myvars    : TSet.t Lazy.t;
                 tosatisfy : (Simpl.t,Q.t) Sassigns.sassign list;
                 toevaluate: Simpl.t list }

  let init = { seen      = Assign.empty;
               sharing   = TSet.empty;
               myvars    = lazy TSet.empty;
               tosatisfy = [];
               toevaluate= [] }

  type eval =
    | Beval   of (sign,straight) Msg.t
    | Qeval   of Term.t * Q.t
    | Unit    of { var         : int;
                   nature      : TS.nature;
                   is_coeff_pos: bool;
                   rhs_cst     : Q.t }
    | ToWatch of int list

  exception IdontUnderstand
                
  let eval c =
    match Simpl.watchable c with
    | [] -> let value = Q.(Simpl.constant c * Simpl.scaling c) in
            let beval b = Beval(straight ()
                                  (Simpl.justif c)
                                  (Simpl.term c,Values.Boolean b))
            in
            let open TS in
            begin match Simpl.nature c with
            | Term                      -> Qeval(Simpl.term c, value)
            | Other                     -> raise IdontUnderstand
            | Lt when Q.sign value <0   -> beval true
            | Le when Q.sign value <=0  -> beval true
            | Eq when Q.sign value =0   -> beval true
            | NEq when Q.sign value <>0 -> beval true
            | _                         -> beval false
            end
    | [var] ->
       let coeff = Q.(Simpl.scaling c * TS.VarMap.find var (Simpl.coeffs c)) in
       let constant = Q.(Simpl.scaling c * Simpl.constant c) in
       Unit{ var;
             nature       = Simpl.nature c;
             is_coeff_pos = Q.sign coeff > 0;
             rhs_cst      = Q.(neg constant / coeff) }
    | watchable -> ToWatch watchable

  let take_sides term b =
    match Terms.reveal term with
    | Terms.C(Symbols.Lt,[lhs;rhs]) ->
       if b then lhs,rhs,true else lhs,rhs,false
    | Terms.C(Symbols.Le,[lhs;rhs]) ->
       if b then lhs,rhs,false else lhs,rhs,true
    | Terms.C(Symbols.Eq Sorts.Rat,[lhs;rhs]) when b ->
       lhs,rhs,false
    | _ -> failwith "Should not try Fourier-Motzkin with this"

  let times coeff term =
    if Q.equal coeff Q.one then term
    else Term.bC Symbols.Times [Term.bC (Symbols.CstRat coeff) []; term]

  let plus a b =
    match Terms.reveal a, Terms.reveal b with
    | Terms.C(Symbols.CstRat q,[]),_ when Q.equal q Q.zero -> b
    | _,Terms.C(Symbols.CstRat q,[]) when Q.equal q Q.zero -> a
    | _ -> Term.bC Symbols.Plus [a; b]
        
  let fm ba1 ba2 var =
    let e1,Values.Boolean b1 = ba1 in
    let e2,Values.Boolean b2 = ba2 in
    let lhs1,rhs1,strict1 = take_sides e1 b1 in
    let lhs2,rhs2,strict2 = take_sides e2 b2 in
    let data1 = proj(Terms.data e1) in
    let data2 = proj(Terms.data e2) in
    let coeff1 = Q.(abs(data1.TS.scaling * TS.VarMap.find var data1.TS.coeffs)) in
    let coeff2 = Q.(abs(data2.TS.scaling * TS.VarMap.find var data2.TS.coeffs)) in
    let lhs1,rhs1 = times coeff2 lhs1, times coeff2 rhs1 in
    let lhs2,rhs2 = times coeff1 lhs2, times coeff1 rhs2 in
    let lhs,rhs = plus lhs1 lhs2, plus rhs1 rhs2 in
    let symb = if (strict1 || strict2) then Symbols.Lt else Symbols.Le in
    let sum = Term.bC symb [lhs; rhs] in
    let justif = Assign.add(SAssign ba2)(Assign.singleton(SAssign ba1)) in
    straight () justif (sum,Values.Boolean true)

                           
  let pp_tosat fmt (Sassigns.SAssign(c,v)) =
    Sassigns.pp_sassign Term.pp Qhashed.pp fmt (Sassigns.SAssign(Simpl.term c,v))

  let sat model state =
    let rec aux = function
      | [],[] ->
         { state with tosatisfy=[]; toevaluate=[] },
         Some(sat () state.seen ~sharing:state.sharing ~myvars:state.myvars)

      | [],(c::tail as toevaluate) ->
         let c = Simpl.simplify model c in
         begin match eval c with
         | Beval(Propa(_,Straight bassign))
              when Assign.mem (SAssign bassign) state.seen
                   && Assign.subset (Simpl.justif c) state.seen
           -> aux ([],tail)
         | Qeval(t,q)
              when Assign.mem (SAssign(t,Values.NonBoolean(vinj q))) state.seen
                   && Assign.subset (Simpl.justif c) state.seen
           -> aux ([],tail)
         | _ ->
            Print.print ["kernel.LRA",2] (fun p ->
                p "kernel.LRA: not sat, still waiting to evaluate %a"
                  (List.pp Simpl.pp) toevaluate);
            { state with tosatisfy=[]; toevaluate=c::tail }, None
         end
           
      | (Sassigns.SAssign(c,v)::tail as tosatisfy), toevaluate ->
         let c = Simpl.simplify model c in
         match eval c, v with
         | Beval(Propa(_,Straight(_,Values.Boolean b))), Values.Boolean b'
              when [%eq : bool] b b' && Assign.subset (Simpl.justif c) state.seen
           -> aux (tail, toevaluate)
         | Qeval(_,q), Values.NonBoolean q'
              when Q.equal q q' && Assign.subset (Simpl.justif c) state.seen
           -> aux (tail, toevaluate)
         | _ ->
            Print.print ["kernel.LRA",2] (fun p ->
                p "kernel.LRA: not sat, still waiting to satisfy %a"
                  (List.pp pp_tosat) tosatisfy);
            { state with tosatisfy = Sassigns.SAssign(c,v)::tail; toevaluate }, None
    in
    aux (state.tosatisfy, state.toevaluate)
        

  let add sassign state =
    Print.print ["kernel.bool",2] (fun p ->
        p "kernel.bool receiving %a" pp_sassign sassign);
    let seen = Assign.add sassign state.seen in
    let SAssign(term,v) = sassign in
    let myvars = lazy(add_myvars term (Lazy.force state.myvars)) in
    match v with
    | Values.Boolean b ->
       begin match (proj(Terms.data term)).TS.nature with
       | TS.Other -> { state with seen; myvars }, None
       | _ ->
          let c = Simpl.make term in
          let sassign = Sassigns.SAssign(c,Values.Boolean b) in
          { state with seen; myvars; tosatisfy = sassign::state.tosatisfy }, Some sassign
       end
    | Values.NonBoolean v ->
       match vproj v with
       | None   -> { state with seen; myvars }, None
       | Some q ->
          let c = Simpl.make term in
          let sassign = Sassigns.SAssign(c,Values.NonBoolean q) in
          { state with seen; myvars; tosatisfy = sassign::state.tosatisfy }, Some sassign

  let share tset state =
    let sharing = TSet.union tset state.sharing in
    let myvars = lazy(TSet.fold add_myvars tset (Lazy.force state.myvars)) in
    let aux term toevaluate =
      match (proj(Terms.data term)).TS.nature with
      | TS.Other -> toevaluate
      | _ -> (Simpl.make term)::toevaluate
    in
    let new2evaluate = TSet.fold aux tset [] in
    let toevaluate = List.rev_append new2evaluate state.toevaluate in
    new2evaluate, { state with sharing; myvars; toevaluate }

  let clear = VarMap.clear
                   
end

open API
       
type ('t,'v,'a,'s) api = (module API with type sign   = sign
                                      and type assign = 'a
                                      and type termdata = 't
                                      and type value  = 'v
                                      and type tset   = 's)

let make (type t v a s)
      ((module DS): (ts,values,t,v,a,s) dsProj)
    : (t,v,a,s) api =
  (module Make(DS))
