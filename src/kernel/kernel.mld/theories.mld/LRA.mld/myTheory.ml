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
                                                    
  (* state type:
     in order to produce the message Sat(seen,sharing,myvars),
     one must satisfy each constraint in tosatisfy,
     and give a value to each term in toevaluate. *)
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

  (* Output type for the evaluation function below *)
  type eval =
    | Beval   of (sign,straight) Msg.t
    | Qeval   of Term.t * Q.t
    | Unit    of { var         : int;
                   nature      : TS.nature;
                   is_coeff_pos: bool;
                   bound       : Q.t }
    | ToWatch of int list

  exception IdontUnderstand

  (* Evaluates a simplified term c,
     special cases if all variables are assigned or ony one is missing *)
  let eval c =
    match Simpl.watchable c with
    | [] -> let value = Q.(Simpl.constant c * Simpl.scaling c) in
            let beval b =
              Beval(straight ()
                      (Simpl.justif c)
                      (Simpl.term c,Values.Boolean b))
            in
            Print.print ["kernel.LRA",2] (fun p ->
                p "kernel.LRA: evaluating term in %a gave value %a"
                  Simpl.pp c Q.pp_print value);
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
             bound        = Q.(neg constant / coeff) }
    | watchable -> ToWatch watchable

  (* Creates a term for coeff*term, special case if coeff is 1 *)
  let times coeff term =
    if Q.equal coeff Q.one then term
    else Term.bC Symbols.Times [Term.bC (Symbols.CstRat coeff) []; term]

  (* Creates a term for a+b, special cases when a or b is 0 *)
  let plus a b =
    match Terms.reveal a, Terms.reveal b with
    | Terms.C(Symbols.CstRat q,[]),_ when Q.equal q Q.zero -> b
    | _,Terms.C(Symbols.CstRat q,[]) when Q.equal q Q.zero -> a
    | _ -> Term.bC Symbols.Plus [a; b]

  let get_coeff t var =
    let data = proj(Terms.data t) in
    Q.(data.TS.scaling * TS.VarMap.find var data.TS.coeffs)

  (* Takes boolean assignment equivalent to a<b, a≤b, a>b, a≥b, a=b, a≠b.
     Outputs: the smaller side, the bigger side,
     Some(whether it is strict) or None if of the form a≠b. *)
  let take_sides term b =
    match Terms.reveal term with
    | Terms.C(Symbols.Lt,[lhs;rhs]) ->
       if b then lhs,rhs,Some true else lhs,rhs,Some false
    | Terms.C(Symbols.Le,[lhs;rhs]) ->
       if b then lhs,rhs,Some false else lhs,rhs,Some true
    | Terms.C(Symbols.Gt,[lhs;rhs]) ->
       if b then rhs,lhs,Some true else rhs,lhs,Some false
    | Terms.C(Symbols.Ge,[lhs;rhs]) ->
       if b then rhs,lhs,Some false else rhs,lhs,Some true
    | Terms.C(Symbols.Eq Sorts.Rat,[lhs;rhs]) when not b ->
       lhs,rhs,None
    | Terms.C(Symbols.NEq Sorts.Rat,[lhs;rhs]) when b ->
       lhs,rhs,None
    | _ -> failwith "Should not try inference with equality"

  (* projects equalities as ≤,
     which indicates whether we want an upper (true) or lower (false) bound for var *)
  let eq_as_le e b which var = 
    match Terms.reveal e,b with
    | Terms.C(Symbols.Eq Sorts.Rat,[a;b]),true
      | Terms.C(Symbols.NEq Sorts.Rat,[a;b]),false
      -> if [%eq:bool] which (Q.sign(get_coeff e var)>0)
         then Term.bC Symbols.Le [a;b],true
         else Term.bC Symbols.Ge [a;b],true
    | _ -> e,b
     
  (* Fourier-Motzkin resolution of ba1 and ba2 over variable var
     Creates message ba1,ba2 ⊢ FM_resolvant(ba1,ba2) *)
  let fm ba1 ba2 var =
    let e1,Values.Boolean b1 = ba1 in
    let e2,Values.Boolean b2 = ba2 in
    let e1,b1 = eq_as_le e1 b1 false var in
    let e2,b2 = eq_as_le e2 b2 true var in
    Print.print ["kernel.LRA",2] (fun p ->
        p "kernel.LRA: lower bound is (%a,%b), upper bound is (%a,%b)"
          Term.pp e1 b1 Term.pp e2 b2 
      );
    let lhs1,rhs1,strict1 = take_sides e1 b1 in
    let lhs2,rhs2,strict2 = take_sides e2 b2 in
    let strict = match strict1, strict2 with
      | Some strict1, Some strict2 -> strict1 || strict2
      | None,_ | _,None -> failwith "Should not try Fourier-Motzkin with a disequality"
    in
    let coeff1 = Q.abs(get_coeff e1 var) in
    let coeff2 = Q.abs(get_coeff e2 var) in
    let lhs1,rhs1 = times coeff2 lhs1, times coeff2 rhs1 in
    let lhs2,rhs2 = times coeff1 lhs2, times coeff1 rhs2 in
    let lhs,rhs = plus lhs1 lhs2, plus rhs1 rhs2 in
    let symb = if strict then Symbols.Lt else Symbols.Le in
    let sum = Term.bC symb [lhs; rhs] in
    let justif = Assign.add(SAssign ba2)(Assign.singleton(SAssign ba1)) in
    straight () justif (sum,Values.Boolean true)

  let disequal lower diseq upper var =
    let e1,Values.Boolean b1 = lower in
    let e2,Values.Boolean b2 = diseq in
    let e3,Values.Boolean b3 = upper in
    let e1,b1 = eq_as_le e1 b1 false var in
    let e3,b3 = eq_as_le e3 b3 true var in
    let l1,r1,strict1 = take_sides e1 b1 in
    let l2,r2,strict2 = take_sides e2 b2 in
    let l3,r3,strict3 = take_sides e3 b3 in
    let lower_coeff = get_coeff e1 var (* should be negative *) in
    let diseq_coeff = get_coeff e2 var (* should be positive *) in
    let upper_coeff = get_coeff e3 var in
    match strict1,strict2,strict3 with
    | Some false, None, Some false
         when (Q.sign lower_coeff<0)&&(Q.sign upper_coeff>0)&&(Q.sign diseq_coeff<>0)
      ->
       let lower_bound = (* lower_bound ≤ |lower_coeff| var *)
         plus l1 (times Q.minus_one (plus r1 (times lower_coeff (Term.term_of_id var))))
       in
       let upper_bound = (* |upper_coeff| var ≤ upper_bound *)
         plus r3 (plus (times Q.minus_one l3) (times upper_coeff (Term.term_of_id var)))
       in
       let diseq_bound = (* diseq_bound ≠ |diseq_coeff| var *)
         if Q.sign diseq_coeff<0
         then (* diseq_bound ≠ - diseq_coeff var *)
           plus l2 (times Q.minus_one (plus r2 (times diseq_coeff (Term.term_of_id var))))
         else (* diseq_coeff var ≠ diseq_bound *)
           plus r2 (plus (times Q.minus_one l2) (times diseq_coeff (Term.term_of_id var)))
       in
       let assumption1 = Term.bC (Symbols.Eq Sorts.Rat)
                            [times (Q.abs diseq_coeff) lower_bound;
                             times (Q.abs lower_coeff) diseq_bound]
       in
       let assumption2 = Term.bC (Symbols.Eq Sorts.Rat)
                            [times (Q.abs diseq_coeff) upper_bound;
                             times (Q.abs upper_coeff) diseq_bound]
       in
       let justif = Assign.add (SAssign lower)
                      (Assign.add (SAssign diseq)
                         (Assign.add (SAssign upper)
                            (Assign.add (boolassign assumption1)
                               (Assign.singleton (boolassign assumption2)))))
       in
       assumption1, assumption2, unsat () justif
             
    | _ ->
       Print.print ["kernel.LRA",2] (fun p ->
           p "kernel.LRA: signs of coeffs are %i, %i, %i"
             (Q.sign lower_coeff) (Q.sign upper_coeff) (Q.sign diseq_coeff));
       failwith "Should not try Disequal with this"

             
  (* let pp_tosat fmt (Sassigns.SAssign(c,v)) = *)
  (*   Sassigns.pp_sassign Term.pp Qhashed.pp fmt (Sassigns.SAssign(Simpl.term c,v)) *)
  let pp_tosat = Sassigns.pp_sassign Simpl.pp Qhashed.pp

  (* Scans the constraints to satisfy and the terms to evaluate
     and removes those that are satisfied/evaluated:
     if nothing is left, creates the Sat(seen,sharing,myvars) message *)
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
        
  let add_myvars term myvars =
    let aux var _ = TSet.add (Term.term_of_id var) in
    TS.(VarMap.fold aux (DS.proj(Terms.data term)).coeffs myvars)


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
    { state with sharing; myvars; toevaluate }, new2evaluate

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
