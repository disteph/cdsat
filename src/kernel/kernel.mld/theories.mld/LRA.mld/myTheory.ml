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

  module P = struct

    (* Creates a term for coeff*term, special case if coeff is 1 *)
    let ( * ) coeff term =
      if Q.equal coeff Q.one then term
      else Term.bC Symbols.Times [Term.bC (Symbols.CstRat coeff) []; term]

    (* Creates a term for a+b, special cases when a or b is 0 *)
    let ( + ) a b =
      match Terms.reveal a, Terms.reveal b with
      | Terms.C(Symbols.CstRat q,[]),_ when Q.equal q Q.zero -> b
      | _,Terms.C(Symbols.CstRat q,[]) when Q.equal q Q.zero -> a
      | _ -> Term.bC Symbols.Plus [a; b]

    (* Creates a term for a-b *)
    let ( - ) a b = a + (Q.minus_one * b)
  end 

  (* Normalises boolean assignment into a<b, a<=b, a=b, or a<>b *)
  let normal (term,Values.Boolean b) =
    let open Symbols in
    let open Terms in
    match reveal term with
    | C(Lt,[a;a']) when not b -> Term.bC Le [a'; a]
    | C(Le,[a;a']) when not b -> Term.bC Lt [a'; a]
    | C(Gt,[a;a']) -> if b then Term.bC Lt [a'; a] else Term.bC Le [a; a']
    | C(Ge,[a;a']) -> if b then Term.bC Le [a'; a] else Term.bC Lt [a; a']
    | C(Eq Sorts.Rat,[a;a']) when not b -> Term.bC (NEq Sorts.Rat) [a; a']
    | C(NEq Sorts.Rat,[a;a']) when not b -> Term.bC (Eq Sorts.Rat) [a; a']
    | _ -> term

  let get_coeff t var =
    let data = proj(Terms.data t) in
    Q.(data.TS.scaling * TS.VarMap.find var data.TS.coeffs)

  (* which indicates whether we want an upper (true) or lower (false) bound for var *)
  let get_bound term var which = 
    let coeff = get_coeff term var in
    let open Symbols in
    let open Terms in
    match reveal term with
    | C(Lt as symb,[a; b]) | C(Le as symb,[a; b]) | C(Eq Sorts.Rat as symb,[a; b])
         when [%eq:bool] which (Q.sign coeff>0) -> a,b,symb,coeff
    | C(Eq Sorts.Rat,[a; b])
      -> P.(Q.minus_one * a), P.(Q.minus_one * b), Eq Sorts.Rat, Q.(minus_one*coeff)
    | _ -> failwith "Is not the right kind of bound"
      
  (* Assumes the only connectives used ar Lt, Le, and Neq Rat*)
  let fm_connective =
    let open Symbols in
    function
    | Lt,_ | _, Lt -> Lt
    | Le,_ | _, Le -> Le
    | Eq Sorts.Rat,Eq Sorts.Rat -> Eq Sorts.Rat
    | _ -> failwith "should not call fm_connective with other connectives than Lt Le Eq"
     
  (* Fourier-Motzkin resolution of ba1 and ba2 over variable var
     Creates message ba1,ba2 ⊢ FM_resolvant(ba1,ba2) *)
  let fm ba1 ba2 var =
    let e1, e2 = normal ba1, normal ba2 in
    Print.print ["kernel.LRA",2] (fun p ->
        p "kernel.LRA: lower bound is %a, upper bound is %a"
          Term.pp e1 Term.pp e2);
    let lhs1,rhs1,symb1,coeff1 = get_bound e1 var false in
    let lhs2,rhs2,symb2,coeff2 = get_bound e2 var true in
    let coeff1,coeff2 = Q.abs coeff1, Q.abs coeff2 in
    let open P in
    let lhs1,rhs1 = coeff2 * lhs1, coeff2 * rhs1 in
    let lhs2,rhs2 = coeff1 * lhs2, coeff1 * rhs2 in
    let lhs,rhs   = lhs1 + lhs2,   rhs1 + rhs2 in
    let sum = Term.bC (fm_connective(symb1,symb2)) [lhs; rhs] in
    let justif = Assign.add(SAssign ba2)(Assign.singleton(SAssign ba1)) in
    straight () justif (sum,Values.Boolean true)

  let disequal lower diseq upper var =
    let e1,e2,e3 = normal lower, normal diseq, normal upper in
    let l1,r1,symb1,lower_coeff = get_bound e1 var false in
    let l3,r3,symb3,upper_coeff = get_bound e3 var true in
    let open P in
    let l2,r2,diseq_coeff =
      let coeff = get_coeff e2 var in
      let open Terms in
      match reveal e2 with
      | C(Symbols.NEq Sorts.Rat,[a; b]) ->
         if Q.sign coeff>0 then a,b,coeff
         else Q.minus_one * a, Q.minus_one * b, Q.(minus_one*coeff)
      | _ -> failwith "Not a disequality"
    in
    assert ((Q.sign lower_coeff<0)&&(Q.sign upper_coeff>0)&&(Q.sign diseq_coeff>0));
    let var = Term.term_of_id var in
    let lower_bound = (* lower_bound ≤ |lower_coeff| var *)
      (l1 - r1) - (lower_coeff * var)
    in
    let upper_bound = (* |upper_coeff| var ≤ upper_bound *)
      (r3 - l3) + (upper_coeff * var)
    in
    let diseq_bound = (* diseq_bound ≠ |diseq_coeff| var *)
      (r2 - l2) + (diseq_coeff * var)
    in
    let assumption1 = Term.bC (Symbols.Eq Sorts.Rat)
                        [(Q.abs diseq_coeff) * lower_bound;
                         (Q.abs lower_coeff) * diseq_bound]
    in
    let assumption2 = Term.bC (Symbols.Eq Sorts.Rat)
                        [(Q.abs diseq_coeff) * upper_bound;
                         (Q.abs upper_coeff) * diseq_bound]
    in
    let justif = Assign.add (SAssign lower)
                   (Assign.add (SAssign diseq)
                      (Assign.add (SAssign upper)
                         (Assign.add (boolassign assumption1)
                            (Assign.singleton (boolassign assumption2)))))
    in
    assumption1, assumption2, unsat () justif

             
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
