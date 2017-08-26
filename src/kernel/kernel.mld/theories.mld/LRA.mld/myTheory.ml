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
    let aux var _ = var |> IntSort.reveal |> fun (i,_) -> TSet.add (Term.term_of_id i) in
    TS.(VarMap.fold aux (DS.proj(Terms.data term)).coeffs myvars)

  (* state type:
     in order to produce the message Sat(seen),
     one must satisfy each constraint in todo. *)
  type state = { seen      : Assign.t;
                 sharing   : TSet.t;
                 myvars    : TSet.t Lazy.t;
                 tosatisfyB: (Simpl.t * bool) list;
                 tosatisfyQ: (Simpl.t * Q.t) list;
                 toevaluate: Term.t list }

  let init = { seen      = Assign.empty;
               sharing   = TSet.empty;
               myvars    = lazy TSet.empty;
               tosatisfyB= [];
               tosatisfyQ= [];
               toevaluate= [] }

  type eval =
    | Beval   of (sign,straight) Msg.t
    | Qeval   of Term.t * Q.t
    | Unit    of TS.nature * bool * Q.t
    | ToWatch of IntSort.t list

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
    | [x] ->
       let coeff = Q.(Simpl.scaling c * TS.VarMap.find x (Simpl.coeffs c)) in
       let constant = Q.(Simpl.scaling c * Simpl.constant c) in
       Unit(Simpl.nature c, Q.sign coeff > 0, Q.(neg constant / coeff))
    | watchable -> ToWatch watchable

  exception WeirdModel

  let pp_tosatB fmt (simpl,b) = pp_bassign fmt (Simpl.term simpl,Values.Boolean b)
  let pp_tosatQ fmt (simpl,q) =
    pp_sassign fmt (SAssign(Simpl.term simpl,Values.NonBoolean(vinj q)))
               
  let sat model state =
    let rec aux = function
      | [],[],[] ->
         { state with tosatisfyB=[]; tosatisfyQ=[]; toevaluate=[] },
         Some(sat () state.seen ~sharing:state.sharing ~myvars:state.myvars)

      | [],[],(t::tail as toevaluate) ->
         let i = IntSort.build (Term.id t,Term.get_sort t) in
         if VarMap.mem i (Model.map model)
         then aux ([],[],tail)
         else { state with tosatisfyB=[]; tosatisfyQ=[]; toevaluate }, None

      | [],((t,q)::tail as tosatisfyQ), toevaluate ->
         let t = Simpl.simplify model t in
         begin match eval t with
         | Qeval(_,q') ->
            if Q.equal q q' && Assign.subset (Simpl.justif t) state.seen
            then aux ([],tail,toevaluate)
            else raise WeirdModel
         | _ ->
            Print.print ["kernel.LRA",2] (fun p ->
                p "kernel.LRA: not sat, still waiting to satisfy %a"
                  (List.pp pp_tosatQ) tosatisfyQ);
            { state with tosatisfyB = []; tosatisfyQ = (t,q)::tail; toevaluate }, None
         end
           
      | ((t,b)::tail as tosatisfyB), tosatisfyQ, toevaluate ->
         let t = Simpl.simplify model t in
         match eval t with
         | Beval(Propa(_,Straight(_,Values.Boolean b'))) ->
            if [%eq : bool] b b' && Assign.subset (Simpl.justif t) state.seen
            then aux (tail, tosatisfyQ, toevaluate)
            else raise WeirdModel
         | _ ->
            Print.print ["kernel.LRA",2] (fun p ->
                p "kernel.LRA: not sat, still waiting to satisfy %a"
                  (List.pp pp_tosatB) tosatisfyB);
            { state with tosatisfyB = (t,b)::tail; tosatisfyQ; toevaluate }, None
    in
    aux (state.tosatisfyB, state.tosatisfyQ, state.toevaluate)
        

  let add sassign state =
    Print.print ["kernel.bool",2] (fun p ->
        p "kernel.bool receiving %a" pp_sassign sassign);
    let seen = Assign.add sassign state.seen in
    let SAssign(term,v) = sassign in
    let myvars = lazy(add_myvars term (Lazy.force state.myvars)) in
    match v with
    | Values.Boolean b ->
       let term = Simpl.make term in
       { state with seen; myvars; tosatisfyB = (term,b)::state.tosatisfyB }, Some term
    | Values.NonBoolean v ->
       match vproj v with
       | Some v ->
          let term = Simpl.make term in
          { state with seen; myvars; tosatisfyQ = (term,v)::state.tosatisfyQ }, Some term
       | None -> { state with seen; myvars }, None

  let share tset state =
    let sharing = TSet.union tset state.sharing in
    let myvars = lazy(TSet.fold add_myvars tset (Lazy.force state.myvars)) in
    let aux term toevaluate =
      match Term.get_sort term with
      | Sorts.Rat -> term::toevaluate
      | _ -> toevaluate
    in
    { state with sharing; myvars; toevaluate = TSet.fold aux tset state.toevaluate }

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
