open General
open Patricia
open Patricia_interfaces
open Patricia_tools
open Sums       

open Top
open Basic
open Messages
open Specs
open Sassigns
open Termstructures.Literals
open Termstructures.Clauses
open Termstructures.VarSet.Generic
       
open API
       
type sign = unit

(* We are not using values *)
include Theory.HasNoValues

module Clauses = TS
          
(* We are not using alternative term representation *)
type ts = Clauses.t
let ts = Termstructures.Register.Clauses

module Make(DS: DSproj with type ts = ts) = struct

  open DS
  type nonrec sign = sign
  type nonrec termdata = Term.datatype
  type nonrec value = Value.t
  type nonrec assign = Assign.t
  type nonrec bassign = bassign
  type nonrec sassign = sassign
  type tset = TSet.t
                
  include Basis.Make(DS)

  (* state type:
     in order to produce the message Sat(seen),
     one must satisfy each constraint in todo. *)
  type state = { seen : Assign.t;
                 todo : Constraint.t list;
                 sharing : TSet.t;
                 myvars : TSet.t Lazy.t }

  (* Initial state. Note: can produce Sat(init). *)
  let init = { seen = Assign.empty;
               todo = [];
               sharing = TSet.empty;
               myvars  = lazy TSet.empty }

  let add_myvars term myvars =
    let aux var = var |> IntSort.reveal |> fun (i,_) -> TSet.add (Term.term_of_id i) in
    IntSortSet.fold aux (DS.proj(Terms.data term)).freevar myvars

  type interesting =
    | Falsified of (sign,unsat) Msg.t
    | Unit      of (sign,straight) Msg.t
    | Satisfied
    | ToWatch   of LSet.t * LitF.t list
              
  let infer c =
    let (t,Values.Boolean b) as bassign = Constraint.bassign c in
    match Constraint.simpl c with
    | Some(_,[])  ->
       Print.print ["kernel.bool",4] (fun p ->
           p "kernel.bool: %a is falsified" Constraint.pp c);
       let justif = Assign.add (SAssign bassign) (Constraint.justif c) in
       Falsified(unsat () justif)
    | Some(_,[l]) ->
       let b',id = LitF.reveal l in
       let term = Term.term_of_id id in
       if Term.equal t term
       then
         (Print.print ["kernel.bool",4] (fun p ->
              p "kernel.bool: Detected UP, but %a is satisfied by %a"
                Constraint.pp c Assign.pp (Constraint.justif c));
          Satisfied)
       else
         (Print.print ["kernel.bool",4] (fun p ->
              p "kernel.bool: Detected UP for constraint %a (justif %a)"
                Constraint.pp c Assign.pp (Constraint.justif c));
         let justif = Assign.add (SAssign bassign) (Constraint.justif c) in
         Unit(straight () justif (term,Values.Boolean(not([%eq:bool] b b')))))
    | Some(lset,watchable) -> ToWatch(lset,watchable)
    | None   ->
       Print.print ["kernel.bool",4] (fun p ->
           p "kernel.bool: Detected true lit, so %a is satisfied by %a"
             Constraint.pp c Assign.pp (Constraint.justif c));
       Satisfied

  exception WeirdModel

  let sat model state =
    let rec aux = function
      | [] -> { state with todo=[] },
              Some(sat () state.seen ~sharing:state.sharing ~myvars:state.myvars)
      | (c::rest) as todo ->
         let c = Constraint.simplify model c in
         match Constraint.simpl c with
         | None ->
            if Assign.subset (Constraint.justif c) state.seen
            then aux rest
            else raise WeirdModel
         | Some _ ->
            Print.print ["kernel.bool",2] (fun p ->
                p "kernel.bool: not sat, still waiting to satisfy %a"
                  (List.pp Constraint.pp) todo);
            { state with todo = c::rest }, None
    in
    aux state.todo
        
  let add sassign state =
    Print.print ["kernel.bool",2] (fun p ->
        p "kernel.bool receiving %a" pp_sassign sassign);
    let seen = Assign.add sassign state.seen in
    let SAssign((term,v) as bassign) = sassign in
    let myvars = lazy(add_myvars term (Lazy.force state.myvars)) in
    match v with
    | Values.NonBoolean _ -> { state with seen; myvars }, Case1 []
    | Values.Boolean _ ->
       let propas,todo =
         match cube bassign with
         | Some set when LSet.cardinal set > 1 ->
            let aux lit (sofar,todo) =
              let b',id = LitF.reveal lit in
              let derived = Term.term_of_id id,Values.Boolean b' in
              let msg     = straight () (Assign.singleton sassign) derived in
              Print.print ["kernel.bool",-1] (fun p ->
                  p "kernel.bool: conjunction %a" Msg.pp msg);
              let c       = Constraint.make derived in
              msg::sofar, c::todo
            in
            let propas, todo = LSet.fold aux set ([],state.todo) in
            Case1 propas, todo
         | _ ->
            let c = Constraint.make bassign in
            Case2 c, c::state.todo
       in
       { state with seen; todo; myvars }, propas

  let share tset state =
    let sharing = TSet.union tset state.sharing in
    let myvars = lazy(TSet.fold add_myvars tset (Lazy.force state.myvars)) in
    { state with sharing; myvars }
    
end

type ('t,'v,'a,'s) api = (module API with type sign   = sign
                                      and type assign = 'a
                                      and type termdata = 't
                                      and type value  = 'v
                                      and type tset   = 's)

let make (type t v a s)
      ((module DS): (ts,values,t,v,a,s) dsProj)
    : (t,v,a,s) api =
  (module Make(DS))
