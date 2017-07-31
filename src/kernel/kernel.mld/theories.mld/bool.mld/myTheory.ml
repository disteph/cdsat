open General
open Patricia
open Patricia_interfaces
open Patricia_tools
open Sums       

open Top
open Messages
open Specs
open Termstructures.Literals
open Termstructures.Clauses

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
  type nonrec assign = Assign.t
  type nonrec bassign = bassign
  type nonrec sassign = sassign

  include Basis.Make(DS)

  (* state type:
     in order to produce the message Sat(seen),
     one must satisfy each constraint in todo. *)
  type state = { seen : Assign.t;
                 todo : Assign.t; }

  (* Initial state. Note: can produce Sat(init). *)
  let init = { seen = Assign.empty;
               todo = Assign.empty; }

  type interesting =
    | Falsified of (sign,unsat) Msg.t
    | Unit of (sign,straight) Msg.t
    | Satisfied of (state -> state)

  exception Nothing2say
              
  let prune justif bassign state =
    let sassign = Values.boolassign bassign in
    if Assign.mem sassign state.todo && Assign.subset justif state.seen
    then { state with todo = Assign.remove sassign state.todo }
    else state

  let infer c =
    let (t,b) as bassign = Constraint.bassign c in
    match Constraint.simpl c with
    | Some(_,[])  ->
       Print.print ["kernel.bool",4] (fun p ->
           p "kernel.bool: %a is falsified" Constraint.pp c);
       let justif = Assign.add (Values.boolassign bassign) (Constraint.justif c) in
       Falsified(unsat () justif)
    | Some(_,[l]) ->
       let b',id = LitF.reveal l in
       let term = Term.term_of_id id in
       if Term.equal t term
       then
         (Print.print ["kernel.bool",4] (fun p ->
              p "kernel.bool: Detected UP, but %a is satisfied by %a"
                Constraint.pp c Assign.pp (Constraint.justif c));
          Satisfied(prune (Constraint.justif c) bassign))
       else
         (Print.print ["kernel.bool",4] (fun p ->
              p "kernel.bool: Detected UP for constraint %a (justif %a)"
                Constraint.pp c Assign.pp (Constraint.justif c));
         let justif = Assign.add (Values.boolassign bassign) (Constraint.justif c) in
         Unit(straight () justif (term,not([%eq:bool] b b'))))
    | Some _ -> raise Nothing2say
    | None   ->
       Print.print ["kernel.bool",4] (fun p ->
           p "kernel.bool: Detected true lit, so %a is satisfied by %a"
             Constraint.pp c Assign.pp (Constraint.justif c));
       Satisfied(prune (Constraint.justif c) bassign)
           
  let sat state =
    if Assign.is_empty state.todo
    then Some(sat () state.seen)
    else
      (Print.print ["kernel.bool",2] (fun p ->
           p "kernel.bool: not sat, still waiting to satisfy %a" Assign.pp state.todo);
       None)
               
  let add sassign state =
    Print.print ["kernel.bool",2] (fun p ->
        p "kernel.bool receiving %a" pp_sassign sassign);
    let seen = Assign.add sassign state.seen in
    let term,v = sassign in
    match v with
    | Values.NonBoolean _ -> Some [], { state with seen = seen }
    | Values.Boolean b ->
       let propas,todo =
         match cube (term,b) with
         | Some set when LSet.cardinal set > 1 ->
            let aux lit (sofar,todo) =
              let b',id = LitF.reveal lit in
              let derived = Term.term_of_id id,([%eq:bool] b b') in
              let msg     = straight () (Assign.singleton sassign) derived in
              msg::sofar,
              Assign.add (Values.boolassign derived) todo
            in
            let propas, todo = LSet.fold aux set ([],state.todo) in
            Some propas, todo
         | _ -> None, Assign.add sassign state.todo
       in
       propas, { seen = seen; todo = todo }
                            
end

type ('t,'v,'a) api = (module API with type sign = sign
                                   and type assign  = 'a
                                   and type bassign = 't termF * bool
                                   and type sassign = 't termF * 'v Values.t)

let make (type t v a)
      ((module DS): (ts,values,t,v,a) dsProj)
    : (t,v,a) api =
  (module Make(DS))
