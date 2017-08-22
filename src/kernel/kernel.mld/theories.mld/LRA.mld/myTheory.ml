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
  type datatypes = Term.datatype*Value.t*Assign.t*TSet.t

  include Basis.Make(DS)
                                                    
  let add_myvars term myvars =
    let aux var _ = var |> IntSort.reveal |> fun (i,_) -> TSet.add (Term.term_of_id i) in
    TS.(VarMap.fold aux (DS.proj(Terms.data term)).coeffs myvars)

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

  type interesting =
    | Falsified of (sign,unsat) Msg.t
    | Unit      of TS.nature * bool * Q.t
    | Satisfied
    | ToWatch   of IntSort.t list

  let infer c =
    let bassign = Constraint.bassign c in
    match Constraint.watchable c with
    | [] -> let sign = Q.(sign (Constraint.constant c * Constraint.scaling c)) in
            let open TS in
            begin match Constraint.nature c with
              (* Print.print ["kernel.bool",4] (fun p -> *)
              (*     p "kernel.bool: Detected true lit, so %a is satisfied by %a" *)
              (*       Constraint.pp c Assign.pp (Constraint.justif c)); *)
            | Lt when sign<0   -> Satisfied
            | Le when sign<=0  -> Satisfied
            | Eq when sign=0   -> Satisfied
            | NEq when sign<>0 -> Satisfied
            | _ ->
               Print.print ["kernel.bool",4] (fun p ->
                   p "kernel.bool: %a is falsified" Constraint.pp c);
               let justif = Assign.add (SAssign bassign) (Constraint.justif c) in
               Falsified(unsat () justif)
            end
    | [x] ->
       Print.print ["kernel.bool",4] (fun p ->
           p "kernel.bool: Detected unit constraint %a (justif %a)"
             Constraint.pp c Assign.pp (Constraint.justif c));
       let coeff = Q.(Constraint.scaling c * TS.VarMap.find x (Constraint.coeffs c)) in
       Unit(Constraint.nature c,
            Q.sign coeff > 0,
            Q.((neg(Constraint.scaling c * Constraint.constant c)) / coeff))
    | watchable -> ToWatch watchable

  exception WeirdModel

  let sat model state =
    let rec aux = function
      | [] -> { state with todo=[] },
              Some(sat () state.seen ~sharing:state.sharing ~myvars:state.myvars)
      | (c::rest) as todo ->
         let c = Constraint.simplify model c in
         match infer c with
         | Satisfied ->
            if Assign.subset (Constraint.justif c) state.seen
            then aux rest
            else raise WeirdModel
         | _ ->
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
    | Values.Boolean b -> failwith "TODO"
       (* let propas,todo = *)
       (*   match cube bassign with *)
       (*   | Some set when LSet.cardinal set > 1 -> *)
       (*      let aux lit (sofar,todo) = *)
       (*        let b',id = LitF.reveal lit in *)
       (*        let derived = Term.term_of_id id,Values.Boolean([%eq:bool] b b') in *)
       (*        let msg     = straight () (Assign.singleton sassign) derived in *)
       (*        let c       = Constraint.make derived in *)
       (*        msg::sofar, c::todo *)
       (*      in *)
       (*      let propas, todo = LSet.fold aux set ([],state.todo) in *)
       (*      Case1 propas, todo *)
       (*   | _ -> *)
       (*      let c = Constraint.make bassign in *)
       (*      Case2 c, c::state.todo *)
       (* in *)
       (* { state with seen; todo; myvars }, propas *)

  let share tset state =
    let sharing = TSet.union tset state.sharing in
    let myvars = lazy(TSet.fold add_myvars tset (Lazy.force state.myvars)) in
    { state with sharing; myvars }


                 
  let rec machine state =
    let add = function
      | None -> Silence, machine state
      | Some sassign -> let state, _ = add sassign state in
                        Silence, machine state
    in
    let share tset = Silence, machine(share tset state) in
    let clone () = machine state in
    let suicide _ = () in
    Specs.SlotMachine { add; share; clone; suicide }

  let init: (sign, datatypes) Top.Specs.slot_machine
    = machine { seen=Assign.empty;
                sharing=TSet.empty;
                myvars=lazy TSet.empty;
                todo = [] }
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
