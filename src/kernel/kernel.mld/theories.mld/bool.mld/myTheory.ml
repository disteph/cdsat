open General
open Sums       

open Top
open Messages
open Terms
open Sassigns

open Termstructures

open Theory
    
open API
       
type sign = unit

module T = struct
  let dskey = Clauses.key
  let ds  = [DSK dskey]
  type nonrec sign = sign
  type api = (module API with type sign = sign)
  let name = "Bool"

  module Make(W : Writable) : API with type sign = sign = struct

    include Basis
    type nonrec sign = sign

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

    let add_myvars = proj >> Clauses.freevar >> TSet.fold TSet.add 

    type interesting =
      | Falsified of (sign,unsat) message
      | Unit      of (sign,straight) message
      | Satisfied
      | ToWatch   of Clauses.VarMap.t * BAssign.t list

    let infer c =
      let (t,Values.Boolean b) as bassign = Constraint.bassign c in
      match Constraint.simpl c with
      | Some(_,[])  ->
        Print.print ["kernel.bool",4] (fun p ->
            p "kernel.bool: %a is falsified" Constraint.pp c);
        let justif = Assign.add (SAssign.build bassign) (Constraint.justif c) in
        Falsified(unsat () justif)
      | Some(_,[(term,Values.Boolean b')]) ->
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
           let justif = Assign.add (SAssign.build bassign) (Constraint.justif c) in
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
          let c = Constraint.simplify c model in
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

    let fromeq state sassign b a1 a2 =
      let f =
        if b then (fun a -> a)
        else (fun a -> W.bC Symbols.Neg [a])
      in
      let derived1 = W.bC Symbols.Imp [a1;f a2], Values.Boolean true in
      let derived2 = W.bC Symbols.Imp [a2;f a1], Values.Boolean true in
      let msg1 = straight () (Assign.singleton sassign) derived1 in
      let msg2 = straight () (Assign.singleton sassign) derived2 in
      let propas = [msg1;msg2] in
      Case1 propas, state.todo
    (* { state with seen; myvars }, propas *)

    let add sassign state =
      Print.print ["kernel.bool",2] (fun p ->
          p "kernel.bool receiving %a" SAssign.pp sassign);
      let seen = Assign.add sassign state.seen in
      let SAssign((term,v) as bassign) = SAssign.reveal sassign in
      let myvars = lazy(add_myvars term (Lazy.force state.myvars)) in
      let finalise (propas,todo) = { state with seen; todo; myvars }, propas in
      match v with
      | Values.NonBoolean _ -> { state with seen; myvars }, Case1 []
      | Values.Boolean b ->
        match Term.reveal term with
        | Terms.C(Symbols.Eq Sorts.Prop, [a1;a2]) ->
          finalise(fromeq state sassign b a1 a2)
        | Terms.C(Symbols.NEq Sorts.Prop, [a1;a2])
        | Terms.C(Symbols.Xor, [a1;a2])->
          finalise(fromeq state sassign (not b) a1 a2)
        | _ ->
          let result =
            match cube bassign with
            | Some set when Clauses.VarMap.cardinal set > 1 ->
              let aux term b' (sofar,todo) =
                let derived = term,Values.Boolean b' in
                let msg     = straight () (Assign.singleton sassign) derived in
                Print.print ["kernel.bool",-1] (fun p ->
                    p "kernel.bool: conjunction %a" pp_message msg);
                let c       = Constraint.make derived in
                msg::sofar, c::todo
              in
              let propas, todo = Clauses.VarMap.fold aux set ([],state.todo) in
              Case1 propas, todo
            | _ ->
              let c = Constraint.make bassign in
              Case2 c, c::state.todo
          in
          finalise result

    let share tset state =
      let sharing = TSet.union tset state.sharing in
      let myvars = lazy(TSet.fold add_myvars tset (Lazy.force state.myvars)) in
      { state with sharing; myvars }

  end

  let make (module W : Writable) : api = (module Make(W))

end

let hdl = register(module T)
