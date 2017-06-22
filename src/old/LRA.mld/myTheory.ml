open Top
open Messages
open Specs

open Termstructures.Rationals
open Algo
open Dtrail

type sign = unit

type ts = TS.t
let ts  = Termstructures.Register.Rationals

include Theory.HasValues(General.Pnum)

module Make(DS: DSproj with type ts = TS.t) = struct

  type assign = DS.Assign.t
  open DS

  (* Converts a term to something understandable by the algorithm: eqs
     is where we accumulate the inequalities, splits is where we accumulate
     the case analyses that we will ask Psyche to make *)

  let rec aToEq lit (splits, eqs) = 
    match proj(Terms.data lit) with
    | TS.Ineq eq    -> splits, (eq::eqs)
    | TS.EqNeq(flip,i) ->
       begin
         match flip, Terms.reveal(Term.term_of_id i) with

         | true, Terms.C(Symbols.Eq Sorts.Rat,[a;b])
         | false, Terms.C(Symbols.NEq Sorts.Rat,[a;b])
             -> 
            let a1 = Term.bC Symbols.Le [a;b] in
            let a2 = Term.bC Symbols.Ge [a;b] in
            (* In case of an equality, we add the two non-strict
            inequalities to eqs *)
            let splits, eqs = aToEq a1 (splits, eqs) in
            aToEq a2 (splits, eqs)

         | true, Terms.C(Symbols.NEq Sorts.Rat,[a;b])
         | false, Terms.C(Symbols.Eq Sorts.Rat,[a;b])
             ->
            let a1 = Term.bC Symbols.Lt [a;b] in
            let a2 = Term.bC Symbols.Gt [a;b] in
            let s1 = Assign.singleton a1 in
            let s2 = Assign.singleton a2 in
            let s0 = Assign.singleton lit in
            (* In case of a disequality, we describe the case analysis
            to make, and store it in splits *)
            (both () s0 s1 s2)::splits,
            eqs
         | _ -> failwith "Should not happen"
       end
    | _ -> splits, eqs

(* FIX : when answering with newly created equation, will return previous ones that already exists.
   Should be improved by creating new terms *)
  let rec eqToA eq = match Equation.getTag eq with
        | [] -> failwith "Can not convert an equation without tag"
        | l  -> List.fold (fun v -> Assign.add (Term.term_of_id v)) l Assign.empty

  let toAssign eqs = List.fold (fun e -> Assign.union (eqToA e)) eqs Assign.empty


  type state = {treated : Assign.t;
                splits : (sign,Assign.t,both) message list;
                stack : trail list}

  let rec machine state  =
    (module struct

      type newoutput = (sign,Assign.t) output
      type tset = Assign.t

      (* Should send a minimal set af equations *)
      let add = function
        | None -> Output(None, machine state)
        | Some tset ->
          (* We collect from tset the equations and the split points *)
          let splits, neweqs = Assign.fold aToEq tset (state.splits,[]) in
           (* Dump.print ["dejan",1] (fun p -> p "Entering Dejan's add"); *)
          try     
            (* We resume the algorithm with the new equations *)
            let _,s = resumeDejeanAlgo neweqs state.stack in
            begin
              (* If we reach this, we are SAT, having ignored the
              splits, so we see if we want Psyche to perform some splits *)
              match splits with
              | [] -> 
                 (* No more case analyses to make, we return the SAT message *)
                 let newState = {
                   treated = Assign.union state.treated tset; 
                   splits = [];
                   stack = s;
                 }
                 in
                 (* Dump.print ["dejan",1] (fun p -> p "Exiting Dejan's add"); *)
                 Output(Some(sat () newState.treated), machine newState)
              | msg::splits' ->
                 (* We ask Psyche to make a case analysis with msg *)
                 let newState = {
                   treated = Assign.union state.treated tset; 
                   splits = splits';
                   stack = s;
                 }
                 in
                 (* Dump.print ["dejan",1] (fun p -> p "Exiting Dejan's add"); *)
                 Output(Some msg, machine newState)
            end

          with Unsat_failure (l,s) ->
            (* Dump.print ["dejan",1] (fun p -> p "Exiting Dejan's add"); *)
            Output(Some(unsat () (toAssign l)), Tools.fail_state)

      let normalise _ = failwith "Not a theory with normaliser"

      let clone () = Output(None, machine state)

      let suicide _ = ()

    end : SlotMachine with type newoutput = (sign,Assign.t) output and type tset = Assign.t)

  let init = machine {treated=Assign.empty; splits=[]; stack=[]}
  let clear () = ()

end

module type API = sig
  type assign
  val init: (sign,assign) slot_machine
  val clear: unit -> unit
end

type ('t,'v,'a) api = (module API with type assign = 'a)

let make (type t)(type v)(type a)
      ((module DS): (ts,values,t,v,a) dsProj)
    : (t,v,a) api =
  (module Make(DS))
