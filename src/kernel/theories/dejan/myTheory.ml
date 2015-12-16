open Top
open Messages
open Specs

open Algo
open Trail

type sign = unit

module Make(DS: sig
    include GTheoryDSType
    val proj: Term.datatype -> ThDS.t
  end) = struct

  open DS

  (* Function to convert types *)
  let rec aToEq a (splits, eqs) = 
    match proj(Terms.data a) with
    | ThDS.Ineq eq    -> splits, (eq::eqs)
    | ThDS.EqNeq(flip,i) ->
       begin
         match flip, Terms.reveal(Term.term_of_id i) with

         | true, Terms.C(Symbols.Eq Sorts.Rat,[a;b])
         | false, Terms.C(Symbols.NEq Sorts.Rat,[a;b])
             -> 
            let a1 = Term.bC Symbols.Le [a;b] in
            let a2 = Term.bC Symbols.Ge [a;b] in
            let splits, eqs = aToEq a1 (splits, eqs) in
            aToEq a2 (splits, eqs)

         | true, Terms.C(Symbols.NEq Sorts.Rat,[a;b])
         | false, Terms.C(Symbols.Eq Sorts.Rat,[a;b])
             ->
            let a1 = Term.bC Symbols.Lt [a;b] in
            let a2 = Term.bC Symbols.Gt [a;b] in
            let s1 = TSet.add a1 TSet.empty in
            let s2 = TSet.add a2 TSet.empty in
            let s3 = TSet.add a TSet.empty in
            (thAnd () s1 s2 s3)::splits,
            eqs
         | _ -> splits, eqs
       end
    | _ -> splits, eqs

  let eqToA eq = match Equation.getTag eq with
    | None -> failwith "Can not convert an equation without tag"
    | Some t -> Term.term_of_id t



  type state = {treated : TSet.t;
                splits : (sign,TSet.t,thAnd) thsays list;
                stack : trail list}

  let rec machine state  =
    (module struct

      type newoutput = (sign,TSet.t) output
      type tset = TSet.t

      let fromTSet tset = TSet.fold aToEq tset (state.splits,[])
      let toTSet eqs = List.fold_left (fun l e -> TSet.add (eqToA e) l) TSet.empty eqs

      (* Requiered function *)
      let treated () = state.treated

      (* Should send a minimal set af equations *)
      let add = function
        | None -> Output(None, machine state)
        | Some tset ->
          let newtreated = TSet.union state.treated tset in
          let splits, neweqs = fromTSet tset in
          try     
            (* Lancer l'algo sur les nouvelles équations *)
            let _,s = resumeDejeanAlgo neweqs state.stack in
            begin
              match splits with
              | [] -> 
                 let newState = {
                   treated = newtreated; 
                   splits = [];
                   stack = s;
                 }
                 in
                 Output(Some(thNotProvable () newtreated), machine newState)
              | msg::splits' ->
                 let newState = {
                   treated = newtreated; 
                   splits = splits';
                   stack = s;
                 }
                 in
                 Output(Some msg, machine newState)
            end

          with Unsat_failure (l,s) -> Output(Some(thProvable () (toTSet l)), fail_state)

      let normalise _ = failwith "Not a theory with normaliser"

      let clone () = Output(None, machine state)

    end : SlotMachine with type newoutput = (sign,TSet.t) output and type tset = TSet.t)

  let init = machine {treated=TSet.empty; splits=[]; stack=[]}

end
