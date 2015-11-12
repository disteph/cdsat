(******************************)
(* Internal implementation of dejean's algorithm *)
(******************************)

include Trail
include FourierMotzkin

exception Unsat_failure of Equation.equation list


let rec dejeanAlgoRec stack =
  match stack with
  | [] -> failwith "Unknown"
  | head::tail ->
    (*print_string "Head state is : ";Equation.print_eqs (getEqs head);print_string "\n";*)
    (* recalculate the constraints each time... *)
    let cons = Trail.createConstraints head in

    (* here we have : either None which means there is no contradiction up to now,
       either two contradicting equations *)
    match  Trail.checkConstraints cons with
    | Some(t1,t2) ->
      (*print_string "Failure in constraints\n";*)
      (* damned, there is a failure, let's go back*)
      (* learn a new clause throug FourierMotzkin resolution and backtrack*)
      let previous = Equation.getPreviousEqs [t1; t2] in

      (* print_string "The following subsystem is contradictory : "; *)
      (* Equation.print_eqs [t1;t2]; *)

      fmResolve stack previous

    (* affect one more variable *)
    |None -> variableChoosing stack cons

(* case when we resolve *)
and fmResolve stack eqs =
  match stack with
  | [] -> failwith "stack was empty but should not"
  | [t] ->  (* no going back possible *)
     raise (Unsat_failure eqs)
  | a::b::q ->
    (*  print_string "The previous equations will be resolved : "; *)
    (* Equation.print_eqs eqs; *)
    (* Printf.printf "there are %i states if we want to go back\n" (List.length (b::q)); *)
    (* print_string "All but this variable will be eliminated : "; *)
    (* print_string (Trail.getLastAssignedVariable a); *)
    (* print_string "\n"; *)
    (* FM resolution should at least return an inequation *)
    let neweq =
      try 
        Some(FourierMotzkin.fourierMotzkin (Trail.getLastAssignedVariable a) eqs)
      with FM_Failure -> None
    in
    match neweq with
    | Some neweq ->
      (*  print_string "FM resolution output is the following : "; *)
      (* Equation.print neweq;print_string "\n"; *)
      (* new equation found by resolution FM  *)
      (* we go back and erase the current state, then add our new equation *)
      let newstack = (Trail.addEq b neweq)::q in
      (* back to constraint computing. There could be another failure*)
      dejeanAlgoRec newstack
    | None ->
       (* FM Failure : impossible to eliminate all but one variable. This means
          the inequations are not linearly independent ; moreover, since they
          produced a contradiction, we are sure to obtain something like 0 < 0*)
       (
         (* print_string "FM resolution failed. Going back again."; *)
         fmResolve (b::q) (Equation.getPreviousEqs eqs)
       )

(* case when we choose a variable *)
and variableChoosing stack cons =
  match stack with
  | [] -> failwith "Stack was empty but should not"
  | head::tail ->
     match Trail.chooseUnassignedVariable head with
      (* the model is complete*)
     | None -> Trail.getCurrentModel head
     | Some var ->

        (* choose a value according to the constraints *)
        let v = Trail.chooseValue cons var in

        (* print_string "Assigning the value : ";print_string (string_of_num v); *)
        (* print_string " to : ";print_string var; print_string "\n"; *)

        let newState = Trail.assignValue head var v in
        dejeanAlgoRec (newState::head::tail)



let dejeanAlgo l =
    let trail = Trail.create l in
    let stack = [trail] in
    dejeanAlgoRec stack
