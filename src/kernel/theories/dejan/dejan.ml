(******************************)
(* Internal implementation of dejean's algorithm *)
(******************************)

include Trail
include FourierMotzkin

exception Unsat_failure of Equation.equation list

(* stack is a list of Trail objects (ie states) *)
(* lav : last assigned variable *)
let rec dejeanAlgoRec stack =
  match stack with
  | [] -> failwith "Unknown"
  | (head::tail) ->
    (*print_string "Head state is : ";Equation.print_eqs (getEqs head);print_string "\n";*)
    (* recalculate the constraints each time... *)

    let cons = Trail.createConstraints head in
    (* here we have : either an empty list which means OK,
       either two contradicting equations *)
    match  Trail.checkConstraints cons with
    | Some(t1,t2) -> (
      (*print_string "Failure in constraints\n";*)
      (* damned, there is a failure, let's go back*)
      let previous = Equation.getPreviousEqs [t1; t2] in
      (*print_string "The following subsystem is contradictory : ";
      Equation.print_eqs [t1;t2];
      print_string "The previous equations will be resolved : ";
      Equation.print_eqs previous;
      Printf.printf "there are %i states if we want to go back\n" (List.length tail);*)

      (* WARNING : those previous equations must be in
         the preceding state in the stack (if they came
         from an assignment of a variable). Even if they came from
         an FM resolution, their state is the state before the last assignment *)

         match tail with
         | [] -> raise (Unsat_failure previous)
         (* we cannot go back : the system itself is contradictory *)
         | a::q ->
              (* none of this should raise an exception while functioning normally *)

              (* we should have a last assigned variable (otherwise we would be
              in the previous case, with an empty tail)*)
              (*print_string "All but this variable will be eliminated : ";*)
              (*print_string (Trail.getLastAssignedVariable head);*)
              (*print_string "\n";*)

              (* FM resolution should at least return an inequation *)
              let neweq = FourierMotzkin.fourierMotzkin
                    (Trail.getLastAssignedVariable head) previous in
              (*print_string "FM resolution output is the following : ";Equation.print neweq;print_string "\n";*)
              (* new equation found by resolution FM *)
              (* we go back and erase the current state, then add our new equation *)
              let newstack = (Trail.addEq a neweq)::q in
              (* back to constraint computing. There could be another failure*)
              dejeanAlgoRec newstack
      )
    |None ->
        match stack with
        | [] -> failwith "Stack was empty but should not"
        | (head::tail) ->
            match Trail.chooseUnassignedVariable head with
            (* the model is complete*)
            | None -> Trail.getCurrentModel head
            | Some(var) ->
              (* choose a value according to the constraints *)
              let v = Trail.chooseValue cons var in
              (*print_string "Assigning the value : ";print_string (string_of_num v);*)
              (*print_string " to : ";print_string var; print_string "\n";*)
              let newState = Trail.assignValue head var v in
              dejeanAlgoRec (newState::head::tail)


let dejeanAlgo l =
    let trail = Trail.create l in
    let stack = [trail] in
    dejeanAlgoRec stack
