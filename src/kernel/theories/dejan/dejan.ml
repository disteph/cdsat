(******************************)
(* Internal implementation of dejean's algorithm *)
(******************************)

include Trail
include FourierMotzkin

exception Unsat_failure of Equation.equation list

(* stack is a list of Trail objects (ie states) *)
let rec dejeanAlgoRec stack =
  match stack with
  | [] -> failwith "Unknown"
  | (head::tail) ->
    (* recalculate the constraints each time... *)
    let cons = Trail.createConstraints head in
    (* here we have : either an empty list which means OK,
       either two contradicting equations *)
    match  Trail.checkConstraints cons with
    | Some(v,t1,t2) ->(
      (* damned, there is a failure, let's go back*)
      let previous = Equation.getPreviousEqs [t1; t2] in
      (* WARNING : if there is no previous equation, previous should return
         the equation itself*)
      (* WARNING : those previous equations could be either in
         the preceding state in the stack (if they came
         from an assignment of a variable), or in the current state
         (if they came from an FM resolution) *)
      (* we have to learn from it : FM on those previous equations *)
      (* but first, let's agree that if we cannot go back, the system
         itself was contradictory *)
         match tail with
         | [] -> raise (Unsat_failure previous)
         (* of course, in that case, all equations in previous
            are trivially equations from the beginning state*)
         | a::q ->
              try
                let neweq = FourierMotzkin.fourierMotzkin v previous in
                (* new equation found by resolution FM *)
                (* we go back and erase the current state, then add our new equation *)
                let newstack = (Trail.addEq a neweq)::q in
                (* back to constraint computing. There could be another failure*)
                dejeanAlgoRec newstack
              with _ -> failwith "Unable to solve a system by Fourier-Motzkin"
              (* that case should never happen...*)
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
              let newState = Trail.assignValue head var v in
              dejeanAlgoRec (newState::head::tail)


let dejeanAlgo l =
    let trail = Trail.create l in
    let stack = [trail] in
    dejeanAlgoRec stack
