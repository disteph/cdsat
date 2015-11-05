(******************************)
(* Internal implementation of dejean's algorithm *)
(******************************)

open Top
open Basic
open Specs

include Trail
include Equation
include FourierMotzkin

exception unsatFailure of Equation.t list


let dejeanAlgo l =
    let trail = Trail.create l
    let stack = [trail] in
    dejeanAlgoRec stack

(* stack is a list of Trail objects (ie states) *)
let rec dejeanAlgoRec stack =
    match stack with
    | [] -> failwith "Unknown"
    | (head::tail) ->
      (* recalculate the constraints each time... *)
      let cons = Trail.createConstraints  in
      (* here we have : either an empty list which means OK,
      either two contradicting equations *)
      match  Trail.checkConstraints with
      | (h::t) ->
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
          if (tail == []) then raise unsatFailure(previous);
          (* of course, in that case, all equations in previous
          are trivially equations from the beginning*)
          try
            let neweq = FourierMotzkin.fourierMotzkin previous in
          (* new equation found by resolution FM *)
          (* we go back and erase the current state, then add our new equation *)
            let (a::q) = tail in
            let newstack = (Trail.addEq a neweq)::q in
          (* back to constraint computing. There could be another failure*)
            dejeanAlgoRec
          with _ -> failwith "Unknown" (* that case should never happen...*)
      | _ -> () (* constraints are correct, let's choose a variable *)

      match Trail.chooseUnassignedVariable head with
      (* the model is complete*)
      | None -> Trail.getCurrentModel head
      | Some(var) ->
        (* choose a value according to the constraints *)
        let val = Trail.chooseValue cons var in
        let newState = Trail.assignValue var val head in
        dejeanAlgoRec (newState::head::tail)
