(****************************************************)
(* TrailState :                                     *)
(* Represent a state of the trail during Dejan algo *)
(* Contains variable affectation and inequalities   *)
(****************************************************)

include Num
include Equation
include Interval
include Hashtbl

open Num

(* Represent data in a state of the resolution *)
type trail =
  {eqs : equation list;         (* System to solve *)
   affect : (var * value) list; (* Variables already affected in previous decisions *)
  }

(* constraints on each variable and the equation linked to it *)
type bornes = interval * equation option * equation option
type c = (var, bornes) Hashtbl.t

(* Create an initial Trail from a list of equations *)
let create eqList =
  {eqs = eqList; affect = []}

(* accessor to the list of equations *)
let getEqs t =
  t.eqs

(* Check equations of the trail to detect atomic constraint on variables *)
let createConstraints trail =
  let constraints = Hashtbl.create 10 in

  let updateConstraint c var eq =
    let coef = (Equation.getCoeff eq var) in
    let b = (Equation.getSup eq) // coef and isStrict = Equation.isStrict eq in
    let i, eqC1, eqC2 = c in
    (* coef < 0 : it is a minimum *)
    if coef </ (num_of_int 0) then
      begin
        match Interval.getInf i with
        | INFINITY -> (Interval.create (NUM b) isStrict (Interval.getSup i) (Interval.isSupStrict i)), (Some eq), eqC2
        | NUM(inf) when (b >/ inf || (b =/ inf && isStrict)) ->
          (Interval.create (NUM b) isStrict (Interval.getSup i) (Interval.isSupStrict i)), (Some eq), eqC2
        | _ -> c
      end
      (* coef > 0 : it is a maximum *)
    else
      begin
        match Interval.getSup i with
        | INFINITY -> (Interval.create (Interval.getInf i) (Interval.isInfStrict i) (NUM b) isStrict), eqC1, (Some eq)
        | NUM(sup) when (b </ sup || (b =/ sup && isStrict)) ->
          (Interval.create (Interval.getInf i) (Interval.isInfStrict i) (NUM b) isStrict), eqC1, (Some eq)
        | _ -> c
      end
  in
  (* Look for atomic equation and call update constraint on them *)
  let searchAtomic eq =
    if Equation.isAtomic eq then begin
      let v = Equation.getActiveVar eq in
      let c = try Hashtbl.find constraints v with Not_found -> (Interval.real, None, None) in
      let newConstraint = updateConstraint c v eq in
      Hashtbl.replace constraints v newConstraint
    end
  in List.iter searchAtomic trail.eqs; constraints

exception Eq_found of (equation * equation) option

(* Look for invalid constraints, ie empty intervals on constraints *)
(* Return the two atomic equation trigering these invalidity *)
let checkConstraints constraints =
  let check v c =
    match c with
    | i, Some(e1), Some(e2) -> if Interval.isEmpty i then raise (Eq_found (Some (e1,e2)))
    | _ -> ()
  in try Hashtbl.iter check constraints; None with Eq_found o -> o

exception Val_found of num

(* Choose a valid value for var according to constraints *)
let chooseValue constraints var =
  let i, _, _ = (try Hashtbl.find constraints var with Not_found -> Interval.real, None, None) in
    (Interval.chooseValue i)

(* assign a value to a variable and add a new state to the stack *)
let assignValue trail var value =
  {eqs = List.map (fun eq -> Equation.affectVar eq var value) trail.eqs;
   affect = (var, value)::trail.affect}

(* return the last variable to be assigned in order to backtrack *)
let getLastAssignedVariable trail =
    match trail.affect with
    | [] -> failwith "No variable was assigned in this state"
    | (var,value)::q -> var

exception Var_found of var

(* Return an unsigned variable if it exists *)
let chooseUnassignedVariable trail =
  let f eq = if not (isTrivial eq) then raise (Var_found (getActiveVar eq))
  in try (List.iter f trail.eqs; None)
  with Var_found v -> Some v

(* the current model as defined at this point of the algorithm *)
(* /!\ may not be coherent *)
let getCurrentModel trail =
  trail.affect

(* Add a new equation in trail *)
let addEq trail eq =
  {eqs= eq::trail.eqs; affect = trail.affect}
