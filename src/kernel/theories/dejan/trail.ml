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

(*type variable = string*)
(*type value = Num.num*)

type trail = {eqs : equation list;
          affect : (var * value) list;
        }

(*The state equation list itself*)

(* the constraints *)
type bornes = interval * equation option * equation option
type c = (var, bornes) Hashtbl.t

(* Create an initial Trail from a list of equations *)
let create eqList =
  {eqs = eqList; affect = []}

let getEqs t =
  t.eqs

(* Check the equation of the trail to detect atomic constraint on varaibles *)
let createConstraints trail =
  let constraints = Hashtbl.create 10 in

  let updateConstraint c v eq =
    let coef = (Equation.getCoeff eq v) in
    let b = (Equation.getSup eq) // coef and isStrict = Equation.isStrict eq in
    let i, eqC1, eqC2 = c in
    if coef </ (num_of_int 0) then
      begin
        match Interval.getInf i with
        | INFINITY -> (Interval.create (NUM b) isStrict (Interval.getSup i) (Interval.isSupStrict i)), (Some eq), eqC2
        | NUM(inf) when (b >/ inf || (b =/ inf && isStrict)) ->
          (Interval.create (NUM b) isStrict (Interval.getSup i) (Interval.isSupStrict i)), (Some eq), eqC2
        | _ -> c
      end
    else
      begin
        match Interval.getSup i with
        | INFINITY -> (Interval.create (Interval.getInf i) (Interval.isInfStrict i) (NUM b) isStrict), eqC1, (Some eq)
        | NUM(sup) when (b </ sup || (b =/ sup && isStrict)) ->
          (Interval.create (Interval.getInf i) (Interval.isInfStrict i) (NUM b) isStrict), eqC1, (Some eq)
        | _ -> c
      end
  in
  let searchAtomic eq =
    if Equation.isAtomic eq then begin
      let v = Equation.getActiveVar eq in
      let c = try Hashtbl.find constraints v with Not_found -> (Interval.real, None, None) in
      let newConstraint = updateConstraint c v eq in
      Hashtbl.replace constraints v newConstraint
    end
  in List.iter searchAtomic trail.eqs; constraints

exception Eq_found of (equation * equation) option

let checkConstraints constraints =
  let check v c =
    match c with
    | i, Some(e1), Some(e2) -> if Interval.isEmpty i then raise (Eq_found (Some (e1,e2)))
    | _ -> ()
  in try Hashtbl.iter check constraints; None with Eq_found o -> o

exception Val_found of num

let chooseValue constraints var =
  let i, _, _ = (try Hashtbl.find constraints var with Not_found -> Interval.real, None, None) in
    (Interval.chooseValue i)

(* assign a value to a variale and add a new state to the stack*)
let assignValue trail var value =
  {eqs = List.map (fun eq -> Equation.affectVar eq var value) trail.eqs;
   affect = (var, value)::trail.affect}

let getLastAssignedVariable trail =
    match trail.affect with
    | [] -> failwith "No variable was assigned in this state"
    | (var,value)::q -> var

exception Var_found of var

let chooseUnassignedVariable trail =
  let f eq = if not (isTrivial eq) then raise (Var_found (getActiveVar eq))
  in try (List.iter f trail.eqs; None)
  with Var_found v -> Some v


let getCurrentModel trail =
  trail.affect

let addEq trail eq =
  {eqs= eq::trail.eqs; affect = trail.affect}
