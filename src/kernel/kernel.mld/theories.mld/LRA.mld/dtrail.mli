(****************************************************)
(* TrailState :                                     *)
(* Represent a state of the trail during Dejan algo *)
(* Contains variable affectation and inequalities   *)
(****************************************************)

open Termstructures.Rationals
type trail

type c

val create : Equation.equation list -> trail
val createConstraints : trail -> c
val checkConstraints : c -> (Equation.equation * Equation.equation) option

val getEqs : trail -> Equation.equation list
val getLastAssignedVariable : trail -> Equation.var
val getAssignedVariables : trail -> Equation.var list

val addEq : trail -> Equation.equation -> trail
val addEqs : trail -> Equation.equation list -> trail
val assignValue : trail -> Equation.var -> Equation.value -> trail

val getCurrentModel : trail -> (Equation.var * Equation.value) list
val chooseUnassignedVariable : trail -> Equation.var option
val chooseValue : c -> Equation.var -> Equation.value
