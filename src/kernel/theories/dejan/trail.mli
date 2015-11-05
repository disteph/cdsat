(****************************************************)
(* TrailState :                                     *)
(* Represent a state of the trail during Dejan algo *)
(* Contains variable affectation and inequalities   *)
(****************************************************)

(*
Liste de variables.
Inégalité : table variables -> coeff + majorant + large ou stricte.
=> liste d'inégalité
Affectation : tables variables -> option valeur
Contraintes atomiques : tables variables -> intervale ??

/!\ Toutes les inégalités et les contraintes atomiques doivent avoir une ref
vers les inégalités dont elles proviennente
*)

type trail
(*The state equation list itself*)
type c
(* the constraints *)
val create : Equation.equation list -> trail
val createConstraints : trail -> c
val checkConstraints : c -> (Equation.var * Equation.equation * Equation.equation) option
val chooseValue : c -> Equation.var -> Equation.value

(* assign a value to a variale and add a new state to the stack*)
val assignValue : trail -> Equation.var -> Equation.value -> trail
(* suppresses the current state, applies FM on the terms, adds the
new term to the state, raises unsatFailure exception if we go back to the first state*)
val addEq : trail -> Equation.equation -> trail

val getCurrentModel : trail -> (Equation.var * Equation.value) list
val chooseUnassignedVariable : trail -> Equation.var option
