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

  type t
  (*The state equation list itself*)
  type c
  (* the constraints *)
  val create : equation list -> t
  val createConstraints : t -> c
  val checkConstraints : c -> (equation * equation) option
  val chooseValue : c -> variable -> value

  (* assign a value to a variale and add a new state to the stack*)
  val assignValue : t -> variable -> value -> t
  (* suppresses the current state, applies FM on the terms, adds the
  new term to the state, raises unsatFailure exception if we go back to the first state*)
  val addEq : t -> equation -> t (* To do*)

  val getCurrentModel : t -> (variable * value) list
  val chooseUnassignedVariable : t -> variable Option
