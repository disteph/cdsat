include Num
open Equation


(* eliminates a variable from a set of inequations.
Allows to compute the general FM resolution*)
let eliminate var eqs =

  (* separate the inequations depending on the signum of the coefficient *)
  (* tail-recursive, but not optimal *)
  let separate eqs =
    (List.filter (fun t -> Equation.getCoeff t var >/ (Num.num_of_int 0)) eqs),
    (List.filter (fun t -> Equation.getCoeff t var </ (Num.num_of_int 0)) eqs),
    (List.filter (fun t -> Equation.getCoeff t var =/ (Num.num_of_int 0)) eqs)
  in

  let pos, neg, nul = separate eqs in

  (* create all the new inequations : we look recursively over
     all the inequations where our variable has a positive
     coefficient, and all where it has a negative one. Those two
     lists were given by the function separate *)

  (*this function is tail-recursive*)
  let rec complete_result l1 l2 accu =
    match l1, l2 with
    | [], _ | _, [] -> accu
    | (t1::q1), l2 ->
    complete_result q1 l2
      (List.fold_left (fun acc eq ->
        (combine (Num.num_of_int 1  // (Equation.getCoeff t1 var)) t1
           (Num.num_of_int (-1) // (Equation.getCoeff eq var)) eq)::acc) accu l2)

  in
  complete_result pos neg nul

(* remove the trivial inequalities (no variable)*)
let remove_trivial ll =
    List.filter (fun eq -> not(Equation.isTrivial eq)) ll

exception FM_Failure

(* a failure is exceptional. It can happen if the elimination
is not suited. The exception indicates to the main algorithm that something
was wrong with the inequations. *)

let fourierMotzkin var eqs =
  let rec fourierMotzkinRec var eqs =
  (*  print_string "enter FM";*)
    match remove_trivial eqs with
    | [] -> raise FM_Failure
    | [eq] -> eq
    | (t::q) ->
      let myvar =
       try
         Some(Equation.getAnotherActiveVar t var)
          (* we did not find another active variable. This is a
             unitary constraint on "var"... return it, then.*)
       with Not_found -> None
       in
       match myvar with
       | None -> t
       | Some(vv) ->  fourierMotzkinRec var (eliminate vv eqs)

  in
  try(
    let eq = fourierMotzkinRec var eqs in
          (* return the equation obtained, which dependencies are
             all the PREVIOUS of the equations used for FM resolution *)
          (* that way, we enforce the invariant that all "previouses"
             of an equation are members of the previous state *)
    Some(Equation.setDependance eq (Equation.getPreviousEqs eqs))
  )
  with FM_Failure -> None
