include Num
include Equation

(* eliminates a variable from a set of inequations.
Allows to compute the general FM resolution*)
let eliminate var eqs =
    (* separate the inequations *)
    let rec separate eqs =
    match eqs with
    | [] -> [], [], []
    | (t::q) -> let pos,neg,nul = separate q in
                if getCoeff t var >/ (Num.num_of_int 0) then
                    (t::pos, neg, nul)
                else begin
                    if getCoeff t var </ (Num.num_of_int 0) then
                      (pos, t::neg, nul)
                    else (pos, neg, t::nul)
                end
    in
    let pos, neg, nul = separate eqs in
    (*print_string "pos : ";
    print_eqs pos;print_string "neg : ";
    print_eqs neg;print_string "nul : ";
    print_eqs nul;*)
    (* create all the new inequations : we look recursively over
    all the inequations where our variable has a positive
    coefficient, and all where it has a negative one. Those two
    lists were given by the function separate *)
    let rec complete_result l1 l2 accu =
    match l1, l2 with
    | [], [] -> accu
    | (t::q), [] -> accu
    | [], (t::q) -> accu
    | (t1::q1), l2 ->
        complete_result q1 l2
        (List.fold_left (fun acc eq ->
            (combine (Num.num_of_int 1  // (getCoeff t1 var)) t1
                     (Num.num_of_int (-1) // (getCoeff eq var)) eq)::acc) accu l2)
    in
    complete_result pos neg nul

(* remove the trivial inequalities (no variable)*)
let rec remove_trivial = function
    | [] -> []
    | t::q when isTrivial t -> remove_trivial q
    | t::q -> t::(remove_trivial q)

exception FM_Failure

(* a failure is exceptional. It can happen if the elimination
is not suited. The exception indicates to the main algorithm that something
was wrong with the inequations. *)
let rec fourierMotzkinRec var eqs =
    match remove_trivial eqs with
    | [] -> raise FM_Failure
    | [eq] -> eq
    | (t::q) ->
      try
        let vv = getAnotherActiveVar t var in
        fourierMotzkinRec var (eliminate vv eqs)
      (* we did not find another active variable. This is a
      unitary constraint on "var"... return it, then.*)
      with Not_found -> t

(* FIXME : seems to fail for :
x1 + x5 + x4 +  < -4 /\ x1 + -4x5 + x4 +  < 1 /\ x1 + x5 + -4x4 +  < 1 *)
let fourierMotzkin var eqs =
    let eq = fourierMotzkinRec var eqs in
    if (Equation.isTrivial eq) then raise FM_Failure else
    (* return the equation obtained, which dependencies are
    all the PREVIOUS of the equations used for FM resolution *)
    (* that way, we enforce the invariant that all "previouses"
    of an equation are members of the previous state *)
    setDependance eq (Equation.getPreviousEqs eqs)
