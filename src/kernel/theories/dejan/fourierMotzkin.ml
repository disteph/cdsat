include Num

(* eliminates a variable from a set of inequations.
Allows to compute the general FM resolution*)
let eliminate var eqs =
    (* separate the inequations *)
    let rec separate eqs =
    match eqs with
    | [] -> [], [], []
    | (t::q) -> let pos,neg,nul = separate q in
                if getCoeff t var >/ Num.num_of_int 0 then
                    (t::pos, neg, nul)
                else begin
                    if getCoeff t var </ Num.num_of_int 0 then
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
          (add (multiply (Num.num_of_int 1  // (getCoeff t1 var)) t1)
               (multiply (Num.num_of_int 1 // (getCoeff eq var)) eq) )::acc) accu l2)
    in
    complete_result pos neg nul

(* remove the trivial inequalities (no variable)*)
let rec remove_trivial = function
    | [] -> []
    | t::q when istrivial t -> remove_trivial q
    | t::q -> t::(remove_trivial q)

exception FM_Failure

(* a failure is exceptional. It can happen if the elimination
is not suited. Nevertheless, we will never encounter it
while using properly FM resolution in dejean's algorithm*)
let rec fourierMotzkin var eqs =
    (*print_eqs eqs;*)
    match remove_trivial eqs with
    | [] -> raise FM_Failure
    | [eq] -> eq
    | (t::q) ->
      try
        let vv = getAnotherActiveVar t var in
        fourierMotzkin var (eliminate vv eqs)
      (* we did not find another active variable. This is a
      unitary constraint on "var"... return it, then.*)
      with Not_found -> t
