
include Dejan

let rec print_model_assignments = function
    | [] -> print_string "\n"
    | (var,value)::q -> print_string "("; print_string var;
                        print_string " : ";
                        print_string (string_of_num value);
                        print_string "),";
                        print_model_assignments q

let test_dejan eqs =
    print_string "Trying to solve the following system : \n";
    Equation.print_eqs eqs;
    try
      let model = Dejan.dejeanAlgo eqs in
      print_string "A solution is : \n";
      print_model_assignments model;
    with
    | Unsat_failure(l) -> print_string "Sorry, this is UNSAT. The
    following inequations lead to a contradiction : \n";
      Equation.print_eqs l

let () =
    (* −y < 0, z − x + 2y < 0, x − y < 0, −z < −1 *)
    (* [e1; e2; e3; e4] should be UNSAT*)
    let e1 = Equation.createFromList [("y", num_of_int (-1))] (num_of_int 0) false [] in
    let e2 = Equation.createFromList [("y", num_of_int 2); ("z", num_of_int 1); ("x", num_of_int (-1))] (num_of_int 0) false [] in
    let e3 = Equation.createFromList [("x", num_of_int 1); ("y", num_of_int (-1))] (num_of_int 0) false [] in
    let e4 = Equation.createFromList [("z", num_of_int (-1))] (num_of_int (-1)) false [] in
    (*print_string "created equations OK\n";*)
    (*test_dejan [e1]*)
    print_string "ok";
