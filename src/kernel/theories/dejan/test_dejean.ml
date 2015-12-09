include Algo

let rec print_model_assignments = function
    | [] -> print_string "\n"
    | (var,value)::q -> print_string "("; print_int var;
                        print_string " : ";
                        print_string (string_of_num value);
                        print_string "),";
                        print_model_assignments q

let test_dejan eqs =
    print_string "Trying to solve the following system : \n";
    Equation.print_eqs eqs;
    try
      let trail = Trail.create eqs in
      let stack = [trail] in
      let model,stack = dejeanAlgoRec stack in
      print_string "A solution is : \n";
      print_model_assignments model;
    with
    | Unsat_failure(l,s) -> print_string "Sorry, this is UNSAT. The
    following inequations lead to a contradiction : \n";
      Equation.print_eqs l

(* creates a special system of inequations, for testing. It should
trigger a lot of FM resolutions *)
let system n =
  let createTerm = fun j i n ->
    if (i=j) then (i, num_of_int (1-n))
    else (j, num_of_int (1))
  in
  let rec createIthInequation = function
    | 0,i,n -> []
    | j,i,n -> (createTerm j i n)::(createIthInequation ((j-1),i,n))
  in
  let rec createSystem = function
    | 0,n -> []
    | i,n -> (
      Equation.createFromList (createIthInequation (n,i,n)) (num_of_int 0) true []
      )::(createSystem ((i-1),n))
  in
  createSystem (n, n)

let () =
    Printexc.record_backtrace true;
    (* −y < 0, z − x + 2y < 0, x − y < 0, −z < −1 *)
    (* [e1; e2; e3; e4] should be UNSAT*)
    (*let e1 = Equation.createFromList [("y", num_of_int (-1))] (num_of_int 0) false [] in
    let e2 = Equation.createFromList [("y", num_of_int 2); ("z", num_of_int 1); ("x", num_of_int (-1))] (num_of_int 0) false [] in
    let e3 = Equation.createFromList [("x", num_of_int 1); ("y", num_of_int (-1))] (num_of_int 0) false [] in
    let e4 = Equation.createFromList [("z", num_of_int (-1))] (num_of_int (-1)) false [] in
    test_dejan [e1;e2;e3;e4];*)
    let l = system 24 in
    (*let eq = FourierMotzkin.fourierMotzkin 0 (List.tl (List.tl l)) in
    match eq with
    | None -> print_string "None\n"
    | Some(eq) -> Equation.print eq;*)
    test_dejan l;


    (*let e5 = Equation.createFromList [("x1", num_of_int 1); ("x2", num_of_int 1); ("x3", num_of_int 1)] (num_of_int (-4)) true [] in
    let e6 = Equation.createFromList [("x1", num_of_int 1); ("x2", num_of_int (-4)); ("x3", num_of_int 1)] (num_of_int 1) true [] in
    let e7 = Equation.createFromList [("x1", num_of_int 1); ("x2", num_of_int 1); ("x3", num_of_int (-4))] (num_of_int 1) true [] in
    let eq = FourierMotzkin.fourierMotzkin "x1" [e5;e6;e7] in
    Equation.print_eqs [e5;e6;e7];
    Equation.print eq;
    print_string "\n"*)
