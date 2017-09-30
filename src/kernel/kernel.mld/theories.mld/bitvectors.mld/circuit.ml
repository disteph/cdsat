include HardCaml.Comb.Make(HardCaml.Transform.MakeCombGates(Signal))

(* let un = consti 4 1 *)
(* let () = print_endline(to_bstr un) *)
(* let () = print_endline(to_bstr(select un 1 0)) *)
                      
(* let deux = consti 4 2 *)
(* let () = print_endline(to_bstr deux) *)

(* let var = wire 4 *)
(* let dd = (((deux *+ var) +:. 1) &:. 1) ==:. 1 *)
(* let () = print_endline(to_string dd) *)
                   
(* let dd = (((var) +:. 1) &:. 1) ==:. 1 *)
(* let () = print_endline(to_string dd) *)

(* let smiii = (var <:. 3) *)
(* let () = print_endline(to_string smiii) *)

(* let odd = (select var 0 0 ==:. 1) &: (select var 0 0 ==:. 1) *)
(* let () = print_endline(to_string odd) *)
           
(* let () = print_endline(to_string(odd &: smiii)) *)

(* let complex = sll (select (var *: var) 3 0) 2 <: (var -: (srl var 3)) *)
(* let () = print_endline(to_string(complex)) *)

(* let test = negate(((var *: var) >:. 4) &: (var <:. 10)) *)
                      
(* let () = print_endline(to_string test) *)
