open Format

include HardCaml.Comb.Make(HardCaml.Transform.MakeCombGates(Signal))

(*******************************)
(* test cases *)
    
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

(*******************************)

(* is the constant true signal (implies width = 1) *)
let isT a = Signal.isSyntacticT(a ==: vdd)

(* is the constant false signal (implies width = 1) *)
let isF a = Signal.isSyntacticT(a ==: (~: vdd))

(* Represent the same signal *)
let equal a b = isT(a ==: b)
let compare a b =
  if equal a b then 0
  else if isT(a <=: b) then 1 else -1

(* hash function only uses the lowest 16 bits: to be refined later? *)
let hash a =to_int(select a 16 0) 
let hash_fold_t = Hash.hash2fold hash

let pp fmt a =
  let rec aux fmt = function
    | [] -> ()
    | t::q -> fprintf fmt "%s%a" (if t then "1" else "0") aux q
  in
  aux fmt (List.map isT (bits a))

let show = Format.stringOf pp

