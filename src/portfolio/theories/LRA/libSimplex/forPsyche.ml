open Core
open EqAst
open Simplex

exception InvalidInput of exn

let inconsistency_model = fun eqs coeffs2 ->
(* function
  |`NC_Base (coeffs1, coeffs2) -> *)
    let coeffs2 = Array.to_list coeffs2 in
    let rec mask coefs ieqs res =
      match coefs, ieqs with
        |c :: ctl, e :: etl -> 
          if (Num.eq_num c Num.num_0) then mask ctl etl res
          else mask ctl etl (e :: res)
        |_ -> res
    in mask coeffs2 eqs []
(*  |`NC_Split _ -> Some eqs *)



let test_inconsistency = fun eqs ->
  begin
    let ids  = Array.of_list (EqAst.ids_of_equations eqs) in
    let aeqs  = Array.of_list eqs in
    let data =
      Array.map
        (fun eq ->
          Array.map
            (fun x ->
              try  Core.StringMap.find x eq.eq_coeffs
              with Not_found -> Num.num_0)
            ids)
        aeqs
    and cttrs =
      Array.map (fun eq -> (eq.eq_bound, eq.eq_sign)) aeqs
    in
      if Array.length data = 0 then None
      else
	let simplex = simplex_K data cttrs in
	  match solve_K simplex with
            | `Unsatisfiable cert -> 
              let reduced = inconsistency_model eqs cert in 
              begin
              match reduced with
                |[] -> failwith "Empty inconsistency certificate!"
                |_ -> Some reduced
              end
            | `Satisfiable theta -> None
  end

let test_goal_inconsistency = fun eqs -> fun eq ->
  test_inconsistency (eq :: eqs)

let hide1 c =
  if (Num.compare_num c Num.num_1 == 0)
  then ""
  else if (Num.compare_num c (Num.Int(-1)) == 0)
  then "-"
  else Num.string_of_num c

let toString = fun e ->
  let coefs = e.eq_coeffs in
  let bound = e.eq_bound in
  let sign = e.eq_sign in
  let coefs_bindings = Core.StringMap.elements coefs in
  let mk_smon = fun (id, c) -> (hide1 c) ^ StringComparable.reveal id in
  let lsmon = List.map mk_smon coefs_bindings in
  let scoefs = String.concat " + " lsmon in
  let sbound = Num.string_of_num bound in
  let mk_string = fun ssign -> scoefs ^ ssign ^ sbound in
  begin
    match sign with
      |`Le -> mk_string " \\leq " 
      |`Ge -> mk_string " \\geq " 
      |`Lt -> mk_string " < " 
      |`Gt -> mk_string " > " 
  end

let print_in_fmt fmt e =
  let print_sign_in_fmt = fun fmt -> function
    |`Le -> Format.fprintf fmt "%s" " \\leq " 
    |`Ge ->  Format.fprintf fmt "%s" " \\geq " 
    |`Lt ->  Format.fprintf fmt "%s" " < " 
    |`Gt ->  Format.fprintf fmt "%s" " > " 
  in
  let print_mon_in_fmt fmt (id, c) =
    Format.fprintf fmt "%s%s" (hide1 c) (StringComparable.reveal id)
  in
  let rec print_lcoef_in_fmt fmt l =
    begin
      match l with
        |[] -> ()
        |[m] -> Format.fprintf fmt "%a" print_mon_in_fmt m
        |m :: tl -> 
          Format.fprintf fmt "%a + %a" 
            print_mon_in_fmt m print_lcoef_in_fmt tl
    end in
  let coefs_bindings = Core.StringMap.elements e.eq_coeffs in
    Format.fprintf fmt "(%a %a %s)" 
      print_lcoef_in_fmt coefs_bindings 
      print_sign_in_fmt e.eq_sign 
      (Num.string_of_num e.eq_bound)
