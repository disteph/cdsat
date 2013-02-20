open Core
open EqAst
open Simplex

exception InvalidInput of exn

let inconsistency_model = fun eqs -> function
  |`NC_Base (coeffs1, coeffs2) ->
    let coeffs1 = Array.to_list coeffs1 in
    let coeffs2 = Array.to_list coeffs2 in
    let rec mask coefs ieqs res =
      match coefs, ieqs with
        |c :: ctl, e :: etl -> 
          if (Num.eq_num c Num.num_0) then mask ctl etl res
          else mask ctl etl (e :: res)
        |_ -> res
    in Some (mask coeffs2 eqs [])
  |`NC_Split _ -> Some eqs



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
    in let simplex = simplex_N data cttrs in
       match solve_N simplex with
         | `Unsatisfiable cert -> inconsistency_model eqs cert
         | `Satisfiable theta -> None
  end

let test_goal_inconsistency = fun eqs -> fun eq ->
  test_inconsistency (eq :: eqs)

let negation = fun e ->
  let coefs = e.eq_coeffs in
  let bound = e.eq_bound in
  let sign = e.eq_sign in
  let mk_neg s = 
        { eq_coeffs = coefs ;
          eq_sign   = s ;
          eq_bound  = bound }
  in
  begin
    match sign with
      |`Le -> mk_neg `Gt
      |`Ge -> mk_neg `Lt
      |`Lt -> mk_neg `Ge
      |`Gt -> mk_neg `Le
  end

let toString = fun e ->
  let coefs = e.eq_coeffs in
  let bound = e.eq_bound in
  let sign = e.eq_sign in
  let coefs_bindings = Core.StringMap.bindings coefs in
  let mk_smon = fun (id, c) -> (Num.string_of_num c) ^ id in
  let lsmon = List.map mk_smon coefs_bindings in
  let scoefs = String.concat " + " lsmon in
  let sbound = Num.string_of_num bound in
  let mk_string = fun ssign -> scoefs ^ ssign ^ sbound in
  begin
    match sign with
      |`Le -> mk_string " <= " 
      |`Ge -> mk_string " => " 
      |`Lt -> mk_string " < " 
      |`Gt -> mk_string " > " 
  end

let print_in_fmt = fun fmt e -> ()
