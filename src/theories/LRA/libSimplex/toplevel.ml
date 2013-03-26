open Core
open EqAst

exception InvalidInput of exn

let parse_equation = fun input ->
  let lexbuf = Lexing.from_string input in
    EqParser.equation EqLexer.lexer lexbuf

let read_and_solve_problem = fun ?(stream = stdin) () ->
  let respace = Str.regexp "^[ \t]*$" in
  let rec input = fun data ->
    let line =
      try  Some (input_line stream)
      with End_of_file -> None
    in
      match line with
      | Some line ->
          if   Str.string_match respace line 0
          then input data
          else input (line :: data)
      | None      -> data
  in
    match List.rev (input []) with
    | ((_ :: _) as eqs) -> begin
        let eqs = 
          try  List.map parse_equation eqs
          with
          | EqLexer.LexError _  as e -> raise (InvalidInput e)
          | Parsing.Parse_error as e -> raise (InvalidInput e)
        in
        let ids  = Array.of_list (EqAst.ids_of_equations eqs) in
        let eqs  = Array.of_list eqs in
        let data =
          Array.map
            (fun eq ->
               Array.map
                 (fun x ->
                    try  Core.StringMap.find x eq.eq_coeffs
                    with Not_found -> Num.num_0)
                 ids)
            eqs
        and cttrs =
          Array.map (fun eq -> (eq.eq_bound, eq.eq_sign)) eqs
        in let simplex = Simplex.simplex_N data cttrs in
          match Simplex.solve_N simplex with
          | `Unsatisfiable cert ->
              Printf.fprintf stderr "unsatisfiable\n%!";
              Printf.fprintf stderr "%s%!" (Simplex.string_of_ucert_N cert)
          | `Satisfiable theta ->
              Printf.fprintf stderr "solved: %s\n%!"
                (String.concat ", "
                   (Array.to_list (Array.map Big_int.string_of_big_int theta)))
      end
  
    | _ ->
        raise (InvalidInput (Failure "input is too short"))
