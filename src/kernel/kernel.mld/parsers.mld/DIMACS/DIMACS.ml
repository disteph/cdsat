open String
open Parser

let rec list_from_string s list_so_far n = 
  if (n>=length s) then List.rev list_so_far 
  else
    match s.[n] with 
      | ' '  -> list_from_string s list_so_far (n+1)
      |	'\n' -> list_from_string s list_so_far (n+1)
      |	'\t' -> list_from_string s list_so_far (n+1)
      | '-'  -> list_from_string s ("-"::list_so_far) (n+1)
      |  _   -> let rec word_from_string s word_so_far n =
	   if (n>=length s) then List.rev (word_so_far::list_so_far) 
	   else
	     begin
	       match s.[n] with
		 | ' '  -> list_from_string s (word_so_far::list_so_far) n
		 | '\n' -> list_from_string s (word_so_far::list_so_far) n
		 | '\t' -> list_from_string s (word_so_far::list_so_far) n
		 | c    -> word_from_string s (word_so_far^(Parse.latexescaped c)) (n+1)
	     end
	 in
	   word_from_string s "" n


let rec print_list = function
  | []   -> ()
  | s::l -> print_string (s^" "); print_list l

(* lex a cnf file to list of lists of literals, with auxiliary argument *)

let rec parse_cnf cnf_so_far = function
  | []     -> List.rev cnf_so_far
  | "0"::l -> parse_cnf ((* []:: *)cnf_so_far) l
  | l -> let rec parse_clause clause_so_far ispos = function
      | []     -> parse_cnf ((List.rev clause_so_far)::cnf_so_far) []
      | "0"::l -> parse_cnf ((List.rev clause_so_far)::cnf_so_far) l
      | "-"::l -> parse_clause clause_so_far false l
      | s::l when ispos -> parse_clause ((true,s)::clause_so_far) true l
      | s::l   -> parse_clause ((false,s)::clause_so_far) true l
    in parse_clause [] true l

(* lex a cnf file to list of lists of literals *)

let rec parse_cnf_file = function
  | []     -> []
  | "p"::"cnf"::_::_::l -> parse_cnf [] l
  | a::l -> parse_cnf_file l


type afterglance = (bool*string) list list

let glance contents = 
  parse_cnf_file(list_from_string contents [] 0)

let guessThDecProc _ = Some ["bool"]

let parse (type t) l interpreter = 

  let (module I: InterpretType with type t=t) =
    interpreter ~decsorts:[] in

  (* parse a literals from boolean (for sign) and string *)
  let generate_atom (b,var) = 
    let v = I.decsymb var (Sort("prop",[]),[]) [] in
      if b then v else I.sigsymb "not" [v]
  in

  (* parse a clause from list of literal descriptions *)
  let generate_clause = function
    | [] -> I.sigsymb "false" []
    | l  -> I.sigsymb "or" (List.map generate_atom l)
  in

  List.map generate_clause l,
  None
