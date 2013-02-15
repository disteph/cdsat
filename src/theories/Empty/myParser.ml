open Kernel

open Formulae
open Printf
open Atoms

(* converts a file to a string with its contents *)

let read_from_file filename =
  let lines = ref "" in
  let chan = open_in filename in
    try
      while true; do
	let line = input_line chan in
	  if not (line.[0]='c') then 
	    lines := (!lines)^"\n"^line
      done; ""
    with End_of_file ->
      close_in chan;
      !lines;

open String

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
		 | c    -> word_from_string s (word_so_far^(Char.escaped c)) (n+1)
	     end
	 in
	   word_from_string s "" n


let rec print_list = function
  | []   -> ()
  | s::l -> print_string (s^" "); print_list l

(* lex a cnf file to list of lists of literals, with auxiliary argument *)

let rec parse_cnf cnf_so_far = function
  | []     -> List.rev cnf_so_far
  | "0"::l -> parse_cnf cnf_so_far l
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



module Generate(F:FormulaImplem with type lit = Atom.t) = struct

  (* parse a literals from boolean (for sign) and string *)

  let generate_atom (b,t) = F.build (Lit (Atom.bbuild(b,t, [])))

  (* parse a clause from list of literal descriptions *)

  let rec generate_clause =  function
    | t::[] -> generate_atom t
	(*    | t::l  -> if (Random.bool())*)
    | t::l  -> if (true)
      then F.build (AndP((generate_atom t),(generate_clause l)))
      else F.build (AndN((generate_atom t),(generate_clause l)))
    | []    -> F.build (OrN(
			  F.build (Lit (Atom.bbuild(true,"p",[]))),
			  F.build (Lit (Atom.bbuild(false,"p",[])))
			))

  (* parse a cnf from list of clause descriptions *)

  let rec generate_cnf =  function
    | t::[] -> generate_clause t
    | t::l  -> if (false)
      then F.build (OrP((generate_clause t),(generate_cnf l)))
      else F.build (OrN((generate_clause t),(generate_cnf l)))
    | []    -> F.build (AndP(
			  F.build (Lit (Atom.bbuild(true,"p",[]))),
			  F.build (Lit (Atom.bbuild(false,"p",[])))
			))

  let parse filename = generate_cnf(parse_cnf_file (list_from_string (read_from_file filename) [] 0))

end
