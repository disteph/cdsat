(* Main file *)

open Flags
open Formulae
open Io
open Test
open MyPatricia

include Tests(MyPAT)
include PrintableFormula(MyPAT.UF)

(* p(x) \/- !p(x) *)
let f1 = 
  orN(
    lit(true,"p",[]),
    lit(false,"p",[])
  )

(* p(x) \/+ !p(x) *)
let f2 = 
  orP(
    lit(true,"p",[]),
    lit(false,"p",[])
  )

(* !p(x) \/+ p(x) : infinite computation if proof-search is depth-first*)
let f3 = 
  orP(
    lit(false,"p",[]),
    lit(true,"p",[])
  )

(* (a \/- b) \/- (!a /\- !b) *)

let f4 = 
  orN(
    orN(
      lit(true,"a",[]),
      lit(true,"b",[])
    ),
    andN(
      lit(false,"a",[]),
      lit(false,"b",[])
    )
  )

(* (a \/+ b) \/- (!a /\- !b) *)
let f5 = 
  orN(
    orP(
      lit(true,"a",[]),
      lit(true,"b",[])
    ),
    andN(
      lit(false,"a",[]),
      lit(false,"b",[])
    )
  )

(* (!a \/+ !b) not provable - naive algorithm goes into infinite computation *)
let f6 = 
  orP(
    lit(false,"a", []), 
    lit(false,"b", [])
  )

(* (!a /\- !b) *)

let f7=
  andN(
    lit(false,"a", []), 
    lit(false,"b", [])
  )

let f8=
  orN(
    orP(
      lit(false,"p",[]),
      lit(false,"q",[])
    ),
    andP(
      lit(true,"p",[]),
      lit(true,"q",[])
    )
  )

let print_test f = "Trying to prove: $"^Src.FE.Form.toString f^"$

\\vspace{10pt}\n"^
  Src.FE.toString (go f)^"\\vspace{30pt}

";;

write_to_file "latex/eurecaml.tex" (print_test f1^
				      print_test f2^
				      print_test f3^
				      print_test f4^
				      print_test f5^
				      print_test f6^
				      print_test f7^
				      print_test f8
				   )
;;


if !Flags.do_file then let _ =treatfile "test.cnf" in ();;

(* treatdir("problems/sat/uf20-91");; *)

(* write_to_file "latex/eurecaml.tex" (printanswer (treatfile "test.cnf"));;*)

