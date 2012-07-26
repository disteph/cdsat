(* Main file *)

open Formulae;;
open Io;;
open Test;;
open MySmart;;
open MyPatricia;;

include Tests(MyPAT);;

Search.debug := 1;;

(* p(x) \/- !p(x) *)
let f1 = 
  UF.build(OrN(
	      UF.build(Lit(true,"p",[])),
	      UF.build(Lit(false,"p",[]))
	    )
	  )
;;

(* p(x) \/+ !p(x) *)
let f2 = 
  UF.build(OrP(
	    UF.build(Lit(true,"p",[])),
	    UF.build(Lit(false,"p",[]))
	  ))
;;

(* !p(x) \/+ p(x) : infinite computation if proof-search is depth-first*)
let f3 = 
  UF.build(OrP(
	    UF.build(Lit(false,"p",[])),
	    UF.build(Lit(true,"p",[]))
	  ))
;;

(* (a \/- b) \/- (!a /\- !b) *)

let f4 = 
  UF.build(OrN(
	    UF.build(OrN(
		      UF.build(Lit(true,"a",[])),
		      UF.build(Lit(true,"b",[]))
		    )),
	    UF.build(AndN(
		      UF.build(Lit(false,"a",[])),
		      UF.build(Lit(false,"b",[]))
		    ))
	  ))
;; 

(* (a \/+ b) \/- (!a /\- !b) *)
let f5 = 
  UF.build(OrN(
	    UF.build(OrP(
		      UF.build(Lit(true,"a",[])),
		      UF.build(Lit(true,"b",[]))
		    )),
	    UF.build(AndN(
		      UF.build(Lit(false,"a",[])),
		      UF.build(Lit(false,"b",[]))
		    ))
	  ))
;; 

(* (!a \/+ !b) not provable - naive algorithm goes into infinite computation *)
let f6 = 
  UF.build(OrP(
	    UF.build(Lit(false,"a", [])), 
	    UF.build(Lit(false,"b", []))
	  ))
;; 

(* (!a /\- !b) *)

let f7=
  UF.build(AndN(
	    UF.build(Lit(false,"a", [])), 
	    UF.build(Lit(false,"b", []))
	  ))
;;

let f8=
  UF.build(OrN(
	      UF.build(OrP(
			  UF.build(Lit(false,"p",[])),
			  UF.build(Lit(false,"q",[]))
		)),
	      UF.build(AndP(
			  UF.build(Lit(true,"p",[])),
			  UF.build(Lit(true,"q",[]))
		))
	    ))
;;

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


 treatfile "test.cnf";; 

(* treatdir("sat-2002-beta/generated/gen-9/gen-9.1");; *)

(* write_to_file "latex/eurecaml.tex" (printanswer (treatfile "test.cnf"));;*)

