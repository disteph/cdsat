(* Main file *)

open Formulae;;
open Strategy;;
open Io;;
open Test;;
open MySmart;;

include Tests(MySmartUserStrategy);;

Search.debug := 1;;

(* p(x) \/- !p(x) *)
let f1 = 
  F.build(OrN(
	      F.build(Lit(true,"p",[])),
	      F.build(Lit(false,"p",[]))
	    )
	  )
;;

(* p(x) \/+ !p(x) *)
let f2 = 
  F.build(OrP(
	    F.build(Lit(true,"p",[])),
	    F.build(Lit(false,"p",[]))
	  ))
;;

(* !p(x) \/+ p(x) : infinite computation if proof-search is depth-first*)
let f3 = 
  F.build(OrP(
	    F.build(Lit(false,"p",[])),
	    F.build(Lit(true,"p",[]))
	  ))
;;

(* (a \/- b) \/- (!a /\- !b) *)

let f4 = 
  F.build(OrN(
	    F.build(OrN(
		      F.build(Lit(true,"a",[])),
		      F.build(Lit(true,"b",[]))
		    )),
	    F.build(AndN(
		      F.build(Lit(false,"a",[])),
		      F.build(Lit(false,"b",[]))
		    ))
	  ))
;; 

(* (a \/+ b) \/- (!a /\- !b) *)
let f5 = 
  F.build(OrN(
	    F.build(OrP(
		      F.build(Lit(true,"a",[])),
		      F.build(Lit(true,"b",[]))
		    )),
	    F.build(AndN(
		      F.build(Lit(false,"a",[])),
		      F.build(Lit(false,"b",[]))
		    ))
	  ))
;; 

(* (!a \/+ !b) not provable - naive algorithm goes into infinite computation *)
let f6 = 
  F.build(OrP(
	    F.build(Lit(false,"a", [])), 
	    F.build(Lit(false,"b", []))
	  ))
;; 

(* (!a /\- !b) *)

let f7=
  F.build(AndN(
	    F.build(Lit(false,"a", [])), 
	    F.build(Lit(false,"b", []))
	  ))
;;

let f8=
  F.build(OrN(
	      F.build(OrP(
			  F.build(Lit(false,"p",[])),
			  F.build(Lit(false,"q",[]))
		)),
	      F.build(AndP(
			  F.build(Lit(true,"p",[])),
			  F.build(Lit(true,"q",[]))
		))
	    ))
;;

let print_test f = "Trying to prove: $"^Src.Seq.Form.toString f^"$

\\vspace{10pt}\n"^
				      Src.Ans.toString (go f)^"\\vspace{30pt}

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

