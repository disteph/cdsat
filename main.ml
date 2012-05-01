(* Main file *)

open Formulae;;
open Strategy;;
open Io;;
open Test;;

module TestList = Tests(MyUserStrategy);;
include TestList;;

(* p(x) \/- !p(x) *)
let my_f1 = 
  F.build(Neg(
	    OrN(
	      F.build(Pos(
			PosAtom("p",
				[V("x")])
		      )),
	      F.build(Neg(
			NegAtom("p",
				[V("x")])
		      ))
	    )
	  )
	 )
;;

(* p(x) \/+ !p(x) *)
let my_f2 = 
  F.build(Pos(
	    OrP(
	      F.build(Pos(
			PosAtom("p",
				[V("x")])
		      )),
	      F.build(Neg(
			NegAtom("p",
				[V("x")])
		      ))
	    )
	  )
	 )
;;

(* !p(x) \/+ p(x) : infinite computation if proof-search is depth-first*)
let my_f3 = 
  F.build(Pos(
	    OrP(
	      F.build(Neg(
			NegAtom("p",
				[])
		      )),
	      F.build(Pos(
			PosAtom("p",
				[])
		      ))
	    )
	  )
	 )
;;

(* (a \/- b) \/- (!a /\- !b) *)

let my_f4 = 
  F.build(Neg(
	    OrN(
	      F.build(Neg(
			OrN(
			  F.build((Pos(
				     PosAtom("a", [])))),
			  F.build(Pos(
				    PosAtom("b", [])))
			)
		      )),
	      F.build(Neg(
			AndN(
			  F.build(Neg(
				    NegAtom("a", []))), 
			  F.build(Neg(
				    NegAtom("b", [])))
			)
		      )
		     )
	    )
	  )
	 )
;; 

(* (a \/+ b) \/- (!a /\- !b) *)
let my_f5 = 
  F.build(Pos(
	    OrP(
	      F.build(Pos(
			OrP(
			  F.build(Pos(
				    PosAtom("a", []))),
			  F.build(Pos(
				    PosAtom("b", [])))
			)
		      )),
	      F.build(Neg(
			AndN(
			  F.build(Neg(
				    NegAtom("a", []))), 
			  F.build(Neg(
				    NegAtom("b", [])))
			)
		      ))
	    )
	  )
	 )
;; 

(* (!a \/+ !b) not provable - naive algorithm goes into infinite computation *)
let my_f6 = 
  F.build(Pos(
	    OrP(
	      F.build(Neg(
			NegAtom("a", []))), 
	      F.build(Neg(
			NegAtom("b", [])))
	    )
	  )
	 )
;; 

write_to_file "latex/eurecaml.tex" (Src.Ans.toString (go my_f1)^"\\vspace{30pt}"^
				      (Src.Ans.toString (go my_f2))^"\\vspace{30pt}"^
				      (Src.Ans.toString (go my_f3))^"\\vspace{30pt}"^
				      (Src.Ans.toString (go my_f4))^"\\vspace{30pt}"^
				      (Src.Ans.toString (go my_f5))^"\\vspace{30pt}"^
				      (Src.Ans.toString (go my_f6))
				   )
;;

(* treatdir("sat-2002-beta/generated/gen-9/gen-9.1");; *)

(*treatfile "test.cnf";;*)
(* write_to_file "latex/eurecaml.tex" (printanswer (treatfile "test.cnf"));;*)

