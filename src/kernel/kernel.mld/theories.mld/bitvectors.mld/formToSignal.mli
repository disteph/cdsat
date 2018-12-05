(* Evaluation of unit constraints into circuits *)    
open Top
open Terms

(* Direct evaluation functions for bit-vector terms *)
val term_eval : MyTheory.valuation -> Term.t -> Circuit.t
                                                             
(* Direct evaluation functions for bit-vector formulae (optionally with truth value assigned)
   output should be a Circuit of width 1 *)
val form_eval : MyTheory.valuation -> ?truthvalue:bool -> Term.t -> Circuit.t
