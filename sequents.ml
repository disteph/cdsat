open Zipper;;
include Zipper;;
open Formulae;;
include Formulae;;

(* Zipper of (negative) atoms * Formula in Focus * Zipper of positive formulae * List of positive formulae on which focus has been placed more times than others*)
(* Zipper of (negative) atoms * Formulae to inverse * Zipper of positive formulae * List of positive formulae on which focus has been placed more times than others*)
type seq = 
    EntF of (((string*(term list)) ref) zipper)*formula*((posFormula ref) zipper)*((posFormula ref) list)
  | EntUF of (((string*(term list)) ref) zipper)*(formula zipper)*((posFormula ref) zipper)*((posFormula ref) list);;

(*
 * Type of proof-trees
 *)
type prooftree = 
    Axiom of seq 
  | OnePre of seq*prooftree 
  | TwoPre of seq*prooftree*prooftree
;;

(*
 * Type of answers. 
 * In case of fail, we indicate the points where we got tired (a zipper of computations that have been halted, using weak reduction of OCaml))
 *)
type answer = Success of prooftree | Fail of (((unit -> answer)* seq) zipper);;


(*
  PRETTY-PRINTING (in Latex syntax)
 *)

(* Displays sequent *)

let rec printseq = function
    EntUF(atomsN, unfocused, formuP, formuPSaved)
    -> " \\DerOSNeg {"^
      (printzipper (function x-> match !x with (s, tl) -> s^"("^printtl(tl)^")") " \\cdot " atomsN)^
      "} {"^(printzipper printformula " \\cdot " unfocused)^"}"^
      "{"^(printzipper (function x -> printformulaP (!x)) " \\cdot " formuP)^"\\mid "^
      (printl true (function x -> printformulaP (!x)) formuPSaved)^"}";
      
  | EntF(atomsN, focused, formuP, formuPSaved)
    -> " \\DerOSPos {"^(printzipper (function x-> match !x with  (s, tl) -> s^"("^printtl(tl)^")") " \\cdot " atomsN)^
      "} {"^(printformula focused)^"}"^
        "{"^(printzipper (function x -> printformulaP (!x)) " \\cdot " formuP)^"\\mid "^
        (printl true (function x -> printformulaP (!x)) formuPSaved)^"}"
;;

(* Displays prooftree *)
let rec printprooftree = function
    OnePre (a,b) -> "\\infer {"^(printseq a)^"}{"^printprooftree(b)^"}";
  | TwoPre (a,b,c) -> "\\infer {"^(printseq a)^
      "}{"^printprooftree(b)^
        " \\quad "^printprooftree(c)^"}";
  | Axiom (a) -> "\\infer {"^(printseq a)^"}{}"         
;;


(* Displays answer *)
let printanswer = function
    Success( p ) -> "$$"^printprooftree p^"$$";
  | Fail(x) -> "\\textsf {FAIL} \\\\"^printzipper (function (y, z) -> "$$"^(printseq z)^"$$") " " (x)
;;
