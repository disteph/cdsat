open Zipper;;
include Zipper;;
open Formulae;;
include Formulae;;

(* List d'atomes negatifs * FormuleFocusee * Liste de formules positives * Formules positives saved*)
type seq = 
    EntF of (((string*(term list)) ref) zipper)*formula*((posFormula ref) zipper)*((posFormula ref) list)
  | EntUF of (((string*(term list)) ref) zipper)*(formula zipper)*((posFormula ref) zipper)*((posFormula ref) list);;

(*
 * Structure d'arbre de preuve
 *)
type prooftree = 
    Axiom of seq 
  | OnePre of seq*prooftree 
  | TwoPre of seq*prooftree*prooftree
;;
(*
 * Renvoie un proovtree si le théorême est vrai
 *)
type answer = Success of prooftree | Fail of (((unit -> answer)* seq) zipper);;



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

(* Affiche un prooftree en latex *)
let rec printprooftree = function
    OnePre (a,b) -> "\\infer {"^(printseq a)^"}{"^printprooftree(b)^"}";
  | TwoPre (a,b,c) -> "\\infer {"^(printseq a)^
      "}{"^printprooftree(b)^
        " \\quad "^printprooftree(c)^"}";
  | Axiom (a) -> "\\infer {"^(printseq a)^"}{}"         
;;


let printanswer = function
    Success( p ) -> "$$"^printprooftree p^"$$";
  | Fail(x) -> "\\textsf {FAIL} \\\\"^printzipper (function (y, z) -> "$$"^(printseq z)^"$$") " " (x)
;;
