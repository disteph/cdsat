open Collection;;

module Term = struct
  (* 
   * A term is either a variable or a function symbol applied to arguments
   *)
  type term = V of string | C of string*(term list)

  let rec toString = function
      V(a) -> a;
    | C(f, newtl) -> f^printtl(newtl)
  and printrtl = function
      [] -> ""
    | t::[] -> toString(t);
    | t::l -> toString(t)^", "^printtl(l)
  and printtl = function
      [] -> ""
    | tl ->"("^printrtl(tl)^")"
	
end
;;

include Term;;


module Atom =
  (struct
     type t = bool*string*(term list)
     let toString = function
	 (true,s, tl) -> "{"^s^printtl(tl)^"}"
       | (false,s, tl) -> "\\non {"^s^"}"^printtl(tl)
   end:PrintableType with type t = bool*string*(term list))
;;



type 'a form =
    Lit of Atom.t
  | AndP of 'a*'a
  | OrP of 'a*'a
  | AndN of 'a*'a
  | OrN of 'a*'a;;


(* Interface for an implementation of formulae *)

module type FormulaImplem = sig
  type t
  val reveal : t -> t form
  val build : t form -> t
end;;


(* Generic code providing standard functions about formulae *)

module PrintableFormula =
  functor (F: FormulaImplem) -> struct

    type t = F.t
	
    (* Displays a formula *)
    let rec toString f = match F.reveal f with
	Lit(l) -> Atom.toString l
      | AndN(f1,f2) -> "("^toString(f1)^") \\andN ("^toString(f2)^")"
      | OrN(f1,f2) -> "("^toString(f1)^") \\orN ("^toString(f2)^")"
      | AndP(f1,f2) -> "("^toString(f1)^") \\andP ("^toString(f2)^")"
      | OrP(f1,f2) -> "("^toString(f1)^") \\orP ("^toString(f2)^")"


    (* Negates a formula *)
    let rec negation f = F.build(
      match F.reveal f with
	  Lit(b,s, tl) -> Lit(not b,s, tl)
	| AndN(f1,f2) -> OrP(negation f1,negation f2)
	| OrN(f1,f2) -> AndP(negation f1,negation f2)
	| AndP(f1,f2) -> OrN(negation f1,negation f2)
	| OrP(f1,f2) -> AndN(negation f1,negation f2)
    )
  end
;;

(* Default implementation for interface FormulaImplem *)

module MyFormulaImplem = 
  (struct
     type t = Reveal of t form
     let reveal (Reveal a) = a
     let build a = (Reveal a)
   end : FormulaImplem)
;;
