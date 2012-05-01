open Collection;;

module Term =
  (struct
     (* 
      * A term is either a variable or a function symbol applied to arguments
      *)
     type term = V of string | C of string*(term list)


     (*
      * Syntactic equality test on two terms and two term lists
      *)
     let rec 
	 equalsT l = function
	     (V x,V y) when x=y -> true
	   | (V x,V y) -> 
	       begin
		 match l with
		     [] -> false
		   | (x',y')::l' when (x'=x && y'=y)-> true
		   | (x',y')::l' -> equalsT l' (V x,V y)
	       end
	   | (C(f,tl),C(g,ul)) when f=g -> equalsTl l (tl,ul)
	   | _ -> false
     and
	 equalsTl l = function
	     ([],[]) -> true
	   | (t::tl',u::ul') -> (equalsT l (t,u))&&(equalsTl l (tl',ul'))
	   | _ -> false

     let compare t t' = if (equalsT [] (t,t')) then 0 else 1

     let rec toString = function
	 V(a) -> a;
       | C(f, newtl) -> f^"( "^printtl(newtl)^")";
     and printtl = function
	 [] -> ""
       | t::[] -> toString(t);
       | t::l -> toString(t)^", "^printtl(l);

   end)
;;

include Term;;


module Atom =
  (struct
     type t = string*(term list)
     let equalsA l ((f,tl),(g,ul)) = (f=g) && (equalsTl l (tl,ul))
     let compare (f,tl) (g,ul) = if (equalsA [] ((f,tl),(g,ul))) then 0 else 1
     let toString (f,tl) = f^"( "^printtl(tl)^")"
   end:PrintableType with type t = string*(term list))
;;

type 'a posForm = 
    PosAtom of string*(term list) 
  | AndP of 'a*'a
  | OrP of 'a*'a;;
type 'a negForm = 
    NegAtom of string*(term list)
  | AndN of 'a*'a
  | OrN of 'a*'a;;
type 'a form = Pos of 'a posForm | Neg of 'a negForm;;


module type FormulaImplem = sig
  type t
  val reveal : t -> t form
  val build : t form -> t
end;;


module Formula =
  functor (F: FormulaImplem) ->
    (struct

	type t = F.t
	(*
	 * Syntactic equality between formulae.
	 *)
	let rec
	    equals l (p, p') = match (F.reveal p,F.reveal p') with
		(Pos(PosAtom(f,tl)),Pos(PosAtom(g,ul))) -> (f=g) && (equalsTl l (tl,ul))
	      | (Pos(AndP(a,b)),Pos(AndP(a',b'))) -> (equals l (a,a')) && (equals l (b,b'))
	      | (Pos(OrP(a,b)),Pos(OrP(a',b'))) -> (equals l (a,a'))&&(equals l (b,b'))
	      |	(Neg(NegAtom(f,tl)),Neg(NegAtom(g,ul))) -> (f=g) && (equalsTl l (tl,ul))
	      | (Neg(AndN(a,b)),Neg(AndN(a',b'))) -> (equals l (a,a')) && (equals l (b,b'))
	      | (Neg(OrN(a,b)),Neg(OrN(a',b'))) -> (equals l (a,a'))&&(equals l (b,b'))
	      | _ -> false

	let compare p p' = if (equals [] (p,p')) then 0 else 1

	(* Displays a formula *)
	let rec toString f = match (F.reveal f) with
	    Neg(a) -> printformulaN(a)
	  | Pos(a) -> printformulaP(a)
	and
	    printformulaN = function
		NegAtom(s, tl) -> "\\non {"^s^"("^printtl(tl)^")}"
	      | AndN(f1,f2) -> "("^toString(f1)^") \\andN ("^toString(f2)^")"
	      | OrN(f1,f2) -> "("^toString(f1)^") \\orN ("^toString(f2)^")"
	and
	    printformulaP = function
		PosAtom(s, tl) -> s^"("^printtl(tl)^")"
	      | AndP(f1,f2) -> "("^toString(f1)^") \\andP ("^toString(f2)^")"
	      | OrP(f1,f2) -> "("^toString(f1)^") \\orP ("^toString(f2)^")"

     end:PrintableType with type t = F.t)
;;

module MyFormulaImplem = 
  (struct
     type t = Reveal of t form
     let reveal (Reveal a) = a
     let build a = (Reveal a)
   end : FormulaImplem)
;;
