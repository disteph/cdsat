(* 
 * A term is either a variableor a function symbol applied to arguments
 *)
type term = V of string | C of string*(term list);;


(*
 * A formula is either positive, negative or undefined
 *)
type formula = Pos of posFormula | Neg of negFormula | Und of undFormula
and
  posFormula = 
    PosAtom of string*(term list) 
  | AndP of formula*formula
  | OrP of formula*formula
and
  negFormula = 
    NegAtom of string*(term list)
  | AndN of formula*formula
  | OrN of formula*formula
and
  undFormula = 
    PosAtomU of string*(term list)
  | NegAtomU of string*(term list)
  | AndU of formula*formula
  | OrU of formula*formula
;;


(* 
 * perp is the negation; it inverses the polarity of a formula
 *)
let rec
    perp: formula->formula = function
	Pos p -> Neg (perpP p)
      | Neg n -> Pos (perpN n)
      | Und n -> Und (perpU n)
and
    perpP:posFormula->negFormula = function
	PosAtom(f,tl) -> NegAtom(f,tl) 
      | AndP(a,b)     -> OrN(perp a,perp b)
      | OrP(a,b)      -> AndN(perp a,perp b)
and
    perpN:negFormula->posFormula = function
	NegAtom(f,tl) -> PosAtom(f,tl) 
      | AndN(a,b)     -> OrP(perp a,perp b)
      | OrN(a,b)      -> AndP(perp a,perp b)
and
    perpU:undFormula->undFormula = function
	PosAtomU(f,tl) -> NegAtomU(f,tl) 
      |	NegAtomU(f,tl) -> PosAtomU(f,tl) 
      | AndU(a,b)     -> OrU(perp a,perp b)
      | OrU(a,b)      -> AndU(perp a,perp b)
;;

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
;;


(*
 * Syntactic equality between formulae.
 *)
let rec
    equalsP l = function
	(PosAtom(f,tl),PosAtom(g,ul)) -> (f=g) && (equalsTl l (tl,ul))
      | (AndP(a,b),AndP(a',b')) -> (equalsF l (a,a')) && (equalsF l (b,b'))
      | (OrP(a,b),OrP(a',b')) -> (equalsF l (a,a'))&&(equalsF l (b,b'))
      | _ -> false
and
    equalsN l = function
	(NegAtom(f,tl),NegAtom(g,ul)) -> (f=g)&&(equalsTl l (tl,ul))
      | (AndN(a,b),AndN(a',b')) -> (equalsF l (a,a'))&&(equalsF l (b,b'))
      | (OrN(a,b),OrN(a',b')) -> (equalsF l (a,a'))&&(equalsF l (b,b'))
      | _ -> false
and
    equalsU l = function
	(PosAtomU(f,tl),PosAtomU(g,ul)) -> (f=g)&&(equalsTl l (tl,ul))
      |	(NegAtomU(f,tl),NegAtomU(g,ul)) -> (f=g)&&(equalsTl l (tl,ul))
      | (AndU(a,b),AndU(a',b')) -> (equalsF l (a,a'))&&(equalsF l (b,b'))
      | (OrU(a,b),OrU(a',b')) -> (equalsF l (a,a'))&&(equalsF l (b,b'))
      | _ -> false
and
    equalsF l = function
	(Pos p,Pos q) -> equalsP l (p,q)
      | (Neg n,Neg m) -> equalsN l (n,m)
      | (Und n,Und m) -> equalsU l (n,m)
      | _ -> false
;;

(*
Forces polarity of predicate 'at' to be positive (if pos is true) or negative (if pos is false)
*)

let rec
    forcepolP at pos = function
	PosAtom(f,tl) when f<>at -> PosAtom(f,tl)
      | AndP(a,b) -> AndP(forcepolF at pos a,forcepolF at pos b)
      | OrP(a,b) -> OrP(forcepolF at pos a,forcepolF at pos b)
      | _ -> failwith("Polarity of "^at^" is already fixed")
and
    forcepolN at pos = function
	NegAtom(f,tl) when f<>at -> NegAtom(f,tl)
      | AndN(a,b) -> AndN(forcepolF at pos a,forcepolF at pos b)
      | OrN(a,b) -> OrN(forcepolF at pos a,forcepolF at pos b)
      | _ -> failwith("Polarity of "^at^" is already fixed")
and
    forcepolU at pos = function
	NegAtomU(f,tl) when f<>at -> Und(NegAtomU(f,tl))
      | NegAtomU(f,tl) -> if pos then Neg(NegAtom(f,tl)) else Pos(PosAtom(f,tl))
      |	PosAtomU(f,tl) when f<>at -> Und(NegAtomU(f,tl))
      | PosAtomU(f,tl) -> if pos then Pos(PosAtom(f,tl)) else Neg(NegAtom(f,tl))
      | AndU(a,b) -> Und(AndU(forcepolF at pos a,forcepolF at pos b))
      | OrU(a,b) -> Und(OrU(forcepolF at pos a,forcepolF at pos b))
and
    forcepolF at pos = function
	Pos p -> Pos(forcepolP at pos p)
      |	Neg n -> Neg(forcepolN at pos n)
      |	Und n -> forcepolU at pos n
;;


(*
Forces polarity of undefined to be positive or negative
*)

let make_pos = function
    	NegAtomU(f,tl) -> PosAtom(f,tl)
      |	PosAtomU(f,tl) -> PosAtom(f,tl)
      | AndU(a,b) -> AndP(a,b)
      | OrU(a,b) -> OrP(a,b)
;;

let make_neg = function
    	NegAtomU(f,tl) -> NegAtom(f,tl)
      |	PosAtomU(f,tl) -> NegAtom(f,tl)
      | AndU(a,b) -> AndN(a,b)
      | OrU(a,b) -> OrN(a,b)
;;


(* 
PRETTY-PRINTING (to Latex syntax)
*)

(* Displays a term list *)
let rec printtl = function
    [] -> ""
  | t::[] -> printt(t);
  | t::l -> printt(t)^", "^printtl(l);
and 
    printt = function
	V(a) -> a;
      | C(f, newtl) -> f^"( "^printtl(newtl)^")";
;;


(* Displays a formula *)
let rec printformula = function
    Neg(a) -> printformulaN(a)
  | Pos(a) -> printformulaP(a)
  | Und(a) -> printformulaU(a)
and
    printformulaN = function
	NegAtom(s, tl) -> "\\non {"^s^"("^printtl(tl)^")}";
      | AndN(f1,f2) -> "("^printformula(f1)^") \\andN ("^printformula(f2)^")";
      | OrN(f1,f2) -> "("^printformula(f1)^") \\orN ("^printformula(f2)^")";
and
    printformulaP = function
	PosAtom(s, tl) -> s^"("^printtl(tl)^")";
      | AndP(f1,f2) -> "("^printformula(f1)^") \\andP ("^printformula(f2)^")";
      | OrP(f1,f2) -> "("^printformula(f1)^") \\orP ("^printformula(f2)^")";
and
    printformulaU = function
	PosAtomU(s, tl) -> s^"?("^printtl(tl)^")";
      |	NegAtomU(s, tl) -> "\\non {"^s^"?("^printtl(tl)^")}";
      | AndU(f1,f2) -> "("^printformula(f1)^") \\wedge ("^printformula(f2)^")";
      | OrU(f1,f2) -> "("^printformula(f1)^") \\vee ("^printformula(f2)^")";
;;
