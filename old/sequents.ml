open Formulae;;
open Collection;;


(* HConsed strings ? not sure how that can be used.

   open Hashtbl;;
   let hash_cons_string =
   let h:(string,string) Hashtbl.t = Hashtbl.create 577 in
   fun s -> try Hashtbl.find h s
   with Not_found -> Hashtbl.add h s s; s
   end: HashedType where t = string)
   ;;
*)


type polarity = Pos | Neg | Und;;
module Pol  = Map.Make(String)
type polmap = polarity Pol.t


open Map;;

module Sequent =
  functor (F: FormulaImplem) ->
    functor (FSet: CollectImplem with type e = F.t) ->
      functor (ASet: CollectImplem with type e = Atom.t) ->
	(
	  struct

	    module Form = PrintableFormula(F)

	    type t = 
		EntF of ASet.t*F.t*FSet.t*FSet.t*polmap
	      | EntUF of ASet.t*FSet.t*FSet.t*FSet.t*FSet.t*polmap

	    (* Computes polarity of formula *)
	    let polarity polar f = match F.reveal f with
		Lit(true,s, tl) -> begin
		  try Pol.find s polar with _ -> Und 
		end
	      | Lit(false,s, tl) ->
		  let p = begin
		    try Pol.find s polar with _ -> Und
		  end
		  in 
		    begin
		      match p with
			  Pos -> Neg
			| Neg -> Pos
			| Und -> Und
		    end
	      | AndN(f1,f2) -> Neg
	      | OrN(f1,f2) -> Neg
	      | AndP(f1,f2) -> Pos
	      | OrP(f1,f2) -> Pos
		  
	    (* Displays sequent *)
	    let toString = function
		EntF(atomsN, focused, formuP, formuPSaved,_)
		-> " \\DerOSPos {"^(ASet.toString atomsN)^
		  "} {"^(Form.toString focused)^"}"^
		  "{"^(FSet.toString formuP)^" \\cdot "^(FSet.toString formuPSaved)^"}"
	      | EntUF(atomsN, unfocused, formuP, formuPTried, formuPSaved,_)
		-> " \\DerOSNeg {"^(ASet.toString atomsN)^
		  "} {"^(FSet.toString unfocused)^"}"^
		    "{"^(FSet.toString formuP)^" \\cdot "^(FSet.toString formuPSaved)^" \\cdot "^(FSet.toString formuPTried)^"}"

	  end)
;;

module ProofTree =
  functor (F: FormulaImplem) ->
    functor (FSet: CollectImplem with type e = F.t) ->
      functor (ASet: CollectImplem with type e = Atom.t) ->
	(
	  struct

	    module Seq = Sequent(F)(FSet)(ASet)

	    (* Type of proof-trees *)
	    type t = 
		Axiom of Seq.t 
	      | OnePre of Seq.t*t 
	      | TwoPre of Seq.t*t*t

	    (* Displays prooftree *)
	    let rec toString = function
		OnePre (a,b) -> "\\infer {"^(Seq.toString a)^"}{"^toString(b)^"}";
	      | TwoPre (a,b,c) -> "\\infer {"^(Seq.toString a)^"}{"^toString(b)^" \\quad "^toString(c)^"}";
	      | Axiom (a) -> "\\infer {"^(Seq.toString a)^"}{}"         

	  end)
;;

module Answer =
  functor (F: FormulaImplem) ->
    functor (FSet: CollectImplem with type e = F.t) ->
      functor (ASet: CollectImplem with type e = Atom.t) ->
	(
	  struct

	    module Seq = Sequent(F)(FSet)(ASet)
	    module PT = ProofTree(F)(FSet)(ASet)

	    (*
	     * Type of answers. 
	     *)
	    type t = 
		Success of PT.t 
	      | Fail

	    type local = 
		LocalSuccess of PT.t 
	      | LocalFail
	      | Fake of int*((local->t)-> t)

	    (* Displays answer *)
	    let toString = function
		Success(p) -> "$$"^(PT.toString p)^"$$";
	      | Fail -> "\\textsf {FAIL} \\\\"

	  end)
;;
