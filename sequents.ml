open Zipper;;
include Zipper;;
open Formulae;;
open Collection;;

module Sequent =
  functor (F: FormulaImplem) ->
    functor (FSet: CollectImplem with type e = F.t) ->
      functor (ASet: CollectImplem with type e = Atom.t) ->
	(
	  struct

	    module Form = Formula(F)

	    type t = 
		EntF of ASet.t*F.t*FSet.t*FSet.t*FSet.t
	      | EntUF of ASet.t*FSet.t*FSet.t*FSet.t*FSet.t

	    (* Displays sequent *)
	    let rec toString = function
		EntF(atomsN, focused, formuP, formuPTried, formuPSaved)
		-> " \\DerOSPos {"^(ASet.toString atomsN)^
		  "} {"^(Form.toString focused)^"}"^
		  "{"^(FSet.toString formuP)^" \\cdot "^(FSet.toString formuPTried)^" \\cdot "^(FSet.toString formuPSaved)^"}"
	      | EntUF(atomsN, unfocused, formuP, formuPTried, formuPSaved)
		-> " \\DerOSNeg {"^(ASet.toString atomsN)^
		  "} {"^(FSet.toString unfocused)^"}"^
		    "{"^(FSet.toString formuP)^" \\cdot "^(FSet.toString formuPTried)^" \\cdot "^(FSet.toString formuPSaved)^"}"
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
	     * In case of fail, we indicate the points where we got tired (a zipper of computations that have been halted, using weak reduction of OCaml))
	     *)
	    type t = Success of PT.t | Fail of (((unit -> t)* Seq.t) zipper);;

	    (* Displays answer *)
	    let toString = function
		Success( p ) -> "$$"^(PT.toString p)^"$$";
	      | Fail(x) -> "\\textsf {FAIL} \\\\"^printzipper (function (y, z) -> "$$"^(Seq.toString z)^"$$") " " (x)

	  end)
;;
