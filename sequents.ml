open Formulae;;
open Collection;;
open Map;;

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

module type FrontEndType =
  (sig

     module F: FormulaImplem
     module FSet: CollectImplem with type e = F.t
     module ASet: CollectImplem with type e = Atom.t

     module Form :
     sig
       type t = F.t
       val toString : F.t -> string
       val negation : F.t -> F.t
     end

     val polarity : polarity Pol.t -> F.t -> polarity

     module Seq :
     sig
       type t =
           EntF of ASet.t * F.t * FSet.t * FSet.t * polmap
         | EntUF of ASet.t * FSet.t * FSet.t * FSet.t * FSet.t * polmap
       val toString : t -> string
     end

     module PT :
     sig
       type ('a,'b) pt = 
	 | Axiom of 'b 
	 | OnePre of 'b*'a 
	 | TwoPre of 'b*'a*'a
       type t
       val reveal : t -> (t, Seq.t) pt
       val toString : t -> string
     end

     module Ans :
     sig
       type t
       type final = Success of PT.t | Fail
       type ('a,'b) local =  Local of 'a | Fake  of 'b
       type focusaction = 
	 | Focus    of F.t*receive
	 | Cut      of int*F.t*receive*receive
	 | Polarise of string*bool*receive
	 | Get      of bool*bool
       and sideaction= bool
       and reception = 
	 | Accept
	 | Refuse
	 | Action of focusaction
       and receive = (final,bool*bool) local -> reception
       type fakeoutput = 
	 | AskFocus of Seq.t*(focusaction -> output)
	 | AskSide  of Seq.t*(sideaction  -> output)
	 | Stop     of bool*bool*(unit->output)
       and output = (t,fakeoutput) local
       val accept : receive
       val toString : t -> string
     end

   end)
;;

module FrontEnd =
  functor (F: FormulaImplem) ->
    functor (FSet: CollectImplem with type e = F.t) ->
      functor (ASet: CollectImplem with type e = Atom.t) ->
	(
	  struct

	    module F    = F
	    module FSet = FSet
	    module ASet = ASet

	    module Form = PrintableFormula(F)

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


	    module Seq = 
	      (struct
		 (* Type of sequents *)
		 type t = 
		     EntF of ASet.t*F.t*FSet.t*FSet.t*polmap
		   | EntUF of ASet.t*FSet.t*FSet.t*FSet.t*FSet.t*polmap
		       
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

	    module PT =
	      (struct
		 type ('a,'b) pt = 
		   | Axiom of 'b 
		   | OnePre of 'b*'a 
		   | TwoPre of 'b*'a*'a

		 (* Type of proof-trees *)
		 type t = Build of (t,Seq.t) pt

		 let reveal (Build a) = a
		 let build a = Build a

		 (* Displays prooftree *)
		 let rec toString pt = match reveal pt with
		   | OnePre (a,b) -> "\\infer {"^(Seq.toString a)^"}{"^toString(b)^"}";
		   | TwoPre (a,b,c) -> "\\infer {"^(Seq.toString a)^"}{"^toString(b)^" \\quad "^toString(c)^"}";
		   | Axiom (a) -> "\\infer {"^(Seq.toString a)^"}{}"         
	       end)

	    module Ans =
	      (struct
		 (* Type of final answers, known in interface FrontEndType. *)
		 type final = 
		   | Success of PT.t 
		   | Fail

		 (* Type of final answers, NOT known in interface FrontEndType. *)		       
		 type t = final

		 (* Generator of local answer types, either definitive answer or a fake answer *)
		 type ('a,'b) local = 
		   | Local of 'a
		   | Fake  of 'b
		 

		 (* Type of actions that user can perform to put more coins in the machine
		    focusaction: when user is asked for focus
		    sideaction:  when user is asked to choose sides
		    receive: user's reaction when he hears back the result, of type (final,bool*bool) local, from his chosen action
		 *)
		 type focusaction = 
		   | Focus    of F.t*receive
		   | Cut      of int*F.t*receive*receive
		   | Polarise of string*bool*receive
		   | Get      of bool*bool
		 and sideaction = bool
		 and reception = 
		   | Accept
		   | Refuse
		   | Action of focusaction
		 and receive = (final,bool*bool) local -> reception


		 (* Type of local answers, for output of search
		    AskFocus: new focus must be chosen to continue search
		    the sequent is provided, as well as the computation to perform once the user indicates next action
		    AskSide: side must be chosen to continue search
		    Stop: there is no [next/previous] branch after [Success/Failure] branch
		    first bool: [true/false] = [Success/Failure]
		    second bool: [true/false] = [next/previous]
		 *)
		 type output = (t,fakeoutput) local
		 and fakeoutput = 
		   | AskFocus of Seq.t*(focusaction -> output)
		   | AskSide  of Seq.t*(sideaction  -> output)
		   | Stop     of bool*bool*(unit->output)

		 let accept _ = Accept

		 (* Type of local answers, for internal use during search
		    In case of Fake,
		    first bool: true= faking success, false= faking failure
		    second bool: true= look for next branch, false= look for previous branch
		    third argument = computation at resume point; 
		    (it suffices to apply it to a continuation to trigger it) 
		 *)
		 type intern = (t,bool*bool*computations) local
		 and computations = Comp of ((intern -> output)->output)

		 (* Displays answer *)
		 let toString a = match a with
		     Success(p) -> "$$"^(PT.toString p)^"$$";
		   | Fail -> "\\textsf {FAIL} \\\\"

	       end)

	  end)
;;




