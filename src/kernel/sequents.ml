(*
  This file contains the kernel's API to be used by a plugin
*)

open Lib
open Formulae
open Interfaces
open Sums
open Memoisation
open Map

module FrontEnd
  (Atom: AtomType)
  (F   : FormulaImplem with type lit = Atom.t) 
  (FSet: CollectImplem with type e = F.t) 
  (ASet: CollectImplem with type e = F.lit) 
  = struct

    type litType     = Atom.t
    type formulaType = F.t
    type fsetType    = FSet.t
    type asetType    = ASet.t

    module Form = PrintableFormula(Atom)(F)

    module Pol  = Map.Make(Atom)
    type polmap = polarity Pol.t
    let emptypolmap = Pol.empty

    (* Computes polarity of formula *)
    let polarity polar f = match F.reveal f with
      | Lit t  -> (try Pol.find t polar with _ -> Und)
      | TrueP  -> Pos
      | TrueN  -> Neg
      | FalseP -> Pos
      | FalseN -> Neg
      | AndN(f1,f2) -> Neg
      | OrN(f1,f2) -> Neg
      | AndP(f1,f2) -> Pos
      | OrP(f1,f2) -> Pos


    module Seq = struct
      (* Type of sequents *)
      type t = 
	|	EntF  of ASet.t*F.t   *FSet.t*FSet.t*polmap
	| EntUF of ASet.t*FSet.t*FSet.t*FSet.t*polmap

      let interesting = function
	| EntF(atomN, g, formP, formPSaved, polar)      -> (atomN, formP::formPSaved::[])
	| EntUF(atomN, delta, formP, formPSaved, polar) -> (atomN, formP::formPSaved::[])

      let simplify s =
	match interesting s with
	  | (a,formP::formPSaved::l) -> (a,FSet.union formP formPSaved)
	  | _ -> failwith("Not enough items in interesting")
	      
      (* Displays sequent *)
      let toString = function
	| EntF(atomsN, focused, formuP, formuPSaved,_)
	  -> " \\DerOSPos {"^(ASet.toString atomsN)^
	    "} {"^(Form.toString focused)^"}"^
	      "{"^(FSet.toString formuP)^" \\cdot "^(FSet.toString formuPSaved)^"}"
	| EntUF(atomsN, unfocused, formuP, formuPSaved,_)
	  -> " \\DerOSNeg {"^(ASet.toString atomsN)^
	    "} {"^(FSet.toString unfocused)^"}"^
	      "{"^(FSet.toString formuP)^" \\cdot "^(FSet.toString formuPSaved)^"}"
    end

    let print_state seq = if !Flags.printrhs = true then Seq.toString seq else match seq with
      | Seq.EntUF(atms,_,_,_,_) -> ASet.toString atms
      | Seq.EntF(atms,_,_,_,_)  -> ASet.toString atms


    module PT = struct
      type ('a,'b) pt = 
	| Axiom of 'b 
	| OnePre of 'b*'a 
	| TwoPre of 'b*'a*'a

      (* Type of proof-trees *)
      type t = Build of (t,Seq.t) pt

      let reveal (Build a) = a
      let build a = Build a
      let conclusion p = match reveal p with
	| Axiom(s) -> s
	| OnePre(s,b) -> s
	| TwoPre(s,b,c) -> s

      (* Displays prooftree *)
      let rec toString pt = match reveal pt with
	| OnePre (a,b) -> "\\infer {"^(Seq.toString a)^"}{"^toString(b)^"}";
	| TwoPre (a,b,c) -> "\\infer {"^(Seq.toString a)^"}{"^toString(b)^" \\quad "^toString(c)^"}";
	| Axiom (a) -> "\\infer {"^(Seq.toString a)^"}{}"         
    end

    (* Type of final answers, known in interface FrontEndType. *)
    type final = Success of PT.t | Fail of Seq.t
    let sequent = function Success(pt)->PT.conclusion pt | Fail(s)->s

    (* Type of final answers, NOT known in interface FrontEndType. *)		       
    type t = final
    let reveal ans = ans

    (* Generator of local answer types, either definitive answer or a fake answer *)
    type ('a,'b) local = Local of 'a | Fake  of 'b

    type tosearch  = bool->Seq.t->(final,ASet.t*FSet.t)sum
    type tomem     = final->unit

    (* Type of actions that user can perform to put more coins in the machine
       focusaction: when user is asked for focus
       sideaction:  when user is asked to choose sides
       receive: user's reaction when he hears back the result, of type (final,bool*bool) local, from his chosen action
    *)

    type focusaction = 
      | Focus    of F.t*receive*alt_action
      | Cut      of int*F.t*receive*receive*alt_action
      | ConsistencyCheck of receive*alt_action
      | Polarise   of litType*receive
      | DePolarise of litType*receive
      | Get      of bool*bool*alt_action
      | Search   of tosearch*receive*(alt_action,ASet.t*FSet.t->alt_action)sum
      | Restore  of alt_action
    and sideaction = bool
    and reception = 
      | Accept
      | Refuse
      | Action of focusaction
    and receive = (final,bool*bool) local -> reception
    and alt_action = unit->(focusaction option)

    type newnode_exit = 
      | Exit   of reception
      | Mem    of tomem*reception
    type 'a notified = bool*'a*((final,bool*bool) local -> newnode_exit)*alt_action

    let accept _ = Accept


    (* Type of local answers, for output of search
       AskFocus: new focus must be chosen to continue search
       the sequent is provided, as well as the computation to perform once the user indicates next action
       AskSide: side must be chosen to continue search
       Stop: there is no [next/previous] branch after [Success/Failure] branch
       first bool: [true/false] = [Success/Failure]
       second bool: [true/false] = [next/previous]
    *)
    type 'a output = (t,'a fakeoutput) local
    and 'a fakeoutput = 
      | Notify   of Seq.t*bool*  ('a notified -> 'a output)*'a
      | AskFocus of Seq.t*FSet.t*bool*bool*(focusaction -> 'a output)*'a
      | AskSide  of Seq.t*       (sideaction  -> 'a output)*'a
      | Stop     of bool*bool*   (unit        -> 'a output)

    (* Type of local answers, for internal use during search
       In case of Fake,
       first bool: true= faking success, false= faking failure
       second bool: true= look for next branch, false= look for previous branch
       third argument = computation at resume point; 
       (it suffices to apply it to a continuation to trigger it) 
    *)
    type 'a intern = (t,bool*bool*('a computations)) local
    and 'a computations = Comp of (('a intern -> 'a output)-> 'a output)

    (* Displays answer *)
    let toString a = match a with
      | Success(p) -> "$$"^(PT.toString p)^"$$";
      | Fail(s) -> "\\textsf {FAIL} \\\\$$"^(Seq.toString s)^"$$"


    (* Now we provide tools for memoising the proof-search function.
       The user can ask the search to store a result by a reception
       Mem(f,...)    with f:tomem
       The user can ask the user to search for a previously
       obtained result by an action
       Search(g,...) with g:tosearch
       Since tomem and tosearch are abstract for the user, he can
       only inhabit them by the memoisation tools that we provide
       below.
       If the user wants to use them, he must provide more
       structure than just F, FSet, ASet:
       he must provide small extensions of FSet and ASet *)

    module Memo
      (FSetExt: CollectImplemExt with type e = F.t    and type t=FSet.t)
      (ASetExt: CollectImplemExt with type e = Atom.t and type t=ASet.t)
      = struct

	module MP=PATMapExt(Atom)(F)(FSetExt)(ASetExt)(struct
							 type values  = final
							 let vcompare = Pervasives.compare
						       end)
	let tableS = ref MP.empty
	let tableF = ref MP.empty
	  
	let count    = ref 0
	let newcount = ref 0

	let tomem ans = 
	  let (table,algo,b) = match ans with
	    | Success(pt)-> (tableS,MP.find_sub false,true)
	    | Fail(s)    -> (tableF,MP.find_sup false,false)
	  in match sequent ans with
	    | Seq.EntUF(_,delta,_,_,_) as s when (FSet.is_empty delta) ->
		let k = Seq.simplify s in
		  begin match algo k !table with
		    | F _ -> incr count;
			if !Flags.debug>0&&(!count mod Flags.every.(4) =0)
			then (print_endline(string_of_int !count^"/"^string_of_int !newcount^" Recording "^(if b then "success" else "failure")^" for");
			      print_endline(print_state s));
			table := MP.add k (fun _ ->ans) !table
		    | A a -> incr newcount;
			if !Flags.debug>0&&(!newcount mod Flags.every.(5) =0)
			then (print_endline(string_of_int !count^"/"^string_of_int !newcount^" Already know better "^(if b then "success" else "failure")^" than");
			      print_endline(print_state s);
			      let (j,_)=Seq.simplify(match a with Success pt->PT.conclusion pt |Fail d->d) in print_endline(ASet.toString j))
		  end
	    | _ -> failwith("Not a sequent to memoise!")


	let search4success b s  = MP.find_sub b (Seq.simplify s) !tableS
	let search4failure b s  = MP.find_sup b (Seq.simplify s) !tableF
	let search4both    _ s  = match search4success false s with
	  | A a -> A a
	  | _   -> match search4failure false s with
	      | A a -> A a
	      | v   -> v 

	let report() = 
	  print_endline("   Memoisation report:");
	  print_endline("Successes had "^(string_of_int (MP.cardinal !tableS))^" entries, "
			^"Failures had "^(string_of_int (MP.cardinal !tableF))^" entries")

	let clear () =
	  count:=0;newcount:=0;tableS := MP.empty; tableF := MP.empty;MP.clear();
      end

  end
