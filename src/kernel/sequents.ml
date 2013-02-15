(*
  This file contains the kernel's API to be used by a plugin
*)

open Lib
open Formulae
open Collection
open Sums
open Memoisation
open Map

type polarity = Pos | Neg | Und

(* This is the module type that specifies the FrontEnd to which a plugin
   has access *)

module type FrontEndType = sig

(* The kernel knows of an implementation of literals, formulae, collections of
formulae, and collections of atoms *)

  type litType
  type formulaType
  type fsetType
  type asetType

(* The kernel knows of an implementation extending that of formulae
with two extra functions for prettyprinting and for negation *)

  module Form : sig
    val toString : formulaType -> string
    val negation : formulaType -> formulaType
  end

(* The kernel knows, given a polarity assignment for literals, how to compute the polarity of a formula *)

  type polmap
  val emptypolmap : polmap
  val polarity : polmap -> formulaType -> polarity

(* The kernel has a module to implement the notion of sequent;
   the type of sequent states that there are two kinds:

   - focussed sequents of the form
   EntF(atomN, g, formP, formPSaved, polar)
   atomN are the atoms assumed to be true
   g is the formula in focus (i.e. being decomposed)
   formP are the positive formulae on which we could later place focus
   formPSaved are the positive formulae on which we have placed focus
   but we could focus on them again if need be
   polar is a assignment mapping atoms to polarities

   - unfocussed sequents of the form
   EntUF(atomN, delta, formP, formPSaved, polar)
   The arguments are the same as above, except there is no formula in
   focus; instead, there is a set of formulae delta: the formulae to
   be decomposed into atomic elements without creating backtrack
   points along the way

   simplify takes a sequent and outputs the set of atoms and the set
   of positive formulae (union of formP and formPSaved) 

   toString does the prettyprinting 
*)


  module Seq : sig
    type t =
      | EntF  of asetType * formulaType * fsetType * fsetType * polmap
      | EntUF of asetType * fsetType * fsetType * fsetType * polmap
    val simplify : t -> asetType*fsetType
    val toString : t -> string
  end

(* PT is the module implementing proof-trees:
   proof-trees are values of type t

   reveal turns a proof-tree into something that can be pattern-matched
   does the tree have 0 premisses, 1 premisse, 2 premisses ?

   conclusion gives the sequent labelling the root node of the proof-tree

   toString does pretty-printing
*)

  module PT : sig
    type ('a,'b) pt = 
      | Axiom  of 'b 
      | OnePre of 'b*'a 
      | TwoPre of 'b*'a*'a
    type t
    val reveal : t -> (t, Seq.t) pt
    val toString : t -> string
    val conclusion : t -> Seq.t
  end

  (* Three abstract types:
     t is the type of answers that a plugin is trying to produce
     tomem will ever be inhabited by a function that tells the
     proof-search how to store a result
     tosearch will ever be inhabited by a function that tells the
     proof-search to remember a result instead of working on it again
  *)
  type t
  type tomem
  type tosearch

  (* final is the plugin-readable version of type t:
     a final answer is either
     - a success of proof-search, with a proof-tree
     - a failure of proof-search (carrying the sequent for which no proof was found)

     local will be used as the type of intermediary outputs of the
     proof-search engine. Intuitively, such an output is
     - either a final answer embedded into that type with Local
     - or a state of the proof-search that is not yet finished,
       embedded into that type by Fake
  *)

  type final = Success of PT.t | Fail of Seq.t
  type ('a,'b) local =  Local of 'a | Fake  of 'b
    
  (* Now comes the heart of the API: 
     the actions that a plugin can order to kernel to trigger
     computation
  *)

  (* focusaction: list of actions that can be instructed by plugin
     upon reception of AskFocus signal

     1) Focus(toFocus,inter_fun,l)
     - place formula toFocus in focus
     - when I get back the result of doing this, apply inter_fun to it
     to see whether I accept that result or not or I prefer to do
     another action
     - if the result of this focusing fails, then do not ask me for my
       next instruction but just do l (optional argument)

     2) Cut(i,toCut, inter_fun1, inter_fun2,l)
     - make a cut_i on formula toCut
     - when I get back the result from the left branch, apply inter_fun1 to it
     to see whether I accept that result or not or I prefer to do
     another action
     - when I get back the result from the right branch, apply inter_fun2 to it
     to see whether I accept that result or not or I prefer to do
     another action
     - if the result of this cut fails, then do not ask me for my
       next instruction but just do l (optional argument)

     3) ConsistencyCheck(inter_fun,l)
     - check the consistency of the set of atoms
     - when I get back the result, apply inter_fun to it to see
     whether I accept that result or not or I prefer to do another
     action
     - if the result of this cut fails, then do not ask me for my
       next instruction but just do l (optional argument)

     3) Polarise(l,b, inter_fun)
     - make the polarity of l Neg (if b) or Pos (if not b)
     - when I get back the result of doing this, apply inter_fun to it
     to see whether I accept that result or not or I prefer to do
     another action

     4) Get(b1,b2,l)
     - put my current goal on hold as if it was a success (if b1) or
       as if it was a failure (if not b1)
     - move to the next unresolved goal (if b2) or to the previous one
       (if not b2)
     - when you come back to the current goal, do not ask me for my
       next instruction but just do l (optional argument)

     5) Search(tosearch,inter_fun,A l)
     - search in your database in case we already have an exact answer,
       using function tosearch
     - in case there is an exact answer, apply inter_fun to it
     to see whether I accept that answer or not or I prefer to do
     another action
     - if no exact match is found, then do not ask me for my
       next instruction but just do l (optional argument)

     6) Search(tosearch,inter_fun,F f)
     - search in your database in case we already have an answer, even
       approximate, using function tosearch
     - in case there is an exact answer, apply inter_fun to it
     to see whether I accept that answer or not or I prefer to do
     another action
     - otherwise apply f to the potential approximate answers that
       have been found and perform the resulting action, if any

     7) Restore(l)
     - restore formPSaved (the formulae on which focus has already
       been placed) into the set of positive formulae on which focus
       can be placed

  *)

  type focusaction = 
    | Focus    of formulaType*receive*alt_action
    | Cut      of int*formulaType*receive*receive*alt_action
    | ConsistencyCheck of receive*alt_action
    | Polarise of litType*bool*receive
    | Get      of bool*bool*alt_action
    | Search   of tosearch*receive*(alt_action,asetType*fsetType->alt_action)sum
    | Restore  of alt_action
  and sideaction = bool
  and reception = 
    | Accept
    | Refuse
    | Action of focusaction
  and receive = (final,bool*bool) local -> reception
  and alt_action = unit->(focusaction option)

  (* A function to systematically accept answers *)

  val accept:receive

  (* 'a notified = the input that plugin must provide
     upon reception of Notify signal (a new node has been reached)
     so as to resume computation, namely
     (b,data,exit_function,l)

     b: if b=true, plugin accepts defeat if kernel has noticed a loop
     since last focus (i.e. no progress has been made)

     data: new data with which that node will be labelled

     exit_function: tells the kernel what to do when it is about to
     leave that node with a certain answer, namely
     - just accept result, refuse it, or do alternative action
     (Exit(Accept/Refuse/l))
     - or memoise it with function f before doing one of the above
       (Mem(f,Accept/Refuse/l))

     l: next action to do for this newly created node is l (optional)
  *)

  type newnode_exit = 
    | Exit   of reception
    | Mem    of tomem*reception
  type 'a notified = bool*'a*((final,bool*bool) local -> newnode_exit)*alt_action


  (* Output of a call to the kernel:

     it can be either a final answer (type t)
     or a intermediary answer, i.e. a signal of one of the following 4 forms

     1) Notify(s,loop,action_anaylsis,data)
     - Kernel has reached a new focus point: a sequent (s) where
     asynchronous phase is finished
     - loop is true if no progress has been made since last focus
     point (kernel is proposing to fail)
     - data (of type 'a) is the label of the last focus point
     - action anaylysis is the kernel waiting for a new instruction to
     be given (kernel's computation is resumed if given a "coin" of
     type 'a notified)

     2) AskFocus(s,set,action_anaylsis,data)
     - Kernel is stuck at some focus point (previously notified to
     plugin), with sequent s
     - set is the set of positive formulae that haven't yet been tried
     for focus at this focus point If plugin instruct kernel to place
     focus on something, it should belong to set.
     - data (of type 'a) is the label of the last focus point
     - action anaylysis is the kernel waiting for a new instruction to
     be given (kernel's computation is resumed if given a "coin" of
     type focusaction)

     3) AskSide(s,action_anaylsis,data)
     - Kernel is stuck on sequent s in synchronous phase, having to
       choose a side for decomposing positive OR
     - data (of type 'a) is the label of the last focus point
     - action anaylysis is the kernel waiting for a new instruction to
     be given (kernel's computation is resumed if given a "coin" of
     type sideaction)

     4) Stop(b1,b2,action_analysis)
     - Kernel has finished investigating all branches, possibly having
       left some of them open
     - b1 indicates whether the result of proof-search is Succes or
       Failure, assuming that the remaining open branches can be
       closed as we pretended they could
     - b2 indicates whether we were moving to the right or to the left
     - action anaylysis is the kernel waiting for plugin's green light
     to change direction and come back to those open branches.
     (kernel's computation is resumed if given a "coin" of type unit)

  *)

  type 'a output = (t,'a fakeoutput) local
  and  'a fakeoutput = 
    | Notify   of Seq.t*bool*  ('a notified -> 'a output)*'a
    | AskFocus of Seq.t*fsetType*(focusaction -> 'a output)*'a
    | AskSide  of Seq.t*       (sideaction  -> 'a output)*'a
    | Stop     of bool*bool*   (unit        -> 'a output)

  val toString : t -> string

  (* API for Memoisation of results.
     
     As seen above, plugin can instruct kernel to interact with a
     memoisation table, by providing the kernel with the appropriate
     function to run.

     Provided that plugin gives a bit more information about its data
     structures, module Memo provides 4 memoisation handling
     functions, and one function to clear the memoisation table
  *)

  module Memo
    (FSetExt: CollectImplemExt with type e = formulaType    and type t=fsetType)
    (ASetExt: CollectImplemExt with type e = litType  and type t=asetType)
    : sig
      val tomem          : tomem
      val search4success : tosearch
      val search4failure : tosearch
      val search4both    : tosearch
      val report         : unit->unit
      val clear          : unit->unit
    end

end

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
      | Lit t -> (try Pol.find t polar with _ -> Und)
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

    let print_state = if !Flags.printrhs = true then Seq.toString else function
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
      | Polarise of F.lit*bool*receive
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
      | AskFocus of Seq.t*FSet.t*(focusaction -> 'a output)*'a
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
			table := MP.add (fun x _ ->x) k ans !table
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
	  count:=0;newcount:=0;tableS := MP.empty; tableF := MP.empty
      end

  end
