open Formulae
open Collection
open Patricia
open Memoisation
open Map

type polarity = Pos | Neg | Und
module Pol  = Map.Make(Atom.Predicates)
type polmap = polarity Pol.t

(* This is the module type that specifies the FrontEnd to which a user
   has access *)

module type FrontEndType = sig

  module F: FormulaImplem
  module FSet: CollectImplem with type e = F.t
  module ASet: ACollectImplem

  module Form : sig
    type t = F.t
    val toString : F.t -> string
    val negation : F.t -> F.t
  end

  val polarity : polarity Pol.t -> F.t -> polarity

  module Seq : sig
    type t =
      | EntF  of ASet.t * F.t * FSet.t * FSet.t * polmap
      | EntUF of ASet.t * FSet.t * FSet.t * FSet.t * polmap
    val toString : t -> string
  end

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
     t is the type of answers that the user is trying to produce
     tomem will ever be inhabited by a function that tells the
     proof-search how to store a result
     tosearch will ever be inhabited by a function that tells the
     proof-search to remember a result instead of working on it again
  *)
  type t
  type tomem
  type tosearch


  type final = Success of PT.t | Fail of Seq.t
  type ('a,'b) local =  Local of 'a | Fake  of 'b

  type focusaction = 
    | Focus    of F.t*receive*focusaction option
    | Cut      of int*F.t*receive*receive*focusaction option
    | Polarise of Atom.Predicates.t*bool*receive
    | Get      of bool*bool*focusaction option
    | Search   of tosearch*receive*(focusaction option,ASet.t*FSet.t->focusaction option)sum
    | Restore  of focusaction option
  and sideaction = bool
  and reception = 
    | Accept
    | Refuse
    | Action of focusaction
  and receive = (final,bool*bool) local -> reception

  val accept:receive

  type newnode_exit = 
    | Exit   of reception
    | Mem    of tomem*reception
  type 'a notified = bool*'a*((final,bool*bool) local -> newnode_exit)*focusaction option

  type 'a output = (t,'a fakeoutput) local
  and  'a fakeoutput = 
    | Notify   of Seq.t*bool*  ('a notified -> 'a output)*'a
    | AskFocus of Seq.t*FSet.t*(focusaction -> 'a output)*'a
    | AskSide  of Seq.t*       (sideaction  -> 'a output)*'a
    | Stop     of bool*bool*   (unit        -> 'a output)

  val toString : t -> string

  module Memo
    (FSetExt: CollectImplemExt with type e = F.t    and type t=FSet.t)
    (ASetExt: CollectImplemExt with type e = Atom.t and type t=ASet.t)
    : sig
      val tomem          : tomem
      val search4success : tosearch
      val search4failure : tosearch
      val search4both    : tosearch
      val clear          : unit->unit
    end

end

module FrontEnd (F: FormulaImplem) (FSet: CollectImplem with type e = F.t) (ASet: ACollectImplem) = struct
  module F    = F
  module FSet = FSet
  module ASet = ASet
  module Form = PrintableFormula(F)

  (* Computes polarity of formula *)
  let polarity polar f = match F.reveal f with
    | Lit t -> let (b,s,tl) = Atom.reveal t in
	if b then try Pol.find s polar with _ -> Und 
	else let p = try Pol.find s polar with _ -> Und in
	  begin match p with
	    | Pos -> Neg
	    | Neg -> Pos
	    | Und -> Und
	  end
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
    | Focus    of F.t*receive*focusaction option
    | Cut      of int*F.t*receive*receive*focusaction option
    | Polarise of Atom.Predicates.t*bool*receive
    | Get      of bool*bool*focusaction option
    | Search   of tosearch*receive*(focusaction option,ASet.t*FSet.t->focusaction option)sum
    | Restore  of focusaction option
  and sideaction = bool
  and reception = 
    | Accept
    | Refuse
    | Action of focusaction
  and receive = (final,bool*bool) local -> reception

  type newnode_exit = 
    | Exit   of reception
    | Mem    of tomem*reception
  type 'a notified = bool*'a*((final,bool*bool) local -> newnode_exit)*focusaction option

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

      module MP=PATMapExt(F)(FSetExt)(ASetExt)(struct
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
	      let (k1,k2) = Seq.simplify s in
		begin match algo (k1,k2) !table with
		  | F _ -> incr count;
		      if !Flags.debug>0&&(!count mod Flags.every.(4) =0)
		      then (print_endline(string_of_int !count^"/"^string_of_int !newcount^" Recording "^(if b then "success" else "failure")^" for");
			    print_endline(ASet.toString k1));
		      table := MP.add (fun x _ ->x) (k1,k2) ans !table
		  | A a -> incr newcount;
		      if !Flags.debug>0&&(!newcount mod Flags.every.(5) =0)
		      then (print_endline(string_of_int !count^"/"^string_of_int !newcount^" Already know better "^(if b then "success" else "failure")^" than");
			    print_endline(ASet.toString k1);
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

      let clear () =
	print_endline("Successes had "^(string_of_int (MP.cardinal !tableS))^" entries");
	print_endline("Failures had "^(string_of_int (MP.cardinal !tableF))^" entries");
	count:=0;newcount:=0;tableS := MP.empty; tableF := MP.empty
    end

end
