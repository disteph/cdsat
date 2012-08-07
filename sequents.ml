open Formulae
open Collection
open Patricia
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
      | EntF of ASet.t * F.t * FSet.t * FSet.t * polmap
      | EntUF of ASet.t * FSet.t * FSet.t * FSet.t * polmap
    val toString : t -> string
  end

  module PT : sig
    type ('a,'b) pt = 
      | Axiom of 'b 
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
    | Focus    of F.t*receive
    | Cut      of int*F.t*receive*receive
    | Polarise of Atom.Predicates.t*bool*receive
    | Get      of bool*bool
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
  type newnode_ent = ((final,bool*bool) local -> newnode_exit)
  type 'a notified =
    | Entry  of 'a*newnode_ent
    | Search of tosearch*reception*'a*newnode_ent

  type 'a output = (t,'a fakeoutput) local
  and 'a fakeoutput = 
    | Notify   of Seq.t*('a notified -> 'a output)*'a
    | AskFocus of FSet.t*Seq.t*(focusaction -> 'a output)*'a
    | AskSide  of Seq.t*(sideaction  -> 'a output)*'a
    | Stop     of bool*bool*(unit->'a output)

  val toString : t -> string

  module type MemoType = sig
    val compareF    : F.t->F.t->int
    val minA        : ASet.t->Atom.t option
    val subsetA     : ASet.t->ASet.t->bool
    val diffA       : ASet.t->ASet.t->ASet.t
    val first_diffA : ASet.t->ASet.t->(Atom.t option*bool)
    val minF        : FSet.t->F.t option
    val subsetF     : FSet.t->FSet.t->bool
    val diffF       : FSet.t->FSet.t->FSet.t
    val first_diffF : FSet.t->FSet.t->(F.t option*bool)
  end

  module Memo :
    functor (M:MemoType) -> sig
      val tomem    : tomem
      val tosearch : tosearch
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
	      Pos -> Neg
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
	EntF of ASet.t*F.t*FSet.t*FSet.t*polmap
      | EntUF of ASet.t*FSet.t*FSet.t*FSet.t*polmap

    let interesting = function
      | EntF(atomN, g, formP, formPSaved, polar)      -> 	(atomN, formP::formPSaved::[])
      | EntUF(atomN, delta, formP, formPSaved, polar) ->	(atomN, formP::formPSaved::[])
	  
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

  type tosearch = Seq.t->final option
  type tomem    = final->unit

  (* Type of actions that user can perform to put more coins in the machine
     focusaction: when user is asked for focus
     sideaction:  when user is asked to choose sides
     receive: user's reaction when he hears back the result, of type (final,bool*bool) local, from his chosen action
  *)

  type focusaction = 
    | Focus    of F.t*receive
    | Cut      of int*F.t*receive*receive
    | Polarise of Atom.Predicates.t*bool*receive
    | Get      of bool*bool
  and sideaction = bool
  and reception = 
    | Accept
    | Refuse
    | Action of focusaction
  and receive = (final,bool*bool) local -> reception

  type newnode_exit = 
    | Exit   of reception
    | Mem    of tomem*reception
  type newnode_ent = ((final,bool*bool) local -> newnode_exit)
  type 'a notified =
    | Entry  of 'a*newnode_ent
    | Search of tosearch*reception*'a*newnode_ent

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
    | Notify   of Seq.t*('a notified -> 'a output)*'a
    | AskFocus of FSet.t*Seq.t*(focusaction -> 'a output)*'a
    | AskSide  of Seq.t*(sideaction  -> 'a output)*'a
    | Stop     of bool*bool*(unit->'a output)

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
     structure than just F, FSet, ASet: he must provide an
     implementation of the interface MemoType *)

  module type MemoType = sig

    (* Comparison of formulae *)
    val compareF    : F.t->F.t->int
      (* Computes the smallest atom of a set of atoms *)
    val minA        : ASet.t->Atom.t option
      (* Computes whether a set of atoms is a subset of another *)
    val subsetA     : ASet.t->ASet.t->bool
      (* Computes the difference between two sets of atoms *)
    val diffA       : ASet.t->ASet.t->ASet.t
      (* Computes the smallest atom that is in one set of atoms
	 and not in the other *)
    val first_diffA : ASet.t->ASet.t->(Atom.t option*bool)
      (* Computes the smallest formula of a set of formulas *)
    val minF        : FSet.t->F.t option
      (* Computes whether a set of formulas is a subset of another *)
    val subsetF     : FSet.t->FSet.t->bool
      (* Computes the difference between two sets of formulas *)
    val diffF       : FSet.t->FSet.t->FSet.t
      (* Computes the smallest formula that is in one set of formulas
	 and not in the other *)
    val first_diffF : FSet.t->FSet.t->(F.t option*bool)
  end

  module Memo(M:MemoType) = struct

    module UT = struct
      type keys   = ASet.t*FSet.t
      type common = keys
      let ccompare (a,b)(a',b')= if (a==a')&&(b==b') then 0 else 1
      let tag s = s
      type values = final
      let vcompare = Pervasives.compare
      type infos = unit
      let info_build = empty_info_build

      type branching = A of Atom.t | F of F.t
      let bcompare b1 b2 = match b1,b2 with
	| A(a),A(a') when a==a' -> 0
	| F(a),F(a') when a==a' -> 0
	| _ -> 1 

      let check (k,k') = function
	| A(a)-> ASet.is_in a k
	| F(a)-> FSet.is_in a k'
	    
      let match_prefix (k1,k2) (p1,p2) =
	let b = M.subsetA p1 k1 in
	  function
	    | A(a) -> b
		&& (match M.minA(M.diffA k1 p1) with
		      | Some x -> not((Atom.id x)<(Atom.id a))
		      | _        -> true)
	    | F(a) -> b&&(M.subsetF p2 k2)
		&& (match M.minF(M.diffF k2 p2) with
		      | Some x -> not(M.compareF x a<0)
		      | _        -> true)

      let disagree (a,b) (a',b') =
	(*print_endline("");
	  print_endline("a = "^(ASet.toString a));
	  print_endline("a'= "^(ASet.toString a'));
	  print_endline("");
	  print_endline("b "^(FSet.toString b));
	  print_endline("");
	  print_endline("b' "^(FSet.toString b'));*)
	match (M.first_diffA a a') with
	  | (Some d,c) -> (* print_endline("diff_foundA= "^(Atom.toString d));*)
	      ((ASet.inter a a',FSet.inter b b'),A(d),c)
	  | (None,_)   -> match (M.first_diffF b b') with
	      | (Some d,c)   -> (*	print_endline("diff_foundF= "^(Form.toString d)); *)
		  ((ASet.inter a a',FSet.inter b b'),F(d),c)
	      | (None,_)     -> if a==a' then failwith("Disagree called with two arguments that are equal")
		else failwith("Disagree called with two arguments that are not equal. strange")

      let treeHCons = false
    end

    module MP=PATMap(UT)

    let tableS = ref MP.empty
    let tableF = ref MP.empty
    let simplify s =
      match Seq.interesting s with
	| (a,formP::formPSaved::l) -> (a,FSet.union formP formPSaved)
	| _ -> failwith("Not enough items in interesting")
	    
    let count = ref 0

    let tomem ans = 
      let b = match ans with
	| Success(pt)-> (if !Flags.debug=2 then print_endline("Memoising Success "^(Seq.toString(PT.conclusion pt)));true)
	| Fail(s)    -> (if !Flags.debug=2 then print_endline("Memoising Failure "^(Seq.toString s));false)
      in 
      let table = if b then tableS else tableF in 
	match sequent ans with
	  | Seq.EntUF(_,delta,_,_,_) as s when (FSet.is_empty delta) ->
	      let (k1,k2) = simplify s in
		if not (MP.mem (k1,k2) !table) 
		then (incr count;
		      if !Flags.debug>0 then print_endline(string_of_int !count^" Recording "^(if b then "Success" else "Failure")^" "^(ASet.toString k1));
		      table := MP.add (fun x _ ->x) (k1,k2) ans !table)
		else if (!count mod 1000 =0) then print_endline("Déja là! "^string_of_int !count);
	  | _ -> failwith("Not a sequent to memoise!")

    let tosearch s  =
      let sub (a,b) (a',b') = (M.subsetA a a')&&(M.subsetF b b') in
 	match MP.find_sub sub (simplify s) !tableS with
	  | None ->
	      let sup (a,b) (a',b') = (M.subsetA a' a)&&(M.subsetF b' b) in
 		begin match MP.find_sup sup (simplify s) !tableF with
		  | None ->
		      if false then print_endline("Found nothing for "^(Seq.toString s));
		      None
		  | Some(g,h)->
		      if true then print_endline("Found previous Failure for "^(Seq.toString s));
		      Some h
		end
	  | Some(g,h)-> if true then print_endline("Found previous Success for "^(Seq.toString s));
	      Some h
  end

end
