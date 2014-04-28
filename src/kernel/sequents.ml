(*
  This file contains the kernel's API to be used by a plugin
*)

open Formulae
open Interfaces
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
      | OrN(f1,f2)  -> Neg
      | AndP(f1,f2) -> Pos
      | OrP(f1,f2)  -> Pos


    module Seq = struct
      (* Type of sequents *)
      type t = 
      |	EntF  of asetType*formulaType*fsetType*fsetType*polmap
      | EntUF of asetType*fsetType*fsetType*fsetType*polmap

      let interesting = function
	| EntF(atomN, g, formP, formPSaved, polar)      -> (atomN, formP::formPSaved::[])
	| EntUF(atomN, delta, formP, formPSaved, polar) -> (atomN, formP::formPSaved::delta::[])

      let simplify s = match interesting s with
        | (a,formP::formPSaved::l) -> (a,FSet.union formP formPSaved)
        | _ -> failwith("Not enough items in interesting")

      let subseq s1 s2 =
        match s1,s2 with
        | EntUF(atomN1, delta1, formP1, formPSaved1, _),EntUF(atomN2, delta2, formP2, formPSaved2,_)
          -> (ASet.subset atomN1 atomN2)&&(FSet.subset delta1 delta2)&&(FSet.subset (FSet.union formP1 formPSaved1) (FSet.union formP2 formPSaved2))
        | _,_ -> failwith("Incomparable sequents")
	  
      (* Displays sequent *)
      let toString_aux = function
	| EntF(atomsN, focused, formuP, formuPSaved,_)
	  -> " \\DerOSPos {"^(ASet.toString atomsN)^
	  "} {"^(Form.toString focused)^"}"^
	  "{"^(FSet.toString formuP)^" \\cdot "^(FSet.toString formuPSaved)^"}"
	| EntUF(atomsN, unfocused, formuP, formuPSaved,_)
	  -> " \\DerOSNeg {"^(ASet.toString atomsN)^
	  "} {"^(FSet.toString unfocused)^"}"^
	  "{"^(FSet.toString formuP)^" \\cdot "^(FSet.toString formuPSaved)^"}"

      let toString seq = if !Flags.printrhs = true then toString_aux seq else match seq with
        | EntUF(atms,_,_,_,_) -> ASet.toString atms
        | EntF(atms,_,_,_,_)  -> ASet.toString atms

    end

    module Proof:(ProofType with type seq = Seq.t) = struct
      type ('a,'b) pt = 
      | Axiom of 'b 
      | OnePre of 'b*'a 
      | TwoPre of 'b*'a*'a

      (* Type of proof-trees *)
      type t = Build of (t,Seq.t) pt
      type seq = Seq.t

      let reveal (Build a) = a
      let build a = Build a

      let zero seq = build(Axiom seq)
      let one seq pt = build(OnePre(seq,pt))
      let two seq pt1 pt2 = build(TwoPre(seq,pt1,pt2))

      (* let conclusion p = match reveal p with *)
      (*   | Axiom(s) -> s *)
      (*   | OnePre(s,b) -> s *)
      (*   | TwoPre(s,b,c) -> s *)

      (* Displays prooftree *)
      let rec toString pt = match reveal pt with
	| OnePre (a,b) -> "\\infer {"^(Seq.toString a)^"}{"^toString(b)^"}";
	| TwoPre (a,b,c) -> "\\infer {"^(Seq.toString a)^"}{"^toString(b)^" \\quad "^toString(c)^"}";
	| Axiom (a) -> "\\infer {"^(Seq.toString a)^"}{}"         
    end

    module NoProof:(ProofType with type seq = Seq.t) = struct
      type t   = unit
      type seq = Seq.t
      let zero seq = ()
      let one seq pt = ()
      let two seq pt1 pt2 = ()
      let toString () = ""
    end

    (* Type of final answers, known in interface FrontEndType. *)
    type final = Success of Seq.t*Proof.t | Fail of Seq.t

    (* Type of final answers, NOT known in interface FrontEndType. *)		       
    type t = final
    let reveal ans = ans
    let sequent = function 
      | Success(s,_) -> s
      | Fail s       -> s

    (* Generator of local answer types, either definitive answer or a fake answer *)
    type ('a,'b) local = Local of 'a | Fake  of 'b

    (* Type of actions that user can perform to put more coins in the machine
       focusaction: when user is asked for focus
       sideaction:  when user is asked to choose sides
       receive: user's reaction when he hears back the result, of type (final,bool*bool) local, from his chosen action
    *)

    type focusaction = 
    | Focus    of formulaType*receive*alt_action
    | Cut      of int*formulaType*receive*receive*alt_action
    | ConsistencyCheck of receive*alt_action
    | Polarise   of litType*receive
    | DePolarise of litType*receive
    | Get      of bool*bool*alt_action
    | Propose  of t
    | Restore  of alt_action
    and sideaction = bool
    and receive = (t,bool*bool) local -> unit
    and alt_action = unit->(focusaction option)

    type 'a notified = bool*'a*receive*alt_action

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
    | AskFocus of Seq.t*fsetType*bool*bool*(focusaction -> 'a output)*'a
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
      | Success(s,p) -> "$$"^(Proof.toString p)^"$$";
      | Fail(s) -> "\\textsf {FAIL} \\\\$$"^(Seq.toString s)^"$$"

  end
