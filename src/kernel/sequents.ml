(* This file contains the kernel's API to be used by a plugin *)
open Format

open Interfaces_I
open Formulae
open Interfaces_II
open Map

module FrontEnd
  (IAtom     : IAtomType)
  (Constraint: ConstraintType)
  (F   : FormExtraInfo with type lit = IAtom.Atom.t)
  (FSet: CollectImplem with type e = (F.t,IAtom.Atom.t) GForm.t*IAtom.DSubst.t)
  (ASet: CollectImplem with type e = IAtom.t) 
  = struct

    module Form = Formula(IAtom.Atom)(F)

    type ilit        = IAtom.t
    type fsetType    = FSet.t
    type asetType    = ASet.t

    type arities     = IAtom.DSubst.Arity.t
    type dsubsts     = IAtom.DSubst.t
    type constraints = Constraint.t

    module IForm = struct
      type t = Form.t*dsubsts
      let print_in_fmt    = Form.iprint_in_fmt IAtom.DSubst.print_in_fmt
      let negation (f,tl) = (Form.negation f,tl)
    end

    module MyIAtomNeg = IAtomNeg(IAtom)
    let anegation = MyIAtomNeg.negation

    module Pol  = Map.Make(IAtom)
    type polmap = polarity Pol.t
    let emptypolmap = Pol.empty

    (* Computes polarity of formula *)
    let apolarity polar ia = 
      try Pol.find ia polar with _ -> Und

    let fpolarity polar (f,tl) = 
      match GForm.reveal f with
      | TrueP  -> Pos
      | TrueN  -> Neg
      | FalseP -> Pos
      | FalseN -> Neg
      | AndN(f1,f2) -> Neg
      | OrN(f1,f2)  -> Neg
      | AndP(f1,f2) -> Pos
      | OrP(f1,f2)  -> Pos
      | ForAll f    -> Neg
      | Exists f    -> Pos
      | Lit t  -> apolarity polar (IAtom.build(t,tl))


    module Seq = struct
      (* Type of sequents *)
      type t = 
      |	EntF  of asetType*IForm.t*fsetType*fsetType*polmap*arities
      | EntUF of asetType*fsetType*fsetType*fsetType*polmap*arities

      let interesting = function
	| EntF(atomN, g, formP, formPSaved, polar,ar)      -> (atomN, formP::formPSaved::[])
	| EntUF(atomN, delta, formP, formPSaved, polar,ar) -> (atomN, formP::formPSaved::delta::[])

      let simplify s = match interesting s with
        | (a,formP::formPSaved::l) -> (a,FSet.union formP formPSaved)
        | _ -> failwith("Not enough items in interesting")

      let subseq s1 s2 =
        match s1,s2 with
        | EntUF(atomN1, delta1, formP1, formPSaved1,_,_),EntUF(atomN2, delta2, formP2, formPSaved2,_,_)
          -> (ASet.subset atomN1 atomN2)&&(FSet.subset delta1 delta2)&&(FSet.subset (FSet.union formP1 formPSaved1) (FSet.union formP2 formPSaved2))
        | _,_ -> failwith("Incomparable sequents")
	  
      (* Displays sequent *)

      let print_in_fmt_aux fmt = function
	| EntF(atomsN, focused, formuP, formuPSaved,_,_)
	  -> fprintf fmt " \\DerOSPos {%a} {%a} {%a \\cdot %a}"
          ASet.print_in_fmt atomsN
          IForm.print_in_fmt focused
          FSet.print_in_fmt formuP
          FSet.print_in_fmt formuPSaved
	| EntUF(atomsN, unfocused, formuP, formuPSaved,_,_)
          -> fprintf fmt " \\DerOSNeg {%a} {%a} {%a \\cdot %a}"
          ASet.print_in_fmt atomsN
          FSet.print_in_fmt unfocused
          FSet.print_in_fmt formuP
          FSet.print_in_fmt formuPSaved

      let print_in_fmt fmt seq =
        if !Flags.printrhs = true then print_in_fmt_aux fmt seq
        else match seq with
        | EntUF(atms,_,_,_,_,_) -> ASet.print_in_fmt fmt atms
        | EntF(atms,_,_,_,_,_)  -> ASet.print_in_fmt fmt atms

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

      (* Displays prooftree *)
      let rec print_in_fmt fmt pt = match reveal pt with
	| Axiom (a) -> fprintf fmt "\\infer{%a}{}" Seq.print_in_fmt a
	| OnePre (a,b) -> fprintf fmt "\\infer{%a}{%a}" Seq.print_in_fmt a print_in_fmt b
	| TwoPre (a,b,c) -> fprintf fmt "\\infer{%a}{%a \\quad %a}" Seq.print_in_fmt a print_in_fmt b print_in_fmt c
    end

    module NoProof:(ProofType with type seq = Seq.t) = struct
      type t   = unit
      type seq = Seq.t
      let zero seq = ()
      let one seq pt = ()
      let two seq pt1 pt2 = ()
      let print_in_fmt fmt pt = ()
    end

    (* Type of final answers, private in interface FrontEndType. *)

    type t = Success of Seq.t*Proof.t*constraints | Fail of Seq.t

    let sequent = function
      | Success(s,_,_) -> s
      | Fail s -> s

    (* Displays answer *)
    let print_in_fmt fmt = function
      | Success(_,p,_) -> fprintf fmt "\\[%a\\]" Proof.print_in_fmt p;
      | Fail s         -> fprintf fmt "\\textsf {FAIL} \\[%a\\]" Seq.print_in_fmt s

    (* Generator of local answer types, either definitive answer or a fake answer *)
    type ('a,'b) local = Local of 'a | Fake  of 'b


    (* Type of local answers, for internal use during search
       In case of Fake,
       first bool: true= faking success, false= faking failure
       second bool: true= look for next branch, false= look for previous branch
       third argument = computation at resume point; 
       (it suffices to apply it to a continuation to trigger it) 
    *)

    type 'a intern =
    | ISuccess of 
        (Seq.t*Proof.t , bool) local * constraints * 'a computations
    | IFail    of (Seq.t,bool) local * 'a computations
    and 'a computations = bool -> constraints -> ('a intern -> 'a) -> 'a


    (* Type of actions that user can perform to put more coins in the machine
       focusaction: when user is asked for focus
       sideaction:  when user is asked to choose sides
       receive: user's reaction when he hears back the result, of type (final,bool*bool) local, from his chosen action
    *)

    type focusaction = 
    | Focus    of IForm.t*receive*alt_action
    | Cut      of int*IForm.t*receive*receive*alt_action
    | ConsistencyCheck of receive*alt_action
    | Polarise   of ilit*receive
    | DePolarise of ilit*receive
    | Get      of bool*bool*alt_action
    | Propose  of t
    | Restore  of alt_action
    and sideaction = bool
    and receive = t -> unit
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
    | Notify   of Seq.t*constraints*bool*  ('a notified -> 'a output)*'a
    | AskFocus of Seq.t*constraints*fsetType*bool*bool*(focusaction -> 'a output)*'a
    | AskSide  of Seq.t*constraints*       (sideaction  -> 'a output)*'a
    | Stop     of bool*bool*   (unit        -> 'a output)

  end
