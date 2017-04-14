(* This file contains the kernel's API to be used by a plugin *)
open Format

open Top
open Interfaces_basic
open Basic
open Specs
open Variables

open Interfaces_theory
open Literals
open Formulae
open Interfaces_plugin

module DblSet(TrustedSet: Collection)(PluginSet: CollectExtra with type e = TrustedSet.e)
  = (struct

    type e = TrustedSet.e
    type t = TrustedSet.t * PluginSet.t
    type ts = TrustedSet.t
    type ps = PluginSet.t

    let empty                    = (TrustedSet.empty, PluginSet.empty)
    let singleton e              = (TrustedSet.singleton e, PluginSet.singleton e)
    let add e (tt,pt)            = (TrustedSet.add e tt, PluginSet.add e pt)
    let remove e (tt,pt)         = (TrustedSet.remove e tt, PluginSet.remove e pt)
    let union (tt1,pt1)(tt2,pt2) = (TrustedSet.union tt1 tt2, PluginSet.union pt1 pt2)
    let inter (tt1,pt1)(tt2,pt2) = (TrustedSet.inter tt1 tt2, PluginSet.inter pt1 pt2)
    let diff (tt1,pt1)(tt2,pt2)  = (TrustedSet.diff tt1 tt2, PluginSet.diff pt1 pt2)

    let is_empty (t,_)         = TrustedSet.is_empty t
    let mem e (t,_)            = TrustedSet.mem e t
    let equal (t1,_) (t2,_)    = TrustedSet.equal t1 t2
    let subset (t1,_) (t2,_)   = TrustedSet.subset t1 t2
    let print_in_fmt fmt (t,_) = TrustedSet.print_in_fmt fmt t
    let fold f (tt,_)          = TrustedSet.fold f tt

    let next (tt,pt) = let (e,tt') = TrustedSet.next tt in (e,(tt',PluginSet.remove e pt))
    let forTrusted (tt,_) = tt
    let forPlugin  (_,pt) = pt
    let recons tt     = (tt,TrustedSet.fold PluginSet.add tt PluginSet.empty)

  end: CollectKernel with type e = TrustedSet.e
                     and  type e = PluginSet.e
                     and  type ts = TrustedSet.t
                     and  type ps = PluginSet.t)

module Make(PlDS: PlugDSType) = struct

  module Semantic = FormulaF.Make(PlDS.UF)
    
  module FrontEnd(DS: TheoryDSType with type formulae = PlDS.UF.t FormulaF.generic) = struct

    (* Abbreviations for Kernel *)
    open DS

    (* Visible outside Kernel *)
    type dsubsts     = DSubst.t
    type constraints = Constraint.t
    let print_in_fmtC = Constraint.print_in_fmt
    let print_in_fmtL = LitF.print_in_fmt ~print_atom:Term.print_of_id
                        
    module LitF_print = struct
      include LitF
      let print_in_fmt fmt lit =
        print_in_fmt ~print_atom:Term.print_of_id fmt lit
    end
                    
    module IForm = struct
      include Semantic
      let print_in_fmt ?(print_atom=Term.print_of_id) fmt lit =
        print_in_fmt ~print_atom fmt lit
    end
    module FSet = DblSet(MakeCollection(struct
                             include IForm
                             let print_in_fmt fmt = print_in_fmt fmt
                           end))(PlDS.UFSet)
    module ASet = DblSet(MakeCollection(LitF_print))(PlDS.UASet)

    let iatom_build d l =
      let b,atom = LitB.reveal l in
      let module M = Term.Homo(IdMon) in
      let get_in_subst intso = 
        let k,_  = IntSort.reveal intso in
        let fv,_ = DSubst.get k d in
        fv
      in
      let newatom = M.lift get_in_subst atom in
      if b then newatom else Term.bC Symbols.Neg [newatom]
 
    let rec propagate d f =
      match FormulaB.reveal f with
      | LitB l  -> asF(Terms.data(iatom_build d l))
      | TrueP   -> IForm.trueP
      | TrueN   -> IForm.trueN
      | FalseP  -> IForm.falseP
      | FalseN  -> IForm.falseN
      | AndN(f1, f2)   -> IForm.andN(propagate d f1, propagate d f2)
      | OrN(f1, f2)    -> IForm.orN(propagate d f1, propagate d f2)
      | AndP(f1, f2)   -> IForm.andP(propagate d f1, propagate d f2)
      | OrP(f1, f2)    -> IForm.orP(propagate d f1, propagate d f2) 
      | ForAllB(so,f)  -> IForm.forall(so,f,d)
      | ExistsB(so,f)  -> IForm.exists(so,f,d)

    let litF_as_term e =
      let b,index = LitF.reveal e in
      let atom = Term.term_of_id index in
      if b then atom else Term.bC Symbols.Neg [atom]

    let rec makes_senseF f w =
      match FormulaF.reveal f with
      | LitF l  -> makes_sense (litF_as_term l) w
      | TrueP | TrueN | FalseP | FalseN
        -> true
      | AndN(f1, f2) | OrN(f1, f2) | AndP(f1, f2) | OrP(f1, f2)
        -> makes_senseF f1 w && makes_senseF f1 w 
      | ForAllF(_,_,d) | ExistsF(_,_,d)
        -> let dw = DSubst.get_arity d in
           World.prefix dw w

    let asTSet (aset: ASet.t) : TSet.t = 
      ASet.fold (fun e -> TSet.add (litF_as_term e)) aset TSet.empty

    let asASet (tset: TSet.t) : ASet.t =
      TSet.fold
        (fun e aset ->
          match FormulaF.reveal(asF(Terms.data e)) with
          | LitF l -> ASet.add l aset
          | _ -> aset)
        tset
        ASet.empty

    (* Module of Polarities *)

    module Pol  = struct
      module PolMap = Map.Make(LitF)

      type t = polarity PolMap.t

      let empty = PolMap.empty

      let declarePos polar l =
        if PolMap.mem l polar then polar else
          PolMap.add l Pos (PolMap.add (LitF.negation l) Neg polar)

      let remove polar l =
        PolMap.remove l (PolMap.remove (LitF.negation l) polar)

      (* Computes polarity of instantiated literal *)
      let iatom polar ia = 
        try PolMap.find ia polar with _ -> Und

      (* Computes polarity of instantiated formula *)
      let form polar f = 
        match FormulaF.reveal f with
        | TrueP  -> Pos
        | TrueN  -> Neg
        | FalseP -> Pos
        | FalseN -> Neg
        | AndN(f1,f2) -> Neg
        | OrN(f1,f2)  -> Neg
        | AndP(f1,f2) -> Pos
        | OrP(f1,f2)  -> Pos
        | ForAllF(so,f,d)-> Neg
        | ExistsF(so,f,d)-> Pos
        | LitF t  -> iatom polar t
    end

    (* Module of Sequents *)
    type seqU = private SeqU
    type seqF = private SeqF
    type _ rhs =
      | U : FSet.t  -> seqU rhs
      | F : IForm.t -> seqF rhs
    type 'a seq = {
        lits : ASet.t;
        rhs  : 'a rhs;
        formP: FSet.t;
        formPSaved:FSet.t;
        polar : Pol.t;
        world : World.t }

    module Seq = struct

      type t = Seq : _ seq -> t [@@unboxed]

      let interesting (type a) (t : a seq) =
        t.lits,
        t.formP::t.formPSaved::(match t.rhs with U delta -> [delta] | F _ -> [])

      let forPlugin s = match interesting s with
        | (a,formP::formPSaved::l) -> (ASet.forPlugin a, PlDS.UFSet.union (FSet.forPlugin formP)(FSet.forPlugin formPSaved))
        | _ -> failwith "Not enough items in interesting"

      let subseq s1 s2 =
        let U delta1, U delta2 = s1.rhs,s2.rhs in
        (ASet.subset s1.lits s2.lits)
        &&(FSet.subset delta1 delta2)
        &&(FSet.subset (FSet.union s1.formP s1.formPSaved) (FSet.union s2.formP s2.formPSaved))
	  
      (* Displays sequent *)

      let print_in_fmt_aux fmt (type a) : a seq -> unit = function
          t ->
          match t.rhs with
	  | F focused
            -> fprintf fmt " \\DerOSPos {%a} {%a} {%a \\cdot %a}"
                 TSet.print_in_fmt (asTSet t.lits)
                 (fun fmt -> IForm.print_in_fmt fmt) focused
                 FSet.print_in_fmt t.formP
                 FSet.print_in_fmt t.formPSaved
	  | U unfocused
            -> fprintf fmt " \\DerOSNeg {%a} {%a} {%a \\cdot %a}"
                 TSet.print_in_fmt (asTSet t.lits)
                 FSet.print_in_fmt unfocused
                 FSet.print_in_fmt t.formP
                 FSet.print_in_fmt t.formPSaved

      let print_seq_in_fmt fmt (type a) : a seq -> unit = function
          seq -> 
          if !Flags.printrhs then print_in_fmt_aux fmt seq
          else fprintf fmt "%a\nin %a" ASet.print_in_fmt seq.lits World.print_in_fmt seq.world

      let print_in_fmt fmt (Seq t) = print_seq_in_fmt fmt t

    end

    (* Module of Proofs *)

    module Proof:(ProofType with type seq = Seq.t) = struct

      type ('a,'b) pt = 
      | Axiom of 'b 
      | OnePre of 'b*'a 
      | TwoPre of 'b*'a*'a

      (* Type of proof-trees *)
      type t = Build of (t,Seq.t) pt [@@unboxed]
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

    type 'a answer = Provable of 'a seq*Proof.t*Constraint.t | NotProvable of 'a seq

    let sequent = function
      | Provable(s,_,_) -> s
      | NotProvable s -> s

    (* Displays answer *)
    let print_in_fmt fmt = function
      | Provable(_,p,_) -> fprintf fmt "\\[%a\\]" Proof.print_in_fmt p;
      | NotProvable s   -> fprintf fmt "\\textsf {The following sequent is not provable} \\[%a\\]" Seq.print_seq_in_fmt s

    (* Generator of local answer types, either genuine answer or a fake answer *)
    type ('a,'b) local = Genuine of 'a | Fake  of 'b

    (* 'a intern is the type of local answers, for internal use during
       search 

       In both cases,

       1st argument says whether it is
       - a genuine answer, with appropriate data
       - or a fake one, with boolean b saying whether we go to the
         right (b=true) or to the left (b=false)

       Last argument is the computation at resume point; 
       (it suffices to apply it to a continuation to trigger it)

       In case of success (genuine or fake), we also produce the
    constraint produced so far.
    *)

    type 'a intern =
    | Success of (Seq.t*Proof.t , bool) local * Constraint.t * 'a computations
    | Fail    of (Seq.t,bool) local * 'a computations
    and 'a computations = bool -> Constraint.t -> ('a intern -> 'a) -> 'a


    (* Type of actions that user can perform to put more coins in the machine
       coin: when user is asked for focus
       sideaction:  when user is asked to choose sides
       receive: user's reaction when he hears back the result, of type (final,bool*bool) local, from his chosen action
    *)

    type 'a address = bool list -> 'a

    type 'a sideCoin  = bool*('a address * 'a address)

    type receive = seqU answer -> unit

    type 'a focusCoin = 
    | Focus    of IForm.t*('a address*'a address)*receive*('a alt_action)
    | Cut      of int*IForm.t*('a address*'a address)*receive*receive*('a alt_action)
    | ConsistencyCheck of ('a address)*receive*('a alt_action)
    | Polarise   of ASet.e*('a address)*receive
    | DePolarise of ASet.e*('a address)*receive
    | Get      of bool*bool*('a alt_action)
    | Propose  of seqU answer
    | Restore  of ('a address)*receive*('a alt_action)
    and 'a alt_action = unit -> ('a focusCoin option)

    type 'a notified = bool*('a address)*receive*('a alt_action)

    type 'a output = Jackpot of seqU answer | InsertCoin of 'a insertcoin
    and  'a insertcoin = 
    | Notify   of seqU seq*Constraint.t*bool*('a notified -> 'a output)*('a address)
    | AskFocus of seqU seq*Constraint.t*FSet.t*bool*bool*('a focusCoin -> 'a output)*('a address)
    | AskSide  of seqF seq*Constraint.t*('a sideCoin -> 'a output)*('a address)
    | Stop     of bool*bool*(unit -> 'a output)

  end

end
