open Kernel
open Interfaces_I
open Formulae
open ThDecProc_tools
open MyAtom
open Lib.Patricia
open Lib.SetConstructions

module Sig    = ThSig_register.PropSig

module IAtom  = struct
  module Atom   = MyAtom.Atom
  module DSubst = StandardDSubst(StandardArity)
  include Atom (* TO BE MODIFIED *)
  let reveal a = (a,[])
  let build (a,_) = a
end

let sugPlugin = None

module D = struct
  type keys = int
  let kcompare = Pervasives.compare
  type values = Term.t
  let vcompare t1 t2 = Pervasives.compare (Term.id t1) (Term.id t2)
  type infos = keys m_infos
  let info_build = m_info_build kcompare
  let treeHCons = true
end

module Unifier = PATMap(D)(TypesFromHConsed(struct 
  type t = int 
  let id i = i
end))

module Unification = struct
  let mgu 
      (u:Unifier.t)
      (t:Term.fsymb * (Term.t list)) 
      (t':Term.fsymb * (Term.t list)) 
      : Unifier.t option
      = failwith "should implement mgu"
  let mguU (u:Unifier.t) (u':Unifier.t): Unifier.t option
      = failwith "should implement mgu of unifiers"
end


module Constraint = struct

  type t = int*Unifier.t

  let topconstraint = (0,Unifier.empty)
  let proj (n,u) = 
    let tn = Unifier.find n u in
    let newu = Unifier.map (fun  _ tm -> tm) (Unifier.remove n u) in
    (n-1,newu)

  let lift (n,u) = (n+1,u)
  let compare a b = failwith "should implement unifier comparator"
  let meet (i,u) (j,v) = if i!=j then None else 
      match Unification.mguU u v with
      | None-> None
      | Some u -> Some(i,u)

end


module Consistency(ASet: CollectImplem with type e = Atom.t)
  = struct    

    exception FoundIt of Atom.t*Constraint.t*(ASet.t,Constraint.t) stream
    
    let rec goal_consistency atomN t (i,u0) = 
      try
        ASet.fold
          (fun a alias -> 
            let newalias = ASet.remove a alias in
            let (b1,p1,l1) = Atom.reveal a in
            let (b2,p2,l2) = Atom.reveal t in
            (if b1=b2 
             then match Unification.mgu u0 (Atom.Predicates.reveal p1,l1)(Atom.Predicates.reveal p2,l2) with
             | Some u -> 
               raise (FoundIt(a,(i,u), goal_consistency newalias t))
             | None -> ());
            newalias)
          atomN 
          atomN;
        NoMore
      with
        FoundIt(a,u,f) -> Guard(ASet.add a ASet.empty,u,f)
        
    let rec consistency atomN = failwith "to be implemented"
      (* ASet.fold  *)
      (*   (function l -> function *)
      (*   | Some a -> Some a *)
      (*   | None   ->  *)
      (*     (match goal_consistency atomN (Atom.negation l) with *)
      (*     | None     -> None *)
      (*     | Some set -> Some (ASet.add l set) *)
      (*     ) *)
      (*   ) *)
      (*   atomN *)
      (*   None *)
  end



module Structure(F:FormulaType with type lit = Atom.t)
  = struct

    open Theories

    type t = F.t

    let lit (b, f, tl) = F.lit(Atom.bbuild (b, f, tl))

    let st = 
      { sigsymb_i 
	= (fun symb ->
	     let module TT = ThDecProc_tools.PropStructure(F) in
	       TT.symb_i symb);
	decsymb_i = 
	  (function
	  | `Prop -> fun (var:string) l ->
            if l <> [] 
            then raise (ModelError ("ModelError: Variable"^var^"shouldn't have any argument"))  
	    else lit(true,var,[])
	  | _     -> fun (var:string) -> raise (ModelError ("ModelError: Variable "^var^" not of expected type `Prop")));

        boundsymb_i = (fun db so -> raise (ModelError ("ModelError: cannot treat bound variables")));
        quantif_i = (fun db so sf -> raise (ModelError ("ModelError: cannot treat quantifiers")))

      }

    let toform a = a


    (* p(x) \/- !p(x) *)
    let f1() = 
      F.orN(
	lit(true,"p",[]),
	lit(false,"p",[])
      )

    (* p(x) \/+ !p(x) *)
    let f2() = 
      F.orP(
	lit(true,"p",[]),
	lit(false,"p",[])
      )

    (* !p(x) \/+ p(x) : infinite computation if proof-search is depth-first*)
    let f3() = 
      F.orP(
	lit(false,"p",[]),
	lit(true,"p",[])
      )

    (* (a \/- b) \/- (!a /\- !b) *)

    let f4() = 
      F.orN(
	F.orN(
	  lit(true,"a",[]),
	  lit(true,"b",[])
	),
	F.andN(
	  lit(false,"a",[]),
	  lit(false,"b",[])
	)
      )

    (* (a \/+ b) \/- (!a /\- !b) *)
    let f5() = 
      F.orN(
	F.orP(
	  lit(true,"a",[]),
	  lit(true,"b",[])
	),
	F.andN(
	  lit(false,"a",[]),
	  lit(false,"b",[])
	)
      )

    (* (!a \/+ !b) not provable - naive algorithm goes into infinite computation *)
    let f6() = 
      F.orP(
	lit(false,"a", []), 
	lit(false,"b", [])
      )

    (* (!a /\- !b) *)

    let f7() =
      F.andN(
	lit(false,"a", []), 
	lit(false,"b", [])
      )

    let f8()=
      F.orN(
	F.orP(
	  lit(false,"p",[]),
	  lit(false,"q",[])
	),
	F.andP(
	  lit(true,"p",[]),
	  lit(true,"q",[])
	)
      )

    (*
      let f9=
      andN(
      orP(
      lit(false,"p",[]),
      lit(true,"p",[])
      ),
      lit(true,"p",[])
      )
    *)

    let	examples = [(f1,true);(f2,true);(f3,true);(f4,true);(f5,true);(f6,false);(f7,false);(f8,true)]

  end

(* OLD CODE, useful for first-order 

   let goal_consistency atomN t =
   let (b,p,_) = Atom.reveal t in
   let rec filt_inspect filtered =
   if ASet.is_empty filtered then None
   else
   let (at,newfiltered) = ASet.next filtered in
   if Atom.equal at t then Some (ASet.add t ASet.empty)
   else filt_inspect newfiltered
   in filt_inspect (atomN)

*)
