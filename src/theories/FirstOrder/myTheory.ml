open Format

open Kernel
open Interfaces_I
open Formulae
open ThDecProc_tools
open MyAtom
open Lib.Patricia
open Lib.SetConstructions

module Sig    = ThSig_register.FOSig

module IAtom  = struct
  module Atom   = MyAtom.Atom(struct
    type t = int
    let id i = i
    let print_in_fmt fmt i = fprintf fmt "%i" i
    let clear () = ()
    let compare = Pervasives.compare
  end)

  module DSubst = StandardDSubst

  module A = MyAtom.Atom(struct
    type t = DSubst.freeVar
    let id = function
      | DSubst.Eigen i -> 2*i
      | DSubst.Meta i  -> 2*i+1

    let print_in_fmt fmt = function
      | DSubst.Eigen i -> fprintf fmt "%i" i
      | DSubst.Meta i  -> fprintf fmt "?%i" i

    let clear () = ()
    let compare v v' = Pervasives.compare (id v) (id v')
  end)

  type t = { substituted: A.t; original: Atom.t*DSubst.t}

  let id s = A.id s.substituted

  let expose a = A.reveal a.substituted

  let print_in_fmt fmt s = A.print_in_fmt fmt s.substituted

  let clear = A.clear
  let compare v v' = A.compare v.substituted v'.substituted

  let reveal a = a.original

  let build (a,d) = 
    let rec propagate t = match Atom.MyTerm.reveal t with
      | Atom.MyTerm.V i     -> A.MyTerm.build(A.MyTerm.V(DSubst.get i d))
      | Atom.MyTerm.C(f,tl) -> A.MyTerm.build(A.MyTerm.C(f,List.map propagate tl))
    in
    let (b,p,tl) = Atom.reveal a in
    { substituted = A.build (b,p,List.map propagate tl);
      original = (a,d)}

end

module Term = IAtom.A.MyTerm

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
      = None (* failwith "should implement mgu" *)
  let mguU (u:Unifier.t) (u':Unifier.t): Unifier.t option
      = None (* failwith "should implement mgu of unifiers" *)
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


module Consistency(ASet: CollectImplem with type e = IAtom.t)
  = struct    

    exception FoundIt of IAtom.t*Constraint.t*(ASet.t,Constraint.t) stream
    
    let rec goal_consistency atomN t (i,u0) = 
      try
        ASet.fold
          (fun a alias -> 
            let newalias = ASet.remove a alias in
            let (b1,p1,l1) = IAtom.expose a in
            let (b2,p2,l2) = IAtom.expose t in
            (if b1=b2 
             then match Unification.mgu u0 (Predicates.reveal p1,l1)(Predicates.reveal p2,l2) with
             | Some u -> 
               raise (FoundIt(a,(i,u), goal_consistency newalias t))
             | None -> ());
            newalias)
          atomN 
          atomN;
        NoMore
      with
        FoundIt(a,u,f) -> Guard(ASet.add a ASet.empty,u,f)
        
    let rec consistency atomN (i,u0) = NoMore (* failwith "to be implemented" *)
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



module Structure(F:FormulaType with type lit = IAtom.Atom.t)
  = struct

    module PS = ThDecProc_tools.PropStructure(F)

    open Theories

    module BTerm = IAtom.Atom.MyTerm

    type t = Term of BTerm.t | Prop of F.t

    let lit (b, f, tl) = F.lit(IAtom.Atom.bbuild (b, f, tl))

    let toform = function
      | Prop f -> f
      | _      -> raise (ModelError "ModelError: trying to convert into a formula an expression that clearly is not one")

    let toterm = function
      | Term t -> t
      | _ -> raise (ModelError "ModelError: trying to convert into a term an expression that clearly is not one")

    let st = 
      { sigsymb_i = 
          (fun s l -> Prop (PS.symb_i s (List.map toform l))
	  );
	decsymb_i =
          (function
	  | `Prop -> fun (var:string) l -> 
	    Prop (lit (true,var,List.map toterm l))
	  | `Term -> fun (var:string) l -> 
	    Term (BTerm.build (BTerm.C (var,List.map toterm l)))
	  | _     -> fun (var:string) -> raise (ModelError ("ModelError: variable "^var^" not of expected type `Prop or `Term")));
        boundsymb_i = (fun db so -> Term (BTerm.build (BTerm.V db)));
        quantif_i   = (fun b so sf -> 
          let f = toform sf in
          Prop (if b then F.forall f else F.exists f))
      }

    let	examples = []

  end
