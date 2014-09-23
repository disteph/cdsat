(********************)
(* Model primitives *)
(********************)

open Format

open Kernel.Interfaces_I
open Theories
open ThSig_register

module PropStructure(F:Kernel.Formulae.FormulaType) = struct

  type 'a ite = 
    | Leaf of 'a
    | INode of F.t*('a ite)*('a ite)

  let rec map_aux f = function
    | Leaf a'      -> f a'
    | INode(c,d,e) -> INode(c,map_aux f d,map_aux f e)

  let map f    = map_aux (fun x -> Leaf(f x))

  let mmap f a = map_aux (fun y -> map (fun x->f x y) a)

  let rec bool_simpl = function
    | Leaf a      -> a
    | INode(c,d,e)-> F.orN(F.andP(c,bool_simpl d),F.andP(F.negation c,bool_simpl e))

  let symb_i (symb: [> PropSig.symbol]) (formulalist:F.t list) = 
    match symb, formulalist with
      | `True,[]      -> F.trueN
      | `False,[]     -> F.falseN
      | `Neg,[a]      -> F.negation a
      | `And,[a;b]    -> F.andP(a,b)
      | `Or,[a;b]     -> F.orN(a,b)
      | `Imp,[a;b]    -> F.orN(F.negation a,b)
      | `Xor,[a;b]    -> F.andP(F.orN(a,b),F.orN(F.negation a,F.negation b))
      | `EqProp,[a;b] -> F.andP(F.orN(F.negation a,b),F.orN(F.negation b,a))
      | `NEqProp,[a;b] -> F.orN(F.andP(a,F.negation b),F.andP(b,F.negation a))
      | `ITEProp,[a;b;c] -> bool_simpl(INode(a,Leaf b,Leaf c))
      | _             -> raise (ModelError "ModelError: Not the right number of arguments")

end


(* Basic module for arities *)

module StandardArity = struct

  type eigen = int
  type meta  = int

  module IntMap = Map.Make(struct
    type t = int
    let compare = Pervasives.compare
  end)

  (* an arity is a triple (n,m,l,l') where
     - n is the number of next eigenvariable
     - m is the number of next meta-variable
     - l is a map giving, for each eigenvariable, the number of
 meta-variables that existed when the eigenvariable was introduced
     - l' is a map giving, for each meta-variable, the number of
 eigenvariables that existed when the meta-variable was introduced
  *)

  type t   = {
    next_eigen : int;
    next_meta  : int;
    eigen_dependencies : int IntMap.t;
    meta_dependencies  : int IntMap.t
  }

  let init = {next_eigen = 0;
              next_meta  = 0; 
              eigen_dependencies = IntMap.empty;
              meta_dependencies  = IntMap.empty;
             }

  let liftE ar = {
    next_eigen = ar.next_eigen+1;
    next_meta  = ar.next_meta; 
    eigen_dependencies = IntMap.add ar.next_eigen ar.next_meta ar.eigen_dependencies;
    meta_dependencies  = ar.meta_dependencies;
  }

  let liftM ar = {
    next_eigen = ar.next_eigen;
    next_meta  = ar.next_meta+1; 
    eigen_dependencies = ar.eigen_dependencies;
    meta_dependencies  = IntMap.add ar.next_meta ar.next_eigen ar.meta_dependencies;
  }

  let projE ar = {
    next_eigen = ar.next_eigen-1;
    next_meta  = ar.next_meta; 
    eigen_dependencies = IntMap.remove (ar.next_eigen-1) ar.eigen_dependencies;
    meta_dependencies  = ar.meta_dependencies;
  }

  let projM ar = {
    next_eigen = ar.next_eigen;
    next_meta  = ar.next_meta-1; 
    eigen_dependencies = ar.eigen_dependencies;
    meta_dependencies  = IntMap.remove (ar.next_meta-1) ar.meta_dependencies;
  }

  let newEigen ar = ar.next_eigen,(liftE ar)
  let newMeta ar  = ar.next_meta,(liftM ar)

  let equal a1 a2 =
    (a1.next_eigen == a2.next_eigen)
    && (a1.next_meta == a2.next_meta)
    && (IntMap.equal (=) a1.eigen_dependencies a2.eigen_dependencies)
    && (IntMap.equal (=) a1.meta_dependencies a2.meta_dependencies)

  let prefix a1 a2 =
    (a1.next_eigen <= a2.next_eigen)
    && (a1.next_meta <= a2.next_meta)
    && (IntMap.for_all (fun ei nbm -> nbm == IntMap.find ei a2.eigen_dependencies) a1.eigen_dependencies)
    && (IntMap.for_all (fun mv nbe -> nbe == IntMap.find mv a2.meta_dependencies) a1.meta_dependencies)

end

(* Basic module for delayed substitutions *)

module StandardDSubst = struct

  exception DSubstException of string

  module Arity = StandardArity

  type aux = EmptySubst | ConsE of Arity.eigen*t | ConsM of Arity.meta*t
  and t = {reveal : aux; id : int}

  let reveal s = s.reveal
  let id s = s.id

  let equal s1 s2 = match (reveal s1,reveal s2) with
    | EmptySubst, EmptySubst                   -> true
    | ConsE(a,s1'), ConsE(b,s2') -> (s1'==s2') && (a == b)
    | ConsM(a,s1'), ConsM(b,s2') -> (s1'==s2') && (a == b)
    | _,_ -> false

  let hash s =
    match s.reveal with
    | EmptySubst -> 0
    | ConsE(a,s') -> 2 * a + id s'
    | ConsM(a,s') -> 3 * a + id s'

  module H = Hashtbl.Make(struct
    type t1 = t
    type t = t1
    let hash = hash
    let equal = equal
  end)

  let table = H.create 5003

  let substid = ref 0

  let build a =
    let f = {reveal = a; id = !substid} in
    try H.find table f
    with Not_found ->
      incr substid;
      H.add table f f;
      f

  let print_in_fmt fmt t =
    let rec aux fmt t = match t.reveal with
    | EmptySubst -> fprintf fmt ""
    | ConsE(a,s') -> (match s'.reveal with
      | EmptySubst -> fprintf fmt "%i" a
      | _ -> fprintf fmt "%i;%a" a aux s')
    | ConsM(a,s') -> (match s'.reveal with
      | EmptySubst -> fprintf fmt "?%i" a
      | _ -> fprintf fmt "?%i;%a" a aux s')
    in fprintf fmt "[%a]" aux t

  let init = build EmptySubst
  let compare s s' = Pervasives.compare (id s) (id s')

  let clear () = substid := 0;  H.clear table

  let bind2eigen e l = build (ConsE(e,l))
  let bind2meta e l  = build (ConsM(e,l))

  type freeVar = Eigen of Arity.eigen | Meta of Arity.meta

  let get i d =
    let rec aux j d = match reveal d with
    | EmptySubst -> raise (DSubstException 
      (Dump.toString
         (fun f -> f "Attempting to access bound variable %i in esubstitution %a" j print_in_fmt d)))
    | ConsE(k,d') -> if (j==0) then  Eigen k else aux (j-1) d'
    | ConsM(k,d') -> if (j==0) then  Meta k else aux (j-1) d'
    in
    aux i d
end



module DummyDSubst = struct
  type terms = unit
  type t = unit

  let init = ()
  let compare = Pervasives.compare
  let print_in_fmt fmt () = ()
  let id () = 0
  let clear () = ()

  module Arity = StandardArity
  let bind2eigen _ _ = ()
  let bind2meta _ _  = ()
end

(* Basic module for constraints, for ground theories *)

module EmptyConstraint : ConstraintType = struct
  type t = unit
  let topconstraint = ()
  let projE a = a
  let liftE a = a
  let projM a = a
  let liftM a = a
  let compare a b = 0
  let meet a b = Some ()
end

(* Module type for ground theories; i.e. theories not supporting
meta-variables *)

module type GThDecProc = sig

  (* Theory signature *)
  module Sig : SigType

  (* Implem of atoms and consistency checks, as required by kernel *)

  module Atom: AtomType

  module Consistency(ASet: CollectImplem with type e = Atom.t)
    :sig
      val consistency: ASet.t -> ASet.t option
      val goal_consistency: ASet.t -> Atom.t -> ASet.t option
    end

  (* Suggested plugin to be used for proof-search *)
  val sugPlugin:(module Plugins.Type with type literals  = Atom.t
                                     and  type iliterals = Atom.t
                                     and  type delsubsts = DummyDSubst.t) option

  (* A model structure to be used for parsing, depending on an
  implementation of formulae, with a list of illustrative examples *)
  module Structure(F:Kernel.Formulae.FormulaType with type lit=Atom.t) :
  sig
    type t
    val st      : (Sig.sort,Sig.symbol,t) structureType
    val toform  : t->F.t
    val examples: ((unit->F.t)*bool) list
  end
end

(* Functor turning a ground theory into a proper theory.
Cannot treat sequents with meta-variables, obviously *)

module GDecProc2DecProc (MyDecProc:GThDecProc) = struct

    module Sig        = MyDecProc.Sig
    module IAtom      = struct
      module Atom       = MyDecProc.Atom
      module DSubst     = DummyDSubst
      include Atom
      let reveal a = (a,())
      let build (a,_) = a
    end
    module Constraint = EmptyConstraint

    let sugPlugin = MyDecProc.sugPlugin

    module Consistency(ASet: CollectImplem with type e = IAtom.t) = struct

      module Cons = MyDecProc.Consistency(ASet)

      let consistency a sigma = match Cons.consistency a with
        | None    -> NoMore
        | Some b  -> Guard(b,sigma,fun _ -> NoMore)

      let goal_consistency t a sigma = match Cons.goal_consistency a t with
        | None    -> NoMore
        | Some a' -> Guard(a',sigma,fun _ -> NoMore)

    end

    module Structure(F:Kernel.Formulae.FormulaType with type lit=IAtom.Atom.t)
      = MyDecProc.Structure(F)
end
