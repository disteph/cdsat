(********************)
(* Model primitives *)
(********************)

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

module StandardArity : ArityType = struct
  type eigen = int
  type meta  = int
  type t   = int*int*(int list)
  let init = (0,0,[])
  let newEigen (n,m,l) = (n+1),(n+1,m,l)
  let newMeta  (n,m,l) = n,(n,m+1,n::l)
end

(* Basic module for delayed substitutions *)

(* module StandardDSubst(A:ArityType) = struct *)
(*   type terms = Eigen of A.eigen | Meta of A.meta *)

(*   type t = terms list  *)

(*   let init = [] *)
(*   let equal a b = (a=b) *)
(*   let compare = Pervasives.compare *)
(*   let print_in_fmt = failwith "NotImplemented" *)
(*   let id = failwith "NotImplemented" *)
(*   let clear = failwith "NotImplemented" *)

(*   module Arity = A *)
(*   let bind2eigen e l = (Eigen e)::l *)
(*   let bind2meta e l  = (Meta e)::l *)
(* end *)

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
  let proj a = a
  let lift a = a
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

      let goal_consistency a t sigma = match Cons.goal_consistency a t with
        | None    -> NoMore
        | Some a' -> Guard(a',sigma,fun _ -> NoMore)

    end

    module Structure(F:Kernel.Formulae.FormulaType with type lit=IAtom.Atom.t)
      = MyDecProc.Structure(F)
end
