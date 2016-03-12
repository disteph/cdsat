(********************************************)
(* Transitional functor for ground theories *)
(********************************************)

open Format

open Kernel.Interfaces_I
open Theories
open ThSig_register
open AritiesDSubst


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
  let print_in_fmt fmt () = ()
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
                                     and  type delsubsts = StandardDSubst.t) option

  (* A model structure to be used for parsing, depending on an
  implementation of formulae, with a list of illustrative examples *)
  module Structure(F:Kernel.Formulae.FormulaType with type lit=Atom.t) :
  sig
    type t
    val st      : (Sig.symbol,t) structureType
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
      module DSubst     = StandardDSubst
      include Atom
      let reveal a = (a,DSubst.init)
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
