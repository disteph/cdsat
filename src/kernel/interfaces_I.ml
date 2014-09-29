(******************************************************************)
(* This file contains the module types that specify how the kernel
interacts with the other components of Psyche - Part I *)
(******************************************************************)

(* Generic interface for printable hconsed types *)

module type PHCons = sig
  type t
  val id: t -> int
  val print_in_fmt: Format.formatter -> t -> unit
  val clear: unit->unit
  val compare : t -> t -> int
end

(* Generic interface for collections *)

module type CollectImplem = sig
  type e
  type t
  val is_empty: t -> bool
  val is_in: e -> t -> bool
  val empty: t
  val add: e -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val subset: t -> t -> bool
  val remove: e -> t -> t
  val next: t -> e*t
  val fold : (e -> 'a -> 'a) -> t -> 'a -> 'a
  val print_in_fmt: Format.formatter -> t -> unit
end

(* Module type of sequents' arities: specifies types for
   eigenvariables, meta-variables, and for the datastructures
   recording which eigens and which metas have been created and how
   they depend on each other *)

module type ArityType = sig
  type eigen
  type meta
  type t
  val init     : t
  val newEigen : t -> eigen*t
  val newMeta  : t -> meta*t
end

(* Module type of Delayed Substitutions.

   Kernel will not perfom a substitution every time it breaks a ForAll
   or a ThereExists; instead, it records in a datastructure called
   "Delayed substitution" what the bound variable is supposed to be
   substituted by.

   Namely, bound variables will be "substituted by" eigenvariables or
   meta-variables (depending on the quantifier), hence the two
   functions bind2eigen and bind2meta.

   Therefore, the module must provide a sub-module defining arities,
   if only to define the types of eigens and the types of metas (and
   also because a substitution "lives in a certain arity").
*)

module type DSubstType = sig
  include PHCons
  module Arity: ArityType
  val init      : t
  val bind2eigen: Arity.eigen -> t -> t
  val bind2meta : Arity.meta  -> t -> t
end

(* Module type of Atoms. RAS *)

module type AtomType = sig
  include PHCons
  val negation: t -> t
end

(* Module type of "Instantiated atoms".

   Basically, an instantiated atom is an atom + a delayed
   substitution, but the combination may be more subtle than a simple
   pairing (a module of this type may decide to compute a version of
   the instantiated atom where the substitution is propagated into the
   atom, for instance).

   The module must provide the module for atoms, the module for
   delayed substitutions, and build / reveal functions to build or
   reveal the pairs.

   Indeed, the module is supposed to be hconsed, with an id for each
   instantiated atom and a comparison function that may take into
   account the semantics of the atoms.

   For instance in pure 1st-order logic (with bound variables being
   represented by de Bruijn's indices and delayed substitutions being
   represented by lists of terms), (f(#1,b) , [a]) and (f(a,#1) , [b])
   may get the same id, with the comparison function outputting 0.

   In arithmetic, ((#1 + #2) , [a;b]) and ((#2 + #1) , [a;b]) may (or
   may not) be identified by their id and the comparison function.

   The more instantiated atoms are identified at this level (in a way
   that is still sound w.r.t. the semantics), the more branches Psyche
   can closed by "syntactical means" (i.e. without calling a heavy
   decision procedure).
*)

module type IAtomType = sig
  module Atom   : AtomType
  module DSubst : DSubstType
  include PHCons
  val reveal : t -> Atom.t*DSubst.t
  val build  : (Atom.t*DSubst.t) -> t
end

(* Negation can be defined, given a module like above *)

module IAtomNeg(IAtom:IAtomType) = struct
  let negation ia = let (a,tl) = IAtom.reveal ia in IAtom.build (IAtom.Atom.negation a,tl)
end

(* Module of constraints that meta-variables may be subject
   to. Constraints are produced when closing a branch, and are
   propagated through the other open branches of the proof.
*)

module type ConstraintType = sig
  type t
  val topconstraint:t
  val projE : t -> t
  val liftE : t -> t
  val projM : t -> t
  val liftM : t -> t
  val meet : t -> t -> t option
  val compare : t -> t -> int
  val print_in_fmt : Format.formatter -> t -> unit
end

(* types for streams *)

type ('a,'b) gstream = NoMore | Guard of 'a*'b*('a,'b) stream
and ('a,'b) stream = 'b->('a,'b) gstream


(* Module of Theories + Decision Procedures 

   Each Theory comes with its notions of instantiated atoms (and its
   sub-modules of atoms, delayed substitutions, and arities) and
   constraints, and provides two functions (unfocussed, focussed)
   checking the consistency of some atoms with respect to the theory
   (in case of the focussed function, an atom -the goal- may play a
   special role).

   Each answer is a stream; if the input is consistent with the theory,
   the answer is the empty stream; if it is inconsistent, the stream is
   used to enumerate all possible ways to obtain the inconsistency; for
   each way to obtain it, the corresponding element of the stream
   provides
   - the subset of the input atoms that are used to derive the inconsistency
   - the constraint on meta-variables that allow the inconsistency

   Note that every time the stream is interrogated, it is given an
   input constraint that communicates the constraints that have been
   collected from the other closed branches of the proof-tree.
*)

module type DecProc = sig

  module IAtom     : IAtomType
  module Constraint: ConstraintType

  module Consistency(ASet: CollectImplem with type e = IAtom.t)
    :sig
      val consistency     : ASet.t -> (ASet.t,Constraint.t) stream
      val goal_consistency: IAtom.t -> ASet.t -> (ASet.t,Constraint.t) stream
    end

end
