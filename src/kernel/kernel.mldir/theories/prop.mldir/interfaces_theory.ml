(******************************************************************)
(* This file contains the module types that specify how the kernel
interacts with Theory *)
(******************************************************************)

open Top
open Interfaces_basic
open Basic
open Variables

(* Module of constraints that meta-variables may be subject
   to. Constraints are produced when closing a branch, and are
   propagated through the other open branches of the proof.
*)

module type ConstraintType = sig
  type t
  val topconstraint:t
  val proj : t -> t
  val lift : World.t -> t -> t
  val meet : t -> t -> t option
  val compare : t -> t -> int
  val print_in_fmt : Format.formatter -> t -> unit
end

(* DataStructures that Theory must provide for Kernel *)

module type TheoryDSType = sig
  include Specs.GTheoryDSType
  val makes_sense: Term.t -> World.t -> bool
  type formulae
  val asF: Term.datatype -> formulae
  module Constraint: ConstraintType
end

(* Remarks for module type above:

   Kernel will not perfom a substitution every time it breaks a ForAll
   or a ThereExists; instead, it records in a datastructure called
   "Delayed substitution" (inhabitant of DSubst.t) what the bound
   variable is supposed to be substituted by.

   Namely, bound variables will be substituted by eigenvariables or
   meta-variables (depending on the quantifier).

   An inhabitant of Atom.t is an atom whose leaves are variables that
   are bound by a quantifier or by a delayed substitution.

   An inhabitant of IAtom.t is an instantiated atom, i.e. the result
   of actually applying a delayed substitution to an atom (as provided
   by function iatom_build).

   The theory must also be able to say whether an instantiated atom
   "makes sense" in a given world (its eigenvariables and
   metavariables must exist in this world).

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
  module DS : TheoryDSType 
  open DS
  val consistency     :           TSet.t -> (TSet.t,Constraint.t) stream
  val goal_consistency: Term.t -> TSet.t -> (TSet.t,Constraint.t) stream
end
