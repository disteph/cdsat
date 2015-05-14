(********************************************)
(* Transitional functor for ground theories *)
(********************************************)

open Format

open Kernel
open Interfaces_basic
open Interfaces_theory

module type Type = sig

  include Theory.StructType

  module GConsistency(EAtom : sig
    type t
    val proj: t -> Atom.t
    val negation: t->t
  end): sig
    module ASet:CollectTrusted with type e = EAtom.t
    val consistency     :            ASet.t -> ASet.t option
    val goal_consistency: EAtom.t -> ASet.t -> ASet.t option
  end

end

(* Basic module for constraints, for ground theories *)

module EmptyConstraint : ConstraintType = struct
  type t = unit
  let topconstraint = ()
  let print_in_fmt fmt () = ()
  let projE a = a
  let liftE _ a = a
  let projM a = a
  let liftM _ a = a
  let compare a b = 0
  let meet a b = Some ()
end


(* Functor turning a ground theory into a proper theory.
Cannot treat sequents with meta-variables, obviously *)

module GTh2Th (MDP:Type) : Theory.Type = struct

  include MDP

  module Constraint = EmptyConstraint

  module Consistency(EAtom : sig
    type t
    val proj: t -> Atom.t
    val negation: t->t
  end) = struct

    module GCons = GConsistency(EAtom)

    module ASet = GCons.ASet

    let consistency a sigma = match GCons.consistency a with
      | None    -> NoMore
      | Some b  -> Guard(b,sigma,fun _ -> NoMore)

    let goal_consistency t a sigma = match GCons.goal_consistency t a with
      | None    -> NoMore
      | Some a' -> Guard(a',sigma,fun _ -> NoMore)

  end
end
