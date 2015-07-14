(********************************************)
(* Transitional functor for ground theories *)
(********************************************)

open Format

open Top.Interfaces_basic

open Prop.Interfaces_theory

module type GTheoryDSType = sig
  module Term : Top.Specs.Term
  type formulaF
  val asF : Term.datatype -> formulaF
  module TSet : CollectTrusted with type e = Term.t
end

module type GDecProc = sig
  module DS : GTheoryDSType 
  open DS
  val consistency     :           TSet.t -> TSet.t option
  val goal_consistency: Term.t -> TSet.t -> TSet.t option
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

module GTh2Th (MDP:GDecProc) : DecProc with type DS.formulaF = MDP.DS.formulaF = struct

  module DS = struct
    include MDP.DS
    module Constraint = EmptyConstraint
  end

  open DS

  let consistency a sigma = match MDP.consistency a with
    | None    -> NoMore
    | Some b  -> Guard(b,sigma,fun _ -> NoMore)

  let goal_consistency t a sigma = match MDP.goal_consistency t a with
    | None    -> NoMore
    | Some a' -> Guard(a',sigma,fun _ -> NoMore)

end
