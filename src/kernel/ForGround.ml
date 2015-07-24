(********************************************)
(* Transitional functor for ground theories *)
(********************************************)

open Format

open Interfaces
open Prop.Interfaces_theory

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

module GTh2Th
  (WB: WhiteBoard)
  (MDP: sig
    val solve : WB.DS.TSet.t -> WB.answer
  end) 
  : DecProc with type DS.formulaF = WB.DS.formulaF = struct

  module DS = struct
    include WB.DS
    module Constraint = EmptyConstraint
  end

  open DS

  let consistency a sigma = match MDP.solve a with
    | WB.NotProvable _ -> NoMore
    | WB.Provable b    -> Guard(b,sigma,fun _ -> NoMore)

  let goal_consistency t a sigma = match MDP.solve a with
    | WB.NotProvable _ -> NoMore
    | WB.Provable a'   -> Guard(a',sigma,fun _ -> NoMore)

end
