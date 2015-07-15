(********************************************)
(* Transitional functor for ground theories *)
(********************************************)

open Format

open Kernel.Interfaces
open Kernel.Prop.Interfaces_theory

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
    val solve : WB.output -> WB.answer
  end) 
  : DecProc with type DS.formulaF = WB.DS.formulaF = struct

  module DS = struct
    include WB.DS
    module Constraint = EmptyConstraint
  end

  open DS

  let consistency a sigma = match MDP.solve(WB.consistency a) with
    | WB.NotProvable _ -> NoMore
    | WB.Provable b    -> Guard(b,sigma,fun _ -> NoMore)

  let goal_consistency t a sigma = match MDP.solve(WB.goal_consistency t a) with
    | WB.NotProvable _ -> NoMore
    | WB.Provable a'   -> Guard(a',sigma,fun _ -> NoMore)

end

    (* let goal_consistency t atomN = if TSet.mem t atomN then Provable *)
    (*   (TSet.add t TSet.empty) else NotProvable atomN *)
        
    (* let consistency atomN = TSet.fold (function l -> function | *)
    (*   Provable set as ans -> ans | _ -> (match goal_consistency *)
    (*   (Term.bC Symbol.Neg [l]) atomN with | Provable set -> Provable *)
    (*   (TSet.add l set) | ans -> ans ) ) atomN (NotProvable atomN) *)
