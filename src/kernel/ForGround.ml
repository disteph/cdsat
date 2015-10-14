(********************************************)
(* Transitional functor for ground theories *)
(********************************************)

open Format

open Prop.Interfaces_theory
open Theories_register

(* Basic module for constraints, for ground theories *)

module EmptyConstraint : ConstraintType = struct
  type t = unit
  let topconstraint = ()
  let print_in_fmt fmt () = ()
  let proj a = a
  let lift _ a = a
  let compare a b = 0
  let meet a b = Some ()
end


(* Functor turning a ground theory into a proper theory.
Cannot treat sequents with meta-variables, obviously *)

module GTh2Th
  (WB: Combo.WhiteBoard)
  (PProj: sig 
    type formulae
    val asF: WB.DS.Term.datatype -> formulae
  end)
  (MDP: sig
    val solve : WB.DS.TSet.t -> WB.answer
  end) 
  : DecProc with type DS.formulae = PProj.formulae = struct

  module DS = struct
    include WB.DS
    include PProj
    let makes_sense _ _ = true
    module Constraint = EmptyConstraint
  end

  open DS

  let consistency a sigma =
    match MDP.solve a with
    | WB.Provable(_,b)    -> Guard(b,sigma,fun _ -> NoMore)
    | WB.NotProvable(rest,_) when HandlersMap.is_empty rest -> NoMore
    | _ -> failwith "Not all theories have had their say"

  let goal_consistency t a sigma =
    let nont = DS.Term.bC Top.Symbols.Neg [t] in
    match MDP.solve (TSet.add nont a) with
    | WB.Provable(_,a')   -> Guard((if TSet.mem nont a' then TSet.remove nont a' else a'),sigma,fun _ -> NoMore)
    | WB.NotProvable(rest,_) when HandlersMap.is_empty rest -> NoMore
    | _ -> failwith "Not all theories have had their say"

end
