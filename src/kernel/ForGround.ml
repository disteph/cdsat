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
    module Constraint = EmptyConstraint
  end

  open DS

  let consistency a sigma =
    (* print_endline(Dump.stringOf TSet.print_in_fmt a); *)
    match MDP.solve a with
    | WB.NotProvable _ -> (* print_endline "Cons"; *) NoMore
    | WB.Provable b    -> (* print_endline ("Prov"^Dump.stringOf TSet.print_in_fmt b); *) Guard(b,sigma,fun _ -> NoMore)

  let goal_consistency t a sigma =
    (* print_endline("Goal "^Dump.stringOf TSet.print_in_fmt a); *)
    let nont = DS.Term.bC Top.Symbol.Neg [t] in
    match MDP.solve (TSet.add nont a) with
    | WB.NotProvable _ -> (* print_endline "Cons"; *) NoMore
    | WB.Provable a'   -> (* print_endline ("Prov"^Dump.stringOf TSet.print_in_fmt a'); *)
      Guard((if TSet.mem nont a' then TSet.remove nont a' else a'),sigma,fun _ -> NoMore)

end
