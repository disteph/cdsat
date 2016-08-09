(********************************************)
(* Transitional functor for ground theories *)
(********************************************)

open Format

open General.Sums

open Top.Messages
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
              val solve : WB.DS.TSet.t -> (unsat WB.t, sat WB.t) sum
              val clear : unit -> unit
            end) 
       : (DecProc with type DS.formulae = PProj.formulae) = struct

  module DS = struct
    include WB.DS
    include PProj
    let makes_sense _ _ = true
    module Constraint = EmptyConstraint
  end

  open WB
  open DS

  let consistency a sigma =
    Dump.print ["ForGround",1] (fun p ->
        p "Calling theories on key set\n%a"
          TSet.print_in_fmt a);
    let res = MDP.solve a in
    MDP.clear();
    match res with
    | A(WB(_,Propa(b,Unsat)))
      -> if TSet.subset b a
         then Guard(b,sigma,fun _ -> NoMore)
         else
           ( Dump.print ["ForGround",1] (fun p ->
                 p "Theories came back with extra assumptions\n%a"
                   TSet.print_in_fmt (TSet.diff b a));
             failwith "ForGround1" )
    | F(WB(hdls,Sat _)) when HandlersMap.is_empty hdls
      -> NoMore
    | _ -> failwith "Combo: Not a definitive answer"

  let goal_consistency t a sigma =
    let nont = DS.Term.bC Top.Symbols.Neg [t] in
    Dump.print ["ForGround",1] (fun p ->
        p "Calling theories on key set\n%a and goal: %a"
          TSet.print_in_fmt a Term.print_in_fmt t);
    let a' = TSet.add nont a in
    let res = MDP.solve a' in
    MDP.clear();
    match res with
    | A(WB(_,Propa(b,Unsat)))
      -> if TSet.subset b a'
         then Guard((if TSet.mem nont b then TSet.remove nont b else b),sigma,fun _ -> NoMore)
         else
           ( Dump.print ["ForGround",1] (fun p ->
                 p "Theories came back with extra assumptions\n%a"
                   TSet.print_in_fmt (TSet.diff b a'));
             failwith "ForGround2" )
    | F(WB(hdls,Sat _)) when HandlersMap.is_empty hdls
      -> NoMore
    | _ -> failwith "Combo: Not a definitive answer"

end
