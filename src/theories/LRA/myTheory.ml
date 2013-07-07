open Flags
open Kernel

open Lib.Sums
open Interfaces
open LibSimplex
open EqAst
open Core.Num

module Sig    = ThSig_register.LRASig
module Atom   = MyAtom.Atom
module Structure(F:PrintableFormulaType with type lit = Atom.t) = MyModel.Structure(F)

let sugPlugin = None

module Consistency(ASet : CollectImplem with type e = Atom.t) = struct

  let list_of_aset = fun aset ->
    ASet.fold (fun elt l -> (elt :: l)) aset []

  let aset_of_list = fun l ->
    List.fold_left (fun aset elt -> ASet.add elt aset) ASet.empty l

  (* val consistency: ASet.t -> ASet.t option *)
  let consistency = fun a ->
    if !debug>0 then Dump.msg (Some("Procedure called on\n"^ASet.toString a)) None None;
    let list_of_a = list_of_aset a in
    let lres = ForPsyche.test_inconsistency list_of_a in
    let b =      Core.fopt aset_of_list lres
    in
      if !debug>0 then Dump.msg (Some("Procedure finished with "^(match b with None -> "CONSISTENT" | Some x -> "INCONSISTENT: "^ASet.toString x))) None None; 
      b


  (* val goal_consistency: ASet.t -> Atom.t -> ASet.t option*)
  let goal_consistency = fun a e ->
    if ASet.is_in e a then Some (ASet.add e ASet.empty)
    else
      (if !debug>0 then print_endline("Procedure called on goal "^Atom.toString e^" under hypotheses\n"^ASet.toString a);
       let list_of_a = list_of_aset a in
       let e' = Atom.negation e in
       let lres = ForPsyche.test_goal_inconsistency list_of_a e' in
       let b = match Core.fopt aset_of_list lres with
	 | None   -> None
	 | Some c -> Some(if ASet.is_in e' c then ASet.remove e' c else c)
       in
	 if !debug>0 then print_endline("Procedure finished with "^(match b with None -> "CONSISTENT with hypotheses" | Some x -> "INCONSISTENT with hypotheses: "^ASet.toString x)); 
	 b)	

end

