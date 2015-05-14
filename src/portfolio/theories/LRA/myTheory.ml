open Flags
open Kernel

open Lib.Sums
open Interfaces_I
open LibSimplex
open EqAst
open Core.Num

module Sig    = ThSig_register.LRASig
module Atom   = MyAtom.Atom

module Structure(F:Formulae.FormulaType with type lit = Atom.t) = MyModel.Structure(F)

let names   = ["lra";"qflra"; "QF_LRA"]
let sugPlugin = None

module Consistency(ASet : CollectImplem with type e = Atom.t) = struct

  let list_of_aset = fun aset ->
    ASet.fold (fun elt l -> (elt :: l)) aset []

  let aset_of_list = fun l ->
    List.fold_left (fun aset elt -> ASet.add elt aset) ASet.empty l

  (* val consistency: ASet.t*constraints -> (ASet.t*constraints) option *)
  let consistency a =
    Dump.msg (Some(fun p->p "Procedure called on\n%a" ASet.print_in_fmt a)) None None;
    let list_of_a = list_of_aset a in
    let lres = ForPsyche.test_inconsistency list_of_a in
    let b = Core.fopt aset_of_list lres
    in
    (match b with 
    | None   -> Dump.msg (Some(fun p->p "Procedure finished with CONSISTENT with hypotheses.")) None None
    | Some x -> Dump.msg (Some(fun p->p "Procedure finished with INCONSISTENT with hypotheses\n %a" ASet.print_in_fmt x)) None None); 
    b


  (* val goal_consistency: ASet.t -> Atom.t -> constraints -> (ASet.t*constraints) option*)
  let goal_consistency a e =
    if ASet.is_in e a then Some (ASet.add e ASet.empty)
    else
      (Dump.msg (Some(fun p->p "Procedure called on goal %a under hypotheses\n%a" Atom.print_in_fmt e ASet.print_in_fmt a)) None None;
       let list_of_a = list_of_aset a in
       let e' = Atom.negation e in
       let lres = ForPsyche.test_goal_inconsistency list_of_a e' in
       let b = match Core.fopt aset_of_list lres with
	 | None   -> None
	 | Some c -> Some(if ASet.is_in e' c then ASet.remove e' c else c)
       in
       (match b with 
       | None   -> Dump.msg (Some(fun p->p "Procedure finished with CONSISTENT with hypotheses.")) None None
       | Some x -> Dump.msg (Some(fun p->p "Procedure finished with INCONSISTENT with hypotheses\n %a" ASet.print_in_fmt x)) None None); 
	 b)	

end

