open Formulae
open Collection
open Patricia
open SetConstructions

module type CollectImplemExt = sig
  include CollectImplem
    (* Comparison of collections *)
  val compare    : t->t->int
    (* Comparison of elements *)
  val compareE   : e->e->int
    (* Computes the smallest element *)
  val min        : t->e option
    (* Computes the difference set *)
  val diff       : t->t->t
    (* Computes the smallest atom that is in one set of atoms
       and not in the other *)
  val first_diff : t->t->(e option*bool)
end

module PATMapExt
  (F: FormulaImplem)
  (FSet: CollectImplemExt with type e = F.t)
  (ASet: CollectImplemExt with type e = Atom.t)
  (V: sig type values val vcompare:values->values->int end)
  = struct

    let subA alm k p limit =
      let diffA = ASet.diff k p in
      let treatA x = if alm&&ASet.is_empty (ASet.remove x diffA) then Almost x else No in
	match limit,ASet.min diffA with
	  | Some a, Some x when ASet.compareE x a<0 -> treatA x
	  | None  , Some x -> treatA x
	  | _     ,_       -> Yes()

    let subF alm k p limit =
      let diffF = FSet.diff k p in
      let treatF x = if alm&&FSet.is_empty (FSet.remove x diffF) then Almost x else No in
	match limit,FSet.min diffF with
	  | Some a, Some x when FSet.compareE x a<0 -> treatF x
	  | None  , Some x -> treatF x
	  | _     ,_       -> Yes()

    module D = struct
      type keys =  ASet.t*FSet.t
      include V
      type infos     = unit
      let info_build = empty_info_build
      let treeHCons  = false
    end

    module EASet = struct include ASet type keys=D.keys let tag(a,b)=a end
    module EFSet = struct include FSet type keys=D.keys let tag(a,b)=b end

    module UT=LexProduct(TypesFromCollect(EASet))(TypesFromCollect(EFSet))

    include PATMap(D)(UT)


    let sub = UT.sub subA subF
    let sup alm f f' = sub alm f' f

    let byes j x = x
    let bempty   = (ASet.empty,FSet.empty)
    let bsingleton j x = function
      | A(a) -> (ASet.add a ASet.empty,FSet.empty)
      | F(a) -> (ASet.empty,FSet.add a FSet.empty)
    let bunion (a,b)(a',b')=if ASet.is_empty a&&FSet.is_empty b then (a',b') else (a,b) 
      (*             let bunion (a,b)(a',b')=(ASet.union a a',FSet.union b b') *)

    let find_sub alm (k1,k2) =
      let filter =function
	| F(f)->alm
	| A(a)->alm && (not (ASet.is_in (Atom.negation a) k1)) && not (FSet.is_in (F.build(Lit a)) k2)
      in
	find_su (sub alm) true filter (fun _-> true) byes bempty bsingleton bunion (k1,k2)

    let find_sup alm = find_su (sup alm) false (fun _-> true) (fun _-> true) byes bempty bsingleton bunion
end
