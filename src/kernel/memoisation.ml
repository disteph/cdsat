open Lib
open Formulae
open Sums
open Patricia
open SetConstructions

module type CollectImplemExt = sig
  include Collection.CollectImplem

    (* Comparison of collections *)
  val compare    : t->t->int

    (* Comparison of elements *)
  val compareE   : e->e->int

    (* sub false k1 k2 (Some limit)
       computes whether k1 is a subset of k2
       up to the element limit (excluded);
       replace (Some limit) with None if you want no limit.
       It answers Yes() or No.

       sub true... refines the answer No into the answer Almost(a)
       if k1 is almost a subset of k2, were it not for element a
       (necessarily smaller than the limit if there is one)
    *)
  val sub       : bool->t->t->e option->(unit,e) almost

    (* Computes the smallest element that is in one set 
       and not in the other *)
  val first_diff : t->t->(e option*bool)

end


module PATMapExt
  (Atom: AtomType)
  (F: FormulaImplem with type lit = Atom.t)
  (FSet: CollectImplemExt with type e = F.t)
  (ASet: CollectImplemExt with type e = Atom.t)
  (V: sig type values val vcompare:values->values->int end)
  = struct

    module D = struct
      type keys    =  ASet.t*FSet.t
      let kcompare (a1,f1)(a2,f2) =
	let c=ASet.compare a1 a2 in
	  if c==0 then FSet.compare f1 f2 else c
      include V
      type infos     = unit
      let info_build = empty_info_build
      let treeHCons  = false
    end

    module EASet = struct include ASet type keys=D.keys let tag(a,b)=a end
    module EFSet = struct include FSet type keys=D.keys let tag(a,b)=b end

    module UT=LexProduct(TypesFromCollect(EASet))(TypesFromCollect(EFSet))

    include PATMap(D)(UT)

    let sub = UT.sub ASet.sub FSet.sub
    let sup alm f f' = sub alm f' f

    let byes j x = x
    let bempty   = (ASet.empty,FSet.empty)
    let bsingleton j x = function
      | A(a) -> (ASet.add a ASet.empty,FSet.empty)
      | F(a) -> (ASet.empty,FSet.add a FSet.empty)
    let bunion (a,b)(a',b')=if ASet.is_empty a&&FSet.is_empty b then (a',b') else (a,b)
    (* let bunion (a,b)(a',b')=(ASet.union a a',FSet.union b b')  *)

    let find_sub alm (k1,k2) =
      let filter =function
	| F(f)->alm
	| A(a)->alm && (not (ASet.is_in (Atom.negation a) k1)) && not (FSet.is_in (F.build(Lit a)) k2)
      in
	find_su byes bsingleton bempty bunion (sub alm) true filter (fun _-> true) (k1,k2)

    let find_sup alm = find_su byes bsingleton bempty bunion (sup alm) false (fun _-> true) (fun _-> true) 
end
