open Formulae
open Collection
open Patricia

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

    let subA alm diffA limit =
      let treatA x = if alm&&ASet.is_empty (ASet.remove x diffA) then Almost(A(x)) else No in
	match limit,ASet.min diffA with
	  | Some a, Some x when ASet.compareE x a<0 -> treatA x
	  | None  , Some x -> treatA x
	  | _     ,_       -> Yes()

    let subF alm diffF limit =
      let treatF x = if alm&&FSet.is_empty (FSet.remove x diffF) then Almost(F(x)) else No in
	match limit,FSet.min diffF with
	  | Some a, Some x when FSet.compareE x a<0 -> treatF x
	  | None  , Some x -> treatF x
	  | _     ,_       -> Yes()

    let sub alm (k1,k2) (p1,p2) =
      let aux b = match subA alm (ASet.diff k1 p1) None with
	| Yes _    -> subF alm (FSet.diff k2 p2) b
	| Almost(f)-> begin match subF false (FSet.diff k2 p2) b with
	    | Yes _ -> Almost f
	    | _     -> No
	  end
	| No       -> No
      in function
	| Some(A(a)) -> subA alm (ASet.diff k1 p1) (Some a)
	| Some(F(a)) -> aux (Some a)
	| None       -> aux None

    let sup alm (a,b) (a',b') = sub alm (a',b') (a,b)


    module UT = struct
      type keys   = ASet.t*FSet.t
      type common = keys

      let tag s   = s
      let ccompare (a,b)(a',b')=
	let c = ASet.compare a a' in
	  if c=0 then FSet.compare b b' else c

      include V

      type branching = (Atom.t,F.t)sum
      let bcompare b1 b2 = match b1,b2 with
	| A(a),A(a') -> ASet.compareE a a'
	| F(a),F(a') -> FSet.compareE a a'
	| A(a),F(a') -> -1 
	| F(a),A(a') -> 1 

      type infos     = unit
      let info_build = empty_info_build

      let check (k,k') = function
	| A(a)-> ASet.is_in a k
	| F(a)-> FSet.is_in a k'

      let match_prefix (k1,k2) (p1,p2) a = match sub false (k1,k2) (p1,p2) (Some a) , sup false (k1,k2) (p1,p2) (Some a) with 
	| Yes _, Yes _ -> true
	| _            -> false

      let disagree (a,b) (a',b') =
	match (ASet.first_diff a a') with
	  | (Some d,c) -> ((ASet.inter a a',FSet.inter b b'),A(d),c)
	  | (None,_)   -> match (FSet.first_diff b b') with
	      | (Some d,c)   -> ((ASet.inter a a',FSet.inter b b'),F(d),c)
	      | (None,_)     -> failwith("Disagree called with two arguments that are not equal")

      let treeHCons = false
    end

    include PATMap(UT)

    let byes j x = x
    let bempty   = (ASet.empty,FSet.empty)
    let bsingleton j x = function
      | A(a) -> (ASet.add a ASet.empty,FSet.empty)
      | F(a) -> (ASet.empty,FSet.add a FSet.empty)
    let bunion (a,b)(a',b')=(ASet.union a a',FSet.union b b')

    let find_sub = find_su sub true  byes bempty bsingleton bunion
    let find_sup = find_su sup false byes bempty bsingleton bunion
end
