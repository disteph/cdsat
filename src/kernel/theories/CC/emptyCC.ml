(****************************************)
(* The module X for CC(X) where X=Empty *)
(****************************************)

open Top
open Basic
open Interfaces_basic
open Specs

open General
open SetConstructions
open Patricia

module Make(DS: GTheoryDSType)= struct

  open DS

  type t = Term.t

(* the semantic values are terms: 
   there is no interpreted symbol for this theory *)
  type v = Term.t

  let vequal = Terms.equal

  module VSet = TSet

  module DVtoTSet = struct
    type keys = v
    let kcompare = Terms.compare
    type values = TSet.t
    type infos = unit
    let info_build = empty_info_build
    let treeHCons = None
  end

  module F = struct
    type t = Term.t
    let id f = Terms.id f
  end

  module I = TypesFromHConsed (F)

  module MVtoTSet = PATMap.Make (DVtoTSet) (I)

  module VtoTSet = struct
    type e = Term.t
    type v = TSet.t
    type t = MVtoTSet.t
    let find = MVtoTSet.find
    let empty = MVtoTSet.empty
    let add i s t  = MVtoTSet.add i (function _ -> s) t
    let union t t' = MVtoTSet.union (fun s s' -> TSet.union s s') t t'
    let remove i t = MVtoTSet.remove i t
    let map f t = MVtoTSet.map (fun x y -> f y) t
    let fold f t a = MVtoTSet.fold (fun x y a' -> f x a') t a
  end

  module DVtoV = struct
    type keys = Term.t
    let kcompare = Terms.compare
    type values = Term.t
    type infos = unit
    let info_build = empty_info_build
    let treeHCons = Some(Terms.id,Terms.id,vequal)
  end

  module MVtoV = PATMap.Make (DVtoV) (I)

  module VtoV = struct
    type e = Term.t
    type v = Term.t
    type t = MVtoV.t
    let find = MVtoV.find
    let empty = MVtoV.empty
    let add i s t = MVtoV.add i (function _ -> s) t
    let union t t' = MVtoV.union (fun s s' -> s) t t'
    let remove i t = MVtoV.remove i t
    let map f t = MVtoV.map (fun x y -> f y) t
    let fold f t a = MVtoV.fold (fun x y a' -> f x a') t a
  end

  let make t = t

  let leaves r = TSet.add r TSet.empty

  let rec subst p q r =
    if Terms.compare p r = 0 then q else r

  type res = Sol of v*v | Bot | Top

  let solve r r' =
    if Terms.compare r r' = 0 then Top
    else Sol(r,r')

end
