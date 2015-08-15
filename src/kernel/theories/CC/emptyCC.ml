(****************************************)
(* The module X for CC(X) where X=Empty *)
(****************************************)

open Top
open Basic
open Interfaces_basic
open Specs

open Prop
open Literals

open General
open SetConstructions
open Patricia

open Interfaces

module Make(Term: sig 
  include TermF
  val proj: datatype -> LitF.t
end) = struct

  type t = Term.t

  module F = struct
    type t = Term.t
    let id f = Terms.id f
  end

  module I = TypesFromHConsed (F)

  let compare f1 f2 = Pervasives.compare (F.id f1) (F.id f2)

  module DTSet = struct
    type keys = t
    let kcompare = compare
    type values = unit
    type infos = unit
    let info_build = empty_info_build
    let treeHCons = Some Terms.id
  end

  module TSet = PATSet.Make (DTSet) (I)

  let directSubterms t = 
    match Terms.reveal t with
    | Terms.V x    -> []
    | Terms.C(f,l) -> l

  let root t = 
    match Terms.reveal t with
    | Terms.V x    -> None
    | Terms.C(f,l) -> Some f

(* the semantic values are terms: 
   there is no interpreted symbol for this theory *)
  type v = t

  module VSet = TSet

  module DVtoTSet = struct
    type keys = t
    let kcompare = compare
    type values = TSet.t
    type infos = unit
    let info_build = empty_info_build
    let treeHCons = Some((fun ts1 ts2 -> TSet.compare ts1 ts2 =0),Terms.id,TSet.id)
  end

  module MVtoTSet = PATMap.Make (DVtoTSet) (I)

  module VtoTSet = struct
    type e = t
    type v = TSet.t
    type t = MVtoTSet.t
    let find = MVtoTSet.find
    let empty = MVtoTSet.empty
    let add i s t = MVtoTSet.add i (fun f -> match f with
      | None -> s
      | (Some r) -> s) t
    let union t t' = MVtoTSet.union (fun s s' -> TSet.union s s') t t'
    let remove i t = MVtoTSet.remove i t
    let map f t = MVtoTSet.map (fun x y -> f y) t
    let fold f t a = MVtoTSet.fold (fun x y a' -> f x a') t a
  end

  module DVtoV = struct
    type keys = t
    let kcompare = compare
    type values = t
    type infos = unit
    let info_build = empty_info_build
    let treeHCons = Some((fun t1 t2 -> compare t1 t2 =0),Terms.id,Terms.id)
  end

  module MVtoV = PATMap.Make (DVtoV) (I)

  module VtoV = struct
    type e = t
    type v = t
    type t = MVtoV.t
    let find = MVtoV.find
    let empty = MVtoV.empty
    let add i s t = MVtoV.add i (fun f -> match f with
      | None -> s
      | (Some r) -> s) t
    let union t t' = MVtoV.union (fun s s' -> s) t t'
    let remove i t = MVtoV.remove i t
    let map f t = MVtoV.map (fun x y -> f y) t
    let fold f t a = MVtoV.fold (fun x y a' -> f x a') t a
  end

  let make t = t

  let leaves r = (VSet.add r VSet.empty)

  let rec subst p q r =
    if (compare p r) = 0 then q else r

  type res = Sol of v*v | Bot | Top

  let solve r r' =
    if (compare r r') = 0 then Top
    else Sol(r,r')

  let predicate a =
    let b,i = LitF.reveal(Term.proj(Terms.data a)) in
    b,Term.term_of_id i

  let atoI a = 
    let b,t = predicate a in
    match Terms.reveal t with
    | Terms.C(Symbols.Eq _,l) -> 
      if b then (Eq(List.hd l,List.hd (List.tl l)))
      else (NEq(List.hd l,List.hd (List.tl l)))
    | _ -> assert false

  let itoA = function
    | Eq(a,b)  -> Term.bC (Symbols.Eq (Sorts.User "")) [a;b]
    | NEq(a,b) -> Term.bC (Symbols.Eq (Sorts.User "")) [a;b]
    | _ -> assert false


end
