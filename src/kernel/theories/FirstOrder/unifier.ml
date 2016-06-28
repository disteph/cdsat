(****************************************)
(* Internal representation of a Unifier *)
(****************************************)

open Format

open Top
open Basic
open Variables

open General.Patricia
open General.SetConstructions

exception AlreadyConstrained

(* IntSort (an int together with a sort) are used to represent
   abstract keys. *)

module IntSortHashed = HashedTypeFromHCons(IntSort)
module EigenHashed   = HashedTypeFromHCons(Eigen)
module MetaHashed    = HashedTypeFromHCons(Meta)

module Leaf = struct

  type leafExposed = Key of IntSort.t | Eigen of Eigen.t

  module Arg = struct
    type _ t = leafExposed
    let equal _ a b = match a,b with
      | Key c, Key d when IntSortHashed.equal c d -> true
      | Eigen c, Eigen d when EigenHashed.equal c d -> true
      | _,_ -> false 
    let hash _ = function
      | Key c   -> 2*(IntSortHashed.hash c)
      | Eigen c -> 3*(EigenHashed.hash c)
  end

  include HCons.Make(Arg)
  include Init(HCons.NoBackIndex)

  let print_in_fmt fmt t = match reveal t with
    | Key k -> fprintf fmt "k%a" IntSort.print_in_fmt k
    | Eigen ei -> fprintf fmt "%a" Eigen.print_in_fmt ei

end

module KTerm = Terms.Make(Leaf)(Terms.EmptyData(Leaf))

(* A unifier is a map from keys to values = "key terms",
   i.e. terms whose leaves are either eigen or keys.

   The unifier is implemented as a record, with field next_key
   satisfying the following invariant: any key above next_key
   (included) is fresh for the map (not only is it not in its
   domain, but it does not appear within the terms in the range of
   the map)
*)

module TermDest = struct
  type keys = IntSort.t
  let kcompare = IntSort.compare
  type values = KTerm.t
  type infos = keys m_infos
  let info_build = m_info_build kcompare
  let treeHCons = Some(IntSort.id,Terms.id,(fun x y -> Terms.id x = Terms.id y))
end

module TMap = PATMap.Make(TermDest)(TypesFromHConsed(IntSort))

include TermDest
let get_sort k = let (_,so) = IntSort.reveal k in so

(* Constructing values *)

let key2val k   = KTerm.bV(Leaf.build(Leaf.Key k))
let eigen2val e = KTerm.bV(Leaf.build(Leaf.Eigen e))
let bC          = KTerm.bC

let internalise update =
  let aux fv = match FreeVar.reveal fv with
    | FreeVar.Eigen ei -> Leaf.build(Leaf.Eigen ei)
    | FreeVar.Meta mv  -> Leaf.build(Leaf.Key(update mv))
  in
  KTerm.subst aux

(* Reading values *)

type exposed = Eigen of Eigen.t | Key of keys | C of Symbols.t*(KTerm.t list)

let reveal t =
  match Terms.reveal t with
  | Terms.V leaf ->
    (match Leaf.reveal leaf with
    | Leaf.Key k    -> Key k
    | Leaf.Eigen ei -> Eigen ei)
  | Terms.C(f,l) -> C(f,l)

(* Actual unifiers *)

type t = {next_key:int; map:TMap.t}

let empty = {next_key = 0; map = TMap.empty}

let add key t u =
  match reveal t with
  | Key k when kcompare key k == 0 -> u
  | _ ->  
    { next_key = u.next_key;
      map = TMap.add key (function None -> t | Some _ -> raise AlreadyConstrained) u.map }

let new_key so u =
  IntSort.build(u.next_key,so),
  {next_key = u.next_key+1; map = u.map}

(* Computing stuf with on-the-fly normalisation of unifier *)

let rec normalise ((t,u) as c) = 
  let aux = function
    | (Key k,u) when TMap.mem k u.map -> 
      let (t,new_u) = normalise (TMap.find k u.map,u) in
      (t, {next_key = new_u.next_key; map = TMap.add k (fun _ -> t) new_u.map})
    | _ -> c
  in
  aux (reveal t,u)

let expose c = let t,u = normalise c in (reveal t,u)

let get key u =
  if TMap.mem key u.map
  then Some(normalise(TMap.find key u.map,u))
  else None

(* Printing out stuff *)

let print_in_fmtK = IntSort.print_in_fmt
let print_in_fmtV = KTerm.print_in_fmt

let print_in_fmt fmt u =
  let aux fmt =
    TMap.fold
      (fun key term () -> fprintf fmt "k%a -> %a; " print_in_fmtK key print_in_fmtV term)
      u.map
      ()
  in
  fprintf fmt "{next=%i; map = %t}" u.next_key aux


(* We are going to construct maps between keys and mets *)

module KM = struct
  type keys      = IntSort.t
  let kcompare   = IntSort.compare
  type values    = Meta.t
  type infos     = keys m_infos
  let info_build = m_info_build kcompare
  let treeHCons  = Some(IntSortHashed.hash,MetaHashed.hash,MetaHashed.equal)
end
module KMap  = PATMap.Make(KM)(TypesFromHConsed(IntSort))

module MK = struct
  type keys      = Meta.t
  let kcompare   = Meta.compare
  type values    = IntSort.t
  type infos     = keys m_infos
  let info_build = m_info_build kcompare
  let treeHCons  = Some(MetaHashed.hash,IntSortHashed.hash,IntSortHashed.equal)
end
module MMap  = PATMap.Make(MK)(TypesFromHConsed(Meta))

module KK = struct
  type keys      = IntSort.t
  let kcompare   = IntSort.compare
  type values    = IntSort.t
  type infos     = keys m_infos
  let info_build = m_info_build kcompare
  let treeHCons  = Some(IntSortHashed.hash,IntSortHashed.hash,IntSortHashed.equal)
end
module KKMap = PATMap.Make(KK)(TypesFromHConsed(IntSort))
