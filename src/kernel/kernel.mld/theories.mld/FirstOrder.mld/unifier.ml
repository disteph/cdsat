(****************************************)
(* Internal representation of a Unifier *)
(****************************************)

open Format
open General

open Top
open Basic
open Variables

open General.Patricia
open General.SetConstructions

exception AlreadyConstrained
exception NoUnifWBinders

(* IntSort (an int together with a sort) are used to represent
   abstract keys. *)

let get_sort is = let _,s = IntSort.reveal is in s
            
module Leaf = struct

  type leafExposed = Key of IntSort.t | Eigen of Eigen.t [@@deriving eq,hash]

  module Arg = struct
    type 'a t = leafExposed [@@deriving eq,hash]
    let hash x = Hash.wrap1 hash_fold_t x
  end

  include HCons.Make(Arg)
  include Init(HCons.NoBackIndex)

  let pp fmt t = match reveal t with
    | Key k -> fprintf fmt "k%a" IntSort.pp k
    | Eigen ei -> fprintf fmt "%a" Eigen.pp ei

  let show = Dump.stringOf pp

  let get_sort leaf = match reveal leaf with
      | Key c   -> get_sort c
      | Eigen c -> Eigen.get_sort c
      
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
  let treeHCons = Some(IntSort.id,KTerm.hash,KTerm.equal)
end

module TMap = PATMap.Make(TermDest)(TypesFromHConsed(IntSort))

include TermDest

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

type exposed = Eigen of Eigen.t
             | Key of keys
             | C of Symbols.t*(KTerm.t list)

let reveal t =
  match Terms.reveal t with
  | Terms.V leaf ->
    (match Leaf.reveal leaf with
    | Leaf.Key k    -> Key k
    | Leaf.Eigen ei -> Eigen ei)
  | Terms.C(f,l) -> C(f,l)
  | Terms.FB(so,f,d) -> raise NoUnifWBinders

(* Actual unifiers *)

type t = {next_key:int; map:TMap.t}

let empty = {next_key = 0; map = TMap.empty}

let add key t u =
  match reveal t with
  | Key k when kcompare key k = 0 -> u
  | _ ->  
    { next_key = u.next_key;
      map = TMap.add key (function None -> t | Some _ -> raise AlreadyConstrained) u.map }

let new_key so u =
  IntSort.build(u.next_key,so),
  { u with next_key = u.next_key+1 }

(* Computing stuf with on-the-fly normalisation of unifier *)

let rec normalise ((t,u) as c) = 
  let aux = function
    | (Key k,u) when TMap.mem k u.map -> 
      let (t,new_u) = normalise (TMap.find k u.map,u) in
      (t, { new_u with map = TMap.add k (fun _ -> t) new_u.map})
    | _ -> c
  in
  aux (reveal t,u)

let expose c = let t,u = normalise c in (reveal t,u)

let get key u =
  if TMap.mem key u.map
  then Some(normalise(TMap.find key u.map,u))
  else None

(* Printing out stuff *)

let ppK = IntSort.pp
let ppV = KTerm.pp

let pp fmt u =
  let aux =
    TMap.print_in_fmt
      (fun fmt (key,term) -> fprintf fmt "(k%a -> %a)" ppK key ppV term)
  in
  fprintf fmt "next key=%i; map = %a" u.next_key aux u.map


(* We are going to construct maps between keys and mets *)

module KM = struct
  type keys      = IntSort.t
  let kcompare   = IntSort.compare
  type values    = Meta.t
  type infos     = keys m_infos
  let info_build = m_info_build kcompare
  let treeHCons  = Some(IntSort.hash,Meta.hash,Meta.equal)
end
module KMap  = PATMap.Make(KM)(TypesFromHConsed(IntSort))

module MK = struct
  type keys      = Meta.t
  let kcompare   = Meta.compare
  type values    = IntSort.t
  type infos     = keys m_infos
  let info_build = m_info_build kcompare
  let treeHCons  = Some(Meta.hash,IntSort.hash,IntSort.equal)
end
module MMap  = PATMap.Make(MK)(TypesFromHConsed(Meta))

module KK = struct
  type keys      = IntSort.t
  let kcompare   = IntSort.compare
  type values    = IntSort.t
  type infos     = keys m_infos
  let info_build = m_info_build kcompare
  let treeHCons  = Some(IntSort.hash,IntSort.hash,IntSort.equal)
end
module KKMap = PATMap.Make(KK)(TypesFromHConsed(IntSort))
