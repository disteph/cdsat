(*****************)
(* Basic modules *)
(*****************)

open Format

open Interfaces_basic

open General
open Patricia
open SetConstructions
       
module IntSort = struct

  module M = struct
    type _ t = int*bool*Sorts.t
    let equal _ (i1,b1,s1) (i2,b2,s2) = (i1=i2) && (b1=b2) && (s1=s2)
    let hash _ (i,b,s) = 2*i + 3*(if b then 0 else 1) + 5*(Hashtbl.hash s)
  end

  module H = HCons.Make(M)
  module HMade = H.Init(HCons.NoBackIndex)

  type t = HMade.t

  let compare = H.compare
  let id = H.id
  let reveal t = let i,_,s = H.reveal t in i,s 
  let build (i,s)  = HMade.build(i,true,s)
  let buildH (i,s) = HMade.build(i,false,s)
  let clear = HMade.clear

  let print_in_fmt fmt t =
    let (fv,b,so) = H.reveal t in
    match !Dump.display with
    | Dump.Latex ->
       if fv>=0 then Format.fprintf fmt "%s{%i}" (* "%s{%i}^{%a}" *) (if b then "" else "\\underline") fv (* Sorts.print_in_fmt so *)
       else Format.fprintf fmt "?%i" (* "?%i^{%a}" *) (-fv) (* Sorts.print_in_fmt so *)
    | _ ->
       if fv>=0 then Format.fprintf fmt "%s%i" (* "%s{%i}^{%a}" *) (if b then "" else "_") fv (* Sorts.print_in_fmt so *)
       else Format.fprintf fmt "?%i" (* "?%i^{%a}" *) (-fv) (* Sorts.print_in_fmt so *)

  let isDefined fv = let (_,b,_) = H.reveal fv in not b
  let isNeg fv = let (i,_,_) = H.reveal fv in i<0

end

module IntMap = Map.Make(struct
  type t = int
  let compare = Pervasives.compare
end)

module HashedTypeFromHCons(M: sig
  type t
  val id: t -> int
end) = struct
  type t = M.t
  let hash = M.id
  let equal a b = Pervasives.compare (M.id a) (M.id b) == 0
end

module IdMon = struct
  type 'a t = 'a
  let return a = a
  let bind (f: 'a -> 'b t) a = f a
end

module MakeCollection
         (OT: sig
              include Set.OrderedType
              val print_in_fmt: Format.formatter -> t -> unit
            end) = struct
  include Set.Make(OT)
  type e    = elt
  let next t = let e = choose t in (e,remove e t)
  let print_in_fmt fmt s =
    let _ = fold (fun a b -> fprintf fmt "%s%a" (if b then ", " else "") OT.print_in_fmt a; true) s false in
    ()
end

module MakePATCollection(M: PHCons) = struct

  module DSet = struct
    type keys      = M.t
    let kcompare a b = Pervasives.compare (M.id a) (M.id b)
    type infos     = unit
    let info_build = empty_info_build
    let treeHCons  = Some M.id
  end

  module I = TypesFromHConsed(M)

  include PATSet.Make(DSet)(I)
  let next t = let e = choose t in (e,remove e t)
  let print_in_fmt fmt s = print_in_fmt None M.print_in_fmt fmt s
end
