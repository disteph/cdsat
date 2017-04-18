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
    type 'a t = int*bool*Sorts.t [@@deriving eq, hash]
    let hash f = Hash.wrap1 hash_fold_t f
  end

  module H = HCons.Make(M)
  module HMade = H.Init(HCons.NoBackIndex)
  include (HMade: sig type t = unit H.generic [@@deriving eq,hash] end)
            
  let compare = H.compare
  let id = H.id
  let reveal t = let i,_,s = H.reveal t in i,s 
  let build (i,s)  = HMade.build(i,true,s)
  let buildH (i,s) = HMade.build(i,false,s)
  let clear = HMade.clear

  let pp fmt t =
    let (fv,b,so) = H.reveal t in
    match !Dump.display with
    | Dump.Latex ->
       if fv>=0 then Format.fprintf fmt "%s{%i}" (* "%s{%i}^{%a}" *) (if b then "" else "\\underline") fv (* Sorts.print_in_fmt so *)
       else Format.fprintf fmt "?%i" (* "?%i^{%a}" *) (-fv) (* Sorts.print_in_fmt so *)
    | _ ->
       if fv>=0 then Format.fprintf fmt "%s%i" (* "%s{%i}^{%a}" *) (if b then "" else "_") fv (* Sorts.print_in_fmt so *)
       else Format.fprintf fmt "?%i" (* "?%i^{%a}" *) (-fv) (* Sorts.print_in_fmt so *)

  let show = Dump.stringOf pp

  let isDefined fv = let _,b,_ = H.reveal fv in not b
  let isNeg fv = let i,_,_ = H.reveal fv in i<0

end

module IntMap = Map.Make(struct
  type t = int
  let compare = Pervasives.compare
end)

module IdMon = struct
  type 'a t = 'a
  let return a = a
  let bind (f: 'a -> 'b t) a = f a
end

module MakeCollection
         (OT: sig
              type t [@@deriving ord,show]
            end) = struct
  include Set.Make(OT)
  type e    = elt
  let next t = let e = choose t in (e,remove e t)
  let pp fmt s =
    let _ = fold
              (fun a b -> fprintf fmt "%s%a"
                            (if b then ", " else "")
                            OT.pp a;
                          true)
              s false in
    ()
  let show = Dump.stringOf pp
end

module MakePATCollection(M: PHCons) = struct

  module DSet = struct
    type keys      = M.t
    let kcompare   = id2compare M.id
    type infos     = unit
    let info_build = empty_info_build
    let treeHCons  = Some M.id
  end

  module I = TypesFromHConsed(M)

  include PATSet.Make(DSet)(I)
  let next t = let e = choose t in (e,remove e t)
  let pp fmt s = print_in_fmt M.pp fmt s
  let show = Dump.stringOf pp
end
