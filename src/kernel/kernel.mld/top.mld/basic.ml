(*****************)
(* Basic modules *)
(*****************)

open Format

open General
open Patricia
open Patricia_tools

include Basic_sig
    
module IntSort = struct

  module M = struct
    type 'a t = int*bool*Sorts.t [@@deriving eq, hash]
    let name = "IntSort"
  end

  include HCons.Make(M)
  include Init(M)
  let buildH (i,s) = build(i,false,s)
  let build (i,s)  = build(i,true,s)

  let pp fmt t =
    let fv,b,so = reveal t in
    match !Dump.display with
    | Dump.Latex ->
       if fv>=0 then Format.fprintf fmt "%s{%i}" (* "%s{%i}^{%a}" *) (if b then "" else "\\underline") fv (* Sorts.pp so *)
       else Format.fprintf fmt "?%i" (* "?%i^{%a}" *) (-fv) (* Sorts.pp so *)
    | _ ->
       if fv>=0 then Format.fprintf fmt "%s%i" (* "%s{%i}^{%a}" *) (if b then "" else "_") fv (* Sorts.pp so *)
       else Format.fprintf fmt "?%i" (* "?%i^{%a}" *) (-fv) (* Sorts.pp so *)

  let show = Print.stringOf pp

  let isDefined fv = let _,b,_ = reveal fv in not b
  let isNeg fv = let i,_,_ = reveal fv in i<0
  let reveal t = let i,_,s = reveal t in i,s 

end

module IntMap = Map.Make(struct
                    type t = int [@@deriving ord]
                  end)

module IdMon = struct
  type 'a t = 'a
  let return a = a
  let bind (f: 'a -> 'b t) a = f a
end

module MakeCollection
         (OT: sig
              type t [@@deriving ord,show,hash]
            end) = struct
  include Set.Make(OT)
  type e    = elt
  let next t = let e = choose t in (e,remove e t)
  let hash_fold_t s t = List.hash_fold_t OT.hash_fold_t s (elements t)
  let hash t = Hash.fold2hash hash_fold_t t
  let pp fmt s = List.pp OT.pp fmt (elements s)
  let show = Print.stringOf pp
end

module MakePATCollection(M: PHCons) = struct

  module Arg = struct
    include M
    include EmptyInfo
    include TypesFromHConsed(M)
  end

  include SetH(Arg)
  let next t = let e = choose t in (e,remove e t)
  let pp = print_in_fmt ~wrap:("","") M.pp
  let show = Print.stringOf pp
end

