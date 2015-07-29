(*****************)
(* Basic modules *)
(*****************)

open Interfaces_basic

module IntSort = struct

  module M = struct
    type _ t = int*bool*Sorts.t
    let equal _ (i1,b1,s1) (i2,b2,s2) = (i1=i2) && (b1=b2) && (s1=s2)
    let hash _ (i,b,s) = 2*i + 3*(if b then 0 else 1) + 5*(Hashtbl.hash s)
  end

  module H = HCons.Make(M)

  type t = H.Init.t

  let compare = H.compare
  let id = H.id
  let reveal t = let i,b,s = H.reveal t in i,s 
  let build (i,s)  = H.Init.build(i,true,s)
  let buildH (i,s) = H.Init.build(i,false,s)
  let clear = H.Init.clear

  let print_in_fmt fmt t =
    let (fv,b,so) = H.reveal t in
    if fv>=0 then Format.fprintf fmt "%s{%i}" (* "%s{%i}^{%a}" *) (if b then "" else "" (* \\underline *)) fv (* Sorts.print_in_fmt so *)
    else Format.fprintf fmt "?%i^{%a}" (-fv) Sorts.print_in_fmt so

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
