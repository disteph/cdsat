(*****************)
(* Basic modules *)
(*****************)

open Interfaces_basic

module IntSort
  = struct
    type intSort = int*bool*Sorts.t
    type t       = { reveal: intSort; id: int }
    let id is    = is.id
    let reveal_prim is = is.reveal
    let reveal is = let i,_,so = is.reveal in i,so
    module H = Hashtbl.Make(struct
      type t = intSort
      let equal (i1,b1,s1) (i2,b2,s2) = (i1=i2) && (b1=b2) && (s1=s2)
      let hash (i,b,s) = 2*i + 3*(if b then 0 else 1) + 5*(Hashtbl.hash s)
    end)
    let table  = H.create 5003
    let unique = ref 0
    let build_prim a =
      try H.find table a
      with Not_found -> 
        let f = { reveal =  a; id = !unique } in
        incr unique; H.add table a f; f
    let build (i,so) = build_prim(i,true,so)
    let buildH (i,so) = build_prim(i,false,so)
    let clear() = H.clear table
    let compare a b = Pervasives.compare (id a)(id b)
    let print_in_fmt fmt t =
      let (fv,b,so) = t.reveal in
      if fv>=0 then Format.fprintf fmt "%s{%i}^{%a}" (if b then "" else "\\underline") fv Sorts.print_in_fmt so
      else Format.fprintf fmt "?%i^{%a}" (-fv) Sorts.print_in_fmt so
    let isNeg fv = let (i,_,_) = fv.reveal in i<0
  end

module IntMap = Map.Make(struct
  type t = int
  let compare = Pervasives.compare
end)

module HashedTypeFromHCons(M: sig
  type t
  val id: t -> int
end)
= struct
  type t = M.t
  let hash = M.id
  let equal a b = Pervasives.compare (M.id a) (M.id b) == 0
end

module IdMon = struct
  type 'a t = 'a
  let return a = a
  let bind (f: 'a -> 'b t) a = f a
end
