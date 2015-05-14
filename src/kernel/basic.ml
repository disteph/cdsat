(*****************)
(* Basic modules *)
(*****************)

open Interfaces_basic

module IntSort
  = struct
    type intSort = int*Sorts.t
    type t       = { reveal: intSort; id: int }
    let id is    = is.id
    let reveal is = is.reveal
    module H = Hashtbl.Make(struct
      type t = intSort
      let equal (i1,s1) (i2,s2) = (i1=i2) && (s1=s2)
      let hash (i,s) = 2*i + 3*(Hashtbl.hash s)
    end)
    let table  = H.create 5003
    let unique = ref 0
    let build a =
      try H.find table a
      with Not_found -> 
        let f = { reveal =  a; id = !unique } in
        incr unique; H.add table a f; f
    let clear() = H.clear table
    let compare a b = Pervasives.compare (id a)(id b)
    let print_in_fmt fmt t =
      let (fv,so) = reveal t in
      if fv>=0 then Format.fprintf fmt "%i^{%a}" fv Sorts.print_in_fmt so
      else Format.fprintf fmt "?%i^{%a}" (-fv) Sorts.print_in_fmt so
    let isNeg fv = let (i,_) = reveal fv in i<0
  end

module IntMap = Map.Make(struct
  type t = int
  let compare = Pervasives.compare
end)
