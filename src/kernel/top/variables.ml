open Format

open Interfaces_basic
open Basic

module Meta  = struct
  include IntSort
  let get_sort mv = let _,so = reveal mv in so
end

module Eigen = struct
  include IntSort
  let get_sort ei = let _,so = reveal ei in so
end

module MetaHashed  = HashedTypeFromHCons(Meta)
module EigenHashed = HashedTypeFromHCons(Eigen)

module FreeVar = struct

  type freeVarExposed = Meta of Meta.t | Eigen of Eigen.t

  module Arg = struct
    type _ t = freeVarExposed
    let equal _ a b = match a,b with
      | Meta c,  Meta d  when MetaHashed.equal c d  -> true
      | Eigen c, Eigen d when EigenHashed.equal c d -> true
      | _,_ -> false 
    let hash _ = function
      | Meta c  -> 2*(MetaHashed.hash c)
      | Eigen c -> 3*(EigenHashed.hash c)
  end

  include HCons.Make(Arg)
  include Init(HCons.NoBackIndex)

  let get_sort fv = match reveal fv with
    | Meta mv  -> Meta.get_sort mv
    | Eigen ei -> Eigen.get_sort ei

  let print_in_fmt fmt t = match reveal t with
    | Meta mv  -> fprintf fmt "?%a" Meta.print_in_fmt mv
    | Eigen ei -> fprintf fmt "%a" Eigen.print_in_fmt ei

end

module World = struct

  type 'r worldExposed = Init | NewWorld of bool*Sorts.t*'r

  module Arg = struct
    type 'r t = 'r worldExposed
    let equal eq_rec a b = match a,b with
      | Init, Init -> true
      | NewWorld(b1,so1,pw1),NewWorld(b2,so2,pw2)
        -> b1=b2 && so1=so2 && eq_rec pw1 pw2
      | _,_ -> false
    let hash hash_rec = function
      | Init -> 2
      | NewWorld(b,so,w)
        -> 3*(if b then 1 else 2)+5*(Hashtbl.hash so) + 7*(hash_rec w)
  end

  include HCons.Make(Arg)

  type data = {
    last : FreeVar.t option;
    next_eigen : int;
    next_meta  : int;
    ithE : Eigen.t IntMap.t;
    ithM : Meta.t IntMap.t;
    depEM : int IntMap.t;
    depME : int IntMap.t;
  }

  module Data = struct

    type t = data

    let build _ = function
      | Init ->  
        {
          last = None;
          next_eigen = 0;
          next_meta  = 0;
          ithE = IntMap.empty;
          ithM = IntMap.empty;
          depEM = IntMap.empty;
          depME = IntMap.empty;
        }
      | NewWorld(b,so,w) -> 
        let ar = data w in 
        if b 
        then
          let ei = Eigen.build(ar.next_eigen,so) in
          {
            last = Some(FreeVar.build(FreeVar.Eigen ei));
            next_eigen = ar.next_eigen+1;
            next_meta  = ar.next_meta; 
            ithE  = IntMap.add ar.next_eigen ei ar.ithE;
            ithM  = ar.ithM;
            depEM = IntMap.add ar.next_eigen ar.next_meta ar.depEM;
            depME = ar.depME;
          }
        else
          let mv = Meta.build(ar.next_meta,so) in
          {
            last = Some(FreeVar.build(FreeVar.Meta mv));
            next_eigen = ar.next_eigen;
            next_meta  = ar.next_meta+1; 
            ithE  = ar.ithE;
            ithM  = IntMap.add ar.next_meta mv ar.ithM;
            depEM = ar.depEM;
            depME = IntMap.add ar.next_meta ar.next_eigen ar.depME;
          }
        
  end

  include InitData(HCons.NoBackIndex)(Data)

  let init = build Init

  let liftE so w =
    let newWorld = build(NewWorld(true,so,w)) in
    match (data newWorld).last with
    | Some fv -> fv, newWorld
    | None -> failwith "Just added eigen; last freevar must exist"

  let liftM so w =
    let newWorld = build(NewWorld(false,so,w)) in
    match (data newWorld).last with
    | Some fv -> fv, newWorld
    | None -> failwith "Just added meta; last freevar must exist"

  let proj w = match reveal w with
    | Init -> failwith "Trying to project the initial world!"
    | NewWorld(_,_,res) -> 
      (match (data w).last with
      | Some fv -> fv,res
      | None -> failwith "Successful projection should have a last freevar")

  let equal w1 w2 = Pervasives.compare (id w1) (id w2) ==0 
  let hash = id

  let rec prefix w1 w2 =
    equal w1 w2
    || (let a1,a2 = data w1, data w2 in
        (a1.next_eigen <= a2.next_eigen)
        && (a1.next_meta <= a2.next_meta)
        && match reveal w2 with
        | Init -> false
        | NewWorld(_,_,w) -> prefix w1 w)

  let print_in_fmtEM fmt w =
    let ar = data w in 
    let aux fmt =
      IntMap.fold (fun i j () -> Format.fprintf fmt "(%a fresh for #%i); " Eigen.print_in_fmt (IntMap.find i ar.ithE) j) ar.depEM ()
    in
    Format.fprintf fmt "next meta: ?%i; %t" ar.next_meta aux 

  let print_in_fmtME fmt w = 
    let ar = data w in 
    let aux fmt =
      IntMap.fold (fun i j () -> Format.fprintf fmt "(?%a fresh for #%i); " Meta.print_in_fmt (IntMap.find i ar.ithM) j) ar.depME ()
    in
    Format.fprintf fmt "%i; %t" ar.next_eigen aux 

  let print_in_fmt = print_in_fmtEM

  let fold w ei f =
    let ar = data w in
    let (i,_) = Eigen.reveal ei in
    let bound = IntMap.find i ar.depEM in 
    let rec aux j u = 
      if j == bound then u
      else
        aux (j+1) (f (IntMap.find j ar.ithM) u)
    in aux 0

end

module MakesSense = struct

  type t = int*int

  let init = (-1,-1)

  let fv fv = match FreeVar.reveal fv with
    | FreeVar.Eigen ei -> let i,_ = IntSort.reveal ei in (i,-1)
    | FreeVar.Meta mv  -> let i,_ = IntSort.reveal mv in (-1,i)

  let combine (a1,a2) (b1,b2) = (Pervasives.max a1 b1,Pervasives.max a2 b2)

  let check (ei_max,mv_max) w = 
    let ar = World.data w in
    (ei_max < ar.World.next_eigen) && (mv_max < ar.World.next_meta)

end
