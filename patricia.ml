(* Material that must be provided to construct a Patricia tree
structure for Maps *)

module type UserTypes = sig
  (* Domain of the map (keys),
     implementation of keys and how to compute them
     - typically for a HConsed type, common = int
  *)
  type keys
  type common
  val tag         : keys->common

  (* Co-domain of the map (values), and how values compare *)    
  type values
  val vcompare    : values->values->bool

  (* Allows to store information about the map from keys to values
     - typically, number of bindings stored, etc *) 
  type infos
    
  (* Provide info for empty map, singleton map, and disjoint union
  of two maps *) 
  val info_build : infos*(keys->values->infos)*(infos->infos->infos)

  (* Structures for the internal use of the Patricia trees *) 
  type branching
  val check       : common->branching->bool
  val commonise   : common->common->common*branching
  val match_prefix: common->common->branching->bool
  val smallerthan : branching->branching->bool 
  val treeHCons   : bool
end


(* Construction of a Patricia tree structure for maps, given a MapTypes *)

module PATMap =
  functor (UT:UserTypes)-> struct

    (* This module BackOffice will be share by the Patricia tree
    structure for sets, below *)
    module BackOffice = struct

      include UT

      (* Our Patricia trees are HConsed *)

      type 'a pat =
	| Empty
	| Leaf of UT.keys * UT.values
	| Branch of UT.common * UT.branching * 'a * 'a

      type tt = {reveal: tt pat ; id:int ; info: infos}

      let reveal f = f.reveal
      let id f = f.id
      let info f = f.info

      let info_gen = let (a,b,c)=info_build in function
	| Empty    -> a
	| Leaf(k,x)-> b k x
	| Branch(_,_,t0,t1)-> c t0.info t1.info  

      (* Primitive type of Patricia trees to feed the HashTable Make
      functor *)

      module PATPrimitive = 
	(struct
	   type t = tt
	   let equal t1 t2 =
	     match t1.reveal,t2.reveal with
	       | Empty, Empty                             -> true
	       | Leaf(key1,value1), Leaf(key2,value2)     -> key1==key2 && (vcompare value1 value2)
	       | Branch(c1,b1,t3,t3'),Branch(c2,b2,t4,t4')-> c1==c2 && b1==b2 && t3==t4 && t3'==t4'
	       | _                                        -> false 
	   let hash t1 =
	     match t1.reveal with
	       | Empty            -> 1
	       | Leaf(key,value)  -> 2*(Hashtbl.hash key)+3*(Hashtbl.hash value)
	       | Branch(c,b,t2,t3)-> 5*(Hashtbl.hash c)+7*(Hashtbl.hash b)+11*t2.id+13*t3.id
	 end: Hashtbl.HashedType with type t=tt)

      include PATPrimitive

      module H = Hashtbl.Make(PATPrimitive)
      let table = H.create 5003 
      let uniq =ref 0
      let build a =
	let f = {reveal =  a; id = !uniq ; info = info_gen a} in
	  if treeHCons then
	    try H.find table f
	    with Not_found -> incr uniq; H.add table f f; f
	  else f

      let equal   t1 t2 = (t1==t2)
      let compare t1 t2 = Pervasives.compare t1.id t2.id

      (* Now we start the standard functions on maps/sets *)

      let is_empty t = match reveal t with Empty -> true | _ -> false

      let rec mem k t = match reveal t with
	| Empty -> false
	| Leaf (j,_) -> (tag k) == (tag j)
	| Branch (_, m, l, r) -> mem k (if check (tag k) m then l else r)

      let rec find k t = match reveal t with
	| Empty -> raise Not_found
	| Leaf (j,x) -> if (tag k) == (tag j) then x else raise Not_found
	| Branch (_, m, l, r) -> find k (if check (tag k) m then l else r)

      let rec cardinal t = match reveal t with
	| Empty  -> 0
	| Leaf _ -> 1
	| Branch (_,_,t0,t1) -> cardinal t0 + cardinal t1

      (* Smart constructors, using both the HConsing techniques and
      assuring some invariant of Patricia trees *)

      let empty      = build Empty
      let leaf (k,x) = build (Leaf(k,x))
      let branch = function
	| (_,_,e,t) when (reveal e==Empty) -> t
	| (_,_,t,e) when (reveal e==Empty) -> t
	| (c,b,t0,t1)   -> build(Branch (c,b,t0,t1))

      (* Assumed in function join:
	 p0 is the common part of tree t0
	 p1 is the common part of tree t1 *)

      let join (p0,t0,p1,t1) =
	let (c,m) = commonise p0 p1 in
	  if check p0 m then 
	    branch (c, m, t0, t1)
	  else 
	    branch (c, m, t1, t0)

      let remove k t =
	let rec rmv t = match reveal t with
	  | Empty      -> empty
	  | Leaf (j,_) -> if (tag k == tag j) then empty else t
	  | Branch (p,m,t0,t1) -> 
	      if match_prefix (tag k) p m then
		if check (tag k) m then
		  branch (p, m, rmv t0, t1)
		else
		  branch (p, m, t0, rmv t1)
	      else
		t
	in
	  rmv t

    (* Now we have finished the material that common to Maps AND Sets,
    closing module BackOffice *)
    end

    include BackOffice

    (* Now starting functions specific to Maps, not Sets *)

    let add k x t =
      let rec ins t = match reveal t with
	| Empty      -> leaf(k,x)
	| Leaf (j,_) -> 
	    if (tag j == tag k) then 
	      leaf (k,x) 
	    else 
	      join (tag k, leaf(k,x), tag j, t)
	| Branch (c,b,t0,t1) ->
	    if match_prefix (tag k) c b then
	      if check (tag k) b then 
		branch (c,b, ins t0, t1)
	      else
		branch (c,b, t0, ins t1)
	    else
	      join (tag k, leaf(k,x), c, t)
      in
	ins t

    let rec iter f t = match reveal t with
      | Empty -> ()
      | Leaf (k,x) -> f k x
      | Branch (_,_,t0,t1) -> iter f t0; iter f t1

    let rec map f t = match reveal t with
      | Empty              -> empty
      | Leaf (k,x)         -> leaf (k, f k x)
      | Branch (p,m,t0,t1) -> branch (p, m, map f t0, map f t1)
	  
    let rec fold f s accu = match reveal s with
      | Empty -> accu
      | Leaf (k,x) -> f k x accu
      | Branch (_,_,t0,t1) -> fold f t0 (fold f t1 accu)

  end
;;

(* Construction of a Patricia tree structure for sets, given a
SetTypes.  Most of it is imported from PATMap *)

module PATSet =
  functor (ST:UserTypes with type values = unit)-> struct

    (* A Set is just a Map with codomain unit.  Constructing this Map
       structure *)

    module PM = PATMap(struct
      include ST
    end)

    include PM.BackOffice

    (* Now starting functions specific to Sets, not Maps.
       Starting with similar functions *)

    let singleton k= PM.leaf(k,())
    let add k t    = PM.add k () t
    let iter f     = PM.iter (fun k x -> f k)
    let map f      = PM.map (fun k x -> f k)	  
    let fold f     = PM.fold (fun k x -> f k)

    (* Now starting functions specific to Sets, without equivalent
    ones for Maps *)

    let make l     = List.fold_right add l empty

    let rec merge (u1,u2) = match reveal u1,reveal u2 with
      | Empty, _  -> u2
      | _, Empty  -> u1
      | Leaf(k,()), _ -> add k u2
      | _, Leaf(k,()) -> add k u1
      | Branch (p,m,s0,s1), Branch (q,n,t0,t1) ->
	  if m == n && match_prefix q p m then
	    (* The trees have the same prefix. Merge the subtrees. *)
	    branch (p, m, merge (s0,t0), merge (s1,t1))
	  else if ST.smallerthan m n && match_prefix q p m then
	    (* [q] contains [p]. Merge [t] with a subtree of [s]. *)
	    if check q m then 
	      branch (p, m, merge (s0,u2), s1)
            else 
	      branch (p, m, s0, merge (s1,u2))
	  else if ST.smallerthan n m && match_prefix p q n then
	    (* [p] contains [q]. Merge [s] with a subtree of [t]. *)
	    if check p n then
	      branch (q, n, merge (u1,t0), t1)
	    else
	      branch (q, n, t0, merge (u1,t1))
	  else
	    (* The prefixes disagree. *)
	    join (p, u1, q, u2)

    let union s t = merge (s,t)

    let rec subset s1 s2 = match (reveal s1,reveal s2) with
      | Empty, _             -> true
      | _, Empty             -> false
      | Leaf(k,()), _        -> mem k s2
      | Branch _, Leaf(k,()) -> false
      | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	  if m1 == m2 && p1 == p2 then
	    subset l1 l2 && subset r1 r2
	  else if ST.smallerthan m2 m1 && match_prefix p1 p2 m2 then
	    if check p1 m2 then 
	      subset l1 l2 && subset r1 l2
	    else 
	      subset l1 r2 && subset r1 r2
	  else
	    false

    let rec inter s1 s2 = match (reveal s1,reveal s2) with
      | Empty, _      -> empty
      | _, Empty      -> empty
      | Leaf(k,()), _ -> if mem k s2 then s1 else empty
      | _, Leaf(k,()) -> if mem k s1 then s2 else empty
      | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	  if m1 == m2 && p1 == p2 then 
	    merge (inter l1 l2, inter r1 r2)
	  else if ST.smallerthan m1 m2 && match_prefix p2 p1 m1 then
	    inter (if check p2 m1 then l1 else r1) s2
	  else if ST.smallerthan m2 m1 && match_prefix p1 p2 m2 then
	    inter s1 (if check p1 m2 then l2 else r2)
	  else
	    empty

    let rec diff s1 s2 = match (reveal s1,reveal s2) with
      | Empty, _      -> empty
      | _, Empty      -> s1
      | Leaf(k,()), _ -> if mem k s2 then empty else s1
      | _, Leaf(k,()) -> remove k s1
      | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	  if m1 == m2 && p1 == p2 then
	    merge (diff l1 l2, diff r1 r2)
	  else if ST.smallerthan m1 m2 && match_prefix p2 p1 m1 then
	    if check p2 m1 then 
	      merge (diff l1 s2, r1) 
	    else 
	      merge (l1, diff r1 s2)
	  else if ST.smallerthan m2 m1 && match_prefix p1 p2 m2 then
	    if check p1 m2 then diff s1 l2 else diff s1 r2
	  else
	    s1

    let rec for_all p t = match reveal t with
      | Empty      -> true
      | Leaf(k,()) -> p k
      | Branch (_,_,t0,t1) -> for_all p t0 && for_all p t1

    let rec exists p t = match reveal t with
      | Empty      -> false
      | Leaf(k,()) -> p k
      | Branch (_,_,t0,t1) -> exists p t0 || exists p t1

    let rec filter pr t = match reveal t with
      | Empty      -> empty
      | Leaf(k,()) -> if pr k then t else empty
      | Branch (p,m,t0,t1) -> branch (p, m, filter pr t0, filter pr t1)

    let partition p s =
      let rec part (t,f as acc) u =  match reveal u with
	| Empty      -> acc
	| Leaf(k,()) -> if p k then (add k t, f) else (t, add k f)
	| Branch (_,_,t0,t1) -> part (part acc t0) t1
      in
	part (empty, empty) s

    let rec choose t =  match reveal t with
      | Empty      -> raise Not_found
      | Leaf(k,()) -> k
      | Branch (_, _,t0,_) -> choose t0   (* we know that [t0] is non-empty *)

    let elements s =
      let rec elements_aux acc t = match reveal t with
	| Empty      -> acc
	| Leaf(k,()) -> k :: acc
	| Branch (_,_,l,r) -> elements_aux (elements_aux acc l) r
      in
	elements_aux [] s

    let rec elect f t = match reveal t with
      | Empty      -> raise Not_found
      | Leaf(k,()) -> k
      | Branch (_,_,s,t) -> f (elect f s) (elect f t)

  end
;;


module TypesFromHConsed = 
  functor (S:sig
	     type keys
	     val tag        : keys->int
	     type values
	     val vcompare   : values->values->bool
	     type infos
	     val info_build : infos*(keys->values->infos)*(infos->infos->infos)
	     val treeHCons  : bool
	   end)
    -> (struct
	  include S
	  type common = int
	  type branching = int
	  let check k m = (k land m) == 0

	  let lowest_bit x = x land (-x)
	  let branching_bit p0 p1 = lowest_bit (p0 lxor p1)
	  let mask p m = p land (m-1)

	  let match_prefix k p m = (mask k m) == p
	  let commonise p0 p1 = 
	    let m = branching_bit p0 p1 in (mask p0 m,m)
	  let smallerthan n1 n2 = n1 < n2
	end:UserTypes with type keys=S.keys
		      and type values=S.values
		      and type infos=S.infos
       );;

module Memo = 
  functor (S: sig 
	     type keys
	     type common
	     val tag : keys -> common
	     type branching
	     val check       : common->branching->bool
	     val commonise   : common->common->common*branching
	     val match_prefix: common->common->branching->bool
	     val smallerthan : branching->branching->bool 
	     type infos
	     val info_build : infos*(keys->unit->infos)*(infos->infos->infos)	       
	     type values
	     val vcompare : values->values->bool
	   end)-> struct

    module ST = struct
      type keys   = S.keys
      type common = S.common
      let tag     = S.tag
      type branching = S.branching
      let check      = S.check
      let match_prefix = S.match_prefix
      let commonise    = S.commonise
      let smallerthan  = S.smallerthan


      type values = unit
      let vcompare _ _=true 
      type infos = S.infos*keys option
      let info_build = 
	let (a,b,c) = S.info_build in
	  ((a,None),
	   (fun k () ->(b k (),Some k)),
	   (fun (y1,x1) (y2,x2)
	      -> (c y1 y2,
		  match x1,x2 with
		    | None,_ -> x2
		    | _,None -> x1
		    | Some(v1),Some(v2) -> if(tag v1<tag v2)then x1 else x2
		 ))
	  )
      let treeHCons  = true
    end

    module PT = PATSet(ST)

    module PTUser:UserTypes = struct
      type keys = PT.t
      type common = PT.t
      let tag s = s

      type values = ST.values
      let vcompare = ST.vcompare

      type infos = unit
      let info_build = ((),(fun _ _->()),(fun _ _->()))
	
      type branching = ST.keys
      let check k m = PT.mem m k
      let smallerthan n1 n2 = (tag n1) < (tag n2)

      let match_prefix k p m = (PT.subset p k)
	&&( match PT.info(PT.diff k p) with
	      | _,Some x -> not(smallerthan x m)
	      | _        -> true)

      let commonise p0 p1 = 
	let p2 = PT.inter p0 p1 in
	let d0 = let (_,x) = PT.info(PT.diff p0 p1) in x in
	let d1 = let (_,x) = PT.info(PT.diff p1 p0) in x in
	  match (d0,d1) with
	    | Some(e0),Some(e1) -> (p2,if smallerthan e0 e1 then e0 else e1)
	    | _ -> failwith("Impossible!")

      let treeHCons  = false
    end

    module PTT = PATMap(PTUser)

end
