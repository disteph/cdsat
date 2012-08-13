(* Standard type constructs *)
type ('a,'b) sum = A of 'a | F of 'b
type ('a,'b) almost =
  | Yes of 'a
  | Almost of 'b
  | No


(* Material that must be provided to construct a Patricia tree
   structure *)

(* Info that you need to provide anyway *)

module type Dest = sig

  (* Domain of the map/set (keys)*)
  type keys

  (* Co-domain of the map (values), set it to unit for a set *)    
  type values
  val vcompare   : values->values->int

  (* Allows to store information about the Patricia tree
     - typically, number of bindings stored, etc *) 
  type infos
    
  (* Provides info for empty tree, singleton tree, and disjoint union
     of two tree *)
  val info_build : infos*(keys->values->infos)*(infos->infos->infos)

  (* Do you want the patricia trees hconsed? *)
  val treeHCons   : bool
end


(* Structures for the internal use of the Patricia trees. *)

module type Intern = sig

  (* Implementation of keys and how to compute them.
     Typically for a HConsed keys type, common = int *)
  type keys
  type common
  val ccompare   : common->common->int
  val tag        : keys->common
    
  (* branching is the type of data used for discriminating the keys
     (themselves represented in common via tag) *) 
  type branching
  val bcompare    : branching->branching->int

  (* check discriminates its first argument over second *)
  val check       : common->branching->bool

  (* Given two elements of common, disagree outputs:
     their common part,
     the first branching data that discriminates them
     a boolean saying whether that data was in the first [true] or
     second [false] argument *)
  val disagree    : common->common->common*branching*bool

  (* Checks whether the first argument is compatible with the second
     up to some branching data.
     Should output true if the first two arguments are equal *)
  val match_prefix: common->common->branching->bool

end


(* Construction of a Patricia tree structure for maps, given the above *)

module PATMap (D:Dest)(I:Intern with type keys=D.keys) = struct

  open I
  open D

  (* This module BackOffice will be share by the Patricia tree
     structure for sets, below *)
  module BackOffice = struct

    (* Our Patricia trees can be HConsed *)

    type 'a pat =
      | Empty
      | Leaf of keys * values
      | Branch of common * branching * 'a * 'a

    (* Primitive type of Patricia trees to feed the HashTable Make
       functor *)

    module PATPrimitive = struct
      type t = {reveal: t pat ; id:int ; info: infos}

      let equal t1 t2 = if not treeHCons then (t1=t2) else
	match t1.reveal,t2.reveal with
	  | Empty, Empty                             -> true
	  | Leaf(key1,value1), Leaf(key2,value2)     -> (ccompare (tag key1)(tag key2)==0) && (vcompare value1 value2==0)
	  | Branch(c1,b1,t3,t3'),Branch(c2,b2,t4,t4')-> (ccompare c1 c2==0) && (bcompare b1 b2==0) && t3==t4 && t3'==t4'
	  | _                                        -> false 

      let hash t1 = if not treeHCons then Hashtbl.hash t1 else
	match t1.reveal with
	  | Empty            -> 1
	  | Leaf(key,value)  -> 2*(Hashtbl.hash key)+3*(Hashtbl.hash value)
	  | Branch(c,b,t2,t3)-> 5*(Hashtbl.hash c)+7*(Hashtbl.hash b)+11*t2.id+13*t3.id

    end

    include PATPrimitive

    let reveal f = f.reveal
    let id f     = f.id
    let info f   = f.info

    let info_gen = let (a,b,c)=info_build in function
      | Empty    -> a
      | Leaf(k,x)-> b k x
      | Branch(_,_,t0,t1)-> c t0.info t1.info  

    module H = Hashtbl.Make(PATPrimitive)
    let table = H.create 5003 
    let uniq =ref 0
    let build a =
      let f = {reveal =  a; id = !uniq ; info = info_gen a} in
	if treeHCons then
	  try H.find table f
	  with Not_found ->  (* print_endline(string_of_int(!uniq)); *)
	    incr uniq; H.add table f f; f
	else f

    let compare t1 t2 = Pervasives.compare (id t1) (id t2) 

    (* Now we start the standard functions on maps/sets *)

    let is_empty t = match reveal t with Empty -> true | _ -> false

    let rec mem k t = match reveal t with
      | Empty -> false
      | Leaf (j,_) -> ccompare (tag k) (tag j) ==0
      | Branch (_, m, l, r) -> mem k (if check (tag k) m then l else r)

    let rec find k t = match reveal t with
      | Empty -> raise Not_found
      | Leaf (j,x) -> if ccompare (tag k) (tag j) ==0 then x else raise Not_found
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
      | (_,_,e,t) when (reveal e=Empty) -> t
      | (_,_,t,e) when (reveal e=Empty) -> t
      | (c,b,t0,t1)   -> build(Branch (c,b,t0,t1))

    (* Assumed in function join:
       p0 is the common part of tree t0
       p1 is the common part of tree t1 *)

    let join (p0,t0,p1,t1) =
      let (c,m,b) = disagree p0 p1 in
	if b then branch (c, m, t0, t1) else branch (c, m, t1, t0)

    let remove_aux f k t =
      let rec rmv t = match reveal t with
	| Empty      -> empty
	| Leaf (j,x) -> if  ccompare (tag k) (tag j) ==0 then f k x else failwith("Was not there")
	| Branch (p,m,t0,t1) -> 
	    if match_prefix (tag k) p m then
	      if check (tag k) m then
		branch (p, m, rmv t0, t1)
	      else
		branch (p, m, t0, rmv t1)
	    else
	      failwith("Was not there")
      in
	rmv t

    let remove = remove_aux (fun _ _ -> empty)

    let rec toString_aux f = function
	[] -> ""
      | a::[] -> (f(a))
      | a::l -> (f(a))^","^(toString_aux f l)

  (* Now we have finished the material that is common to Maps AND Sets,
     closing module BackOffice *)
  end

  include BackOffice

  (* Now starting functions specific to Maps, not Sets *)

  let add f k x t =
    let rec ins t = match reveal t with
      | Empty      -> leaf(k,f x None)
      | Leaf (j,y) -> 
	  if  ccompare (tag k) (tag j) ==0 then 
	    leaf (k,f x (Some y)) 
	  else 
	    join (tag k, leaf(k,f x None), tag j, t)
      | Branch (c,b,t0,t1) ->
	  if match_prefix (tag k) c b then
	    if check (tag k) b then 
	      branch (c,b, ins t0, t1)
	    else
	      branch (c,b, t0, ins t1)
	  else
	    join (tag k, leaf(k,f x None), c, t)
    in
      ins t

  let rec merge f (u1,u2) =
    let newf x = function
      | None   -> x
      | Some(y)-> f x y
    in match reveal u1,reveal u2 with
      | Empty, _  -> u2
      | _, Empty  -> u1
      | Leaf(k,x), _ -> add newf k x u2
      | _, Leaf(k,x) -> add newf k x u1
      | Branch (p,m,s0,s1), Branch (q,n,t0,t1) ->
	  if (bcompare m n==0) && match_prefix q p m then
	    (* The trees have the same prefix. Merge the subtrees. *)
	    branch (p, m, merge f (s0,t0), merge f (s1,t1))
	  else if bcompare m n < 0 && match_prefix q p m then
	    (* [q] contains [p]. Merge [t] with a subtree of [s]. *)
	    if check q m then 
	      branch (p, m, merge f (s0,u2), s1)
            else 
	      branch (p, m, s0, merge f (s1,u2))
	  else if bcompare n m < 0 && match_prefix p q n then
	    (* [p] contains [q]. Merge [s] with a subtree of [t]. *)
	    if check p n then
	      branch (q, n, merge f (u1,t0), t1)
	    else
	      branch (q, n, t0, merge f (u1,t1))
	  else
	    (* The prefixes disagree. *)
	    join (p, u1, q, u2)

  let union f s t = merge f (s,t)

  let rec inter f s1 s2 = match (reveal s1,reveal s2) with
    | Empty, _      -> empty
    | _, Empty      -> empty
    | Leaf(k,x), _  -> if mem k s2 then let y = find k s2 in leaf(k,f x y) else empty
    | _, Leaf(k,y)  -> if mem k s1 then let x = find k s1 in leaf(k,f x y) else empty
    | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	if (bcompare m1 m2==0) && (ccompare p1 p2==0) then 
	  union (fun _ -> failwith("Should not be called")) (inter f l1 l2) (inter f r1 r2)
	else if bcompare m1 m2<0 && match_prefix p2 p1 m1 then
	  inter f (if check p2 m1 then l1 else r1) s2
	else if bcompare m2 m1<0 && match_prefix p1 p2 m2 then
	  inter f s1 (if check p1 m2 then l2 else r2)
	else
	  empty

  let rec subset f s1 s2 = match (reveal s1,reveal s2) with
    | Empty, _             -> true
    | _, Empty             -> false
    | Leaf(k,x), _         -> mem k s2 &&(let y = find k s2 in f x y)
    | Branch _, Leaf(k,_) -> false
    | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	if (bcompare m1 m2==0) && (ccompare p1 p2==0) then
	  subset f l1 l2 && subset f r1 r2
	else if bcompare m2 m1 < 0 && match_prefix p1 p2 m2 then
	  if check p1 m2 then 
	    subset f l1 l2 && subset f r1 l2
	  else 
	    subset f l1 r2 && subset f r1 r2
	else
	  false

  let rec diff f s1 s2 = match (reveal s1,reveal s2) with
    | Empty, _      -> empty
    | _, Empty      -> s1
    | Leaf(k,x), _  -> if mem k s2 then let y = find k s2 in f k x y else s1
    | _, Leaf(k,y)  -> if mem k s1 then remove_aux (fun k x -> f k x y) k s1 else s1
    | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	if (bcompare m1 m2==0) && (ccompare p1 p2==0) then
	  union (fun _ -> failwith("Should not be called")) (diff f l1 l2) (diff f r1 r2)
	else if bcompare m1 m2<0 && match_prefix p2 p1 m1 then
	  if check p2 m1 then 
	    union (fun _ -> failwith("Should not be called")) (diff f l1 s2) r1 
	  else 
	    union (fun _ -> failwith("Should not be called")) l1 (diff f r1 s2)
	else if bcompare m2 m1<0 && match_prefix p1 p2 m2 then
	  if check p1 m2 then diff f s1 l2 else diff f s1 r2
	else
	  s1

  let opt_st = function
    | None,None    -> 0
    | None,_       -> 1
    | _, None      -> -1
    | Some(k,x),Some(k',x') -> let c = ccompare (tag k) (tag k') in
	if c=0 then vcompare x x' else c

  (* Assume min returns the minimal element of a patricia set (according to the total order opt_st).
     first_diff computes, for patricia sets s1 and s2: (b,c)
     where b is the smallest element belonging to one and not the other
     c is true (resp false) if b was in s1 (resp s2).
  *)

  let rec first_diff min s1 s2 = match (reveal s1,reveal s2) with
    | Empty, _      -> (min s2,false)
    | _, Empty      -> let (b,c) = first_diff min s2 s1 in (b,not c)
    | Leaf(k,x), _  -> let i = opt_st(Some(k,x),min s2) in
	if i=0 then (min(remove k s2),false)
	else if i<0 then (Some(k,x),true) else (min s2,false)
    | _,Leaf(_)     -> let (b,c) = first_diff min s2 s1 in (b,not c)
    | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	if (bcompare m1 m2==0) && (ccompare p1 p2==0) then 
	  let (b1,c1) = first_diff min l1 l2 in
	  let (b2,c2) = first_diff min r1 r2 in
	    if opt_st(b1,b2)<0 then (b1,c1) else (b2,c2)
	else if bcompare m1 m2<0 && match_prefix p2 p1 m1 then
	  let (friend,foe) = if check p2 m1 then (l1,r1) else (r1,l1) in
	  let (b,c) = first_diff min friend s2 in
	  let k' = min foe in
	    if opt_st(b,k')<0 then (b,c) else (k',true)
	else if bcompare m2 m1<0 && match_prefix p1 p2 m2 then
	  let (friend,foe) = if check p1 m2 then (l2,r2) else (r2,l2) in
	  let (b,c) = first_diff min s1 friend in
	  let k' = min foe in
	    if opt_st(b,k')<0 then (b,c) else (k',false)
	else
	  let (k1,k2) = (min s1,min s2) in
	    if opt_st(k1,k2)<0 then (k1,true) else (k2,false)

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

  let rec choose t =  match reveal t with
    | Empty      -> raise Not_found
    | Leaf(k,x) -> (k,x)
    | Branch (_, _,t0,_) -> choose t0   (* we know that [t0] is non-empty *)
	  
  let make f l     = List.fold_right (function (k,x)->add f k x) l empty

  let elements s =
    let rec elements_aux acc t = match reveal t with
      | Empty      -> acc
      | Leaf(k,x) -> (k,x) :: acc
      | Branch (_,_,l,r) -> elements_aux (elements_aux acc l) r
    in
      elements_aux [] s

  let toString f t = toString_aux f (elements t)

  (* find_su looks for an element in t that is smaller (if bp) /
     greater (if not) than k, according to order su. 

     Assumption: 
     su p (tag k) [None/Some m] iff for all n [/up to m, excluded],
        (check p n) implies (check (tag k) n) (if bp)
     or (check (tag k) n) implies (check p n) (if not).

     alm=true allows for approximations: the above is wrong for at
     most one such n. In that case find_su returns the collection of
     such p *)

  let find_su su bp cond cond' yes empty singleton union k t =
    let rec aux t = match reveal t with
      | Empty      -> F empty
      | Leaf (j,x) -> (match su (tag j) k None with
			 | Yes _                -> A(yes j x) 
			 | Almost n when cond n -> F(singleton j x n)
			 | _                    -> F empty)
      | Branch (p,m,l,r) ->
	  let (prems,deuz)=if bp then (r,l) else (l,r) in
	  let f b = match aux prems with
	    | F c when b c ->(match aux deuz with
				| F d -> F(union c d)
				| v   -> v)
	    | v -> v
	  in match su p k (Some m) with
	    | Yes _                -> f(fun c-> cond' c &&((bp=check k m)||cond m))
	    | Almost n when cond n -> f(fun c-> cond' c &&  bp=check k m)
	    | _                    -> F empty
    in aux t

end


(* Construction of a Patricia tree structure for sets, given the above.
   Most of it is imported from PATMap *)

module PATSet (D:Dest with type values = unit)(I:Intern with type keys=D.keys) = struct

  (* A Set is just a Map with codomain unit.  Constructing this Map
     structure *)

  module PM = PATMap(struct
		       include D
		       let vcompare _ _ = 0
		     end)(I)

  include PM.BackOffice

  (* Now starting functions specific to Sets, not Maps.
     Starting with similar functions *)

  let singleton k= PM.leaf(k,())
  let add k t    = PM.add   (fun _ _ -> ()) k () t
  let union      = PM.union (fun _ _ -> ())
  let inter      = PM.inter (fun _ _ -> ())
  let subset     = PM.subset(fun _ _ -> true)
  let diff       = PM.diff  (fun _ _ _ -> empty)
  let first_diff min s1 s2 =
    let mapmin s = match min s with
      | None   -> None
      | Some(x)-> Some(x,())
    in
      match PM.first_diff mapmin s1 s2 with
	| (None,b)     -> (None,b)
	| (Some(k,x),b)-> (Some k,b)
 
  let iter f     = PM.iter (fun k x -> f k)
  let map f      = PM.map  (fun k x -> f k)	  
  let fold f     = PM.fold (fun k x -> f k)
  let choose t   = let (k,_) = PM.choose t in k
  let elements s = List.map (function (k,x)->k) (PM.elements s)
  let find_su su bp cond cond' yes empty singleton
      = PM.find_su su bp cond cond' (fun j () -> yes j) empty (fun j () m->singleton j m)

  (* Now starting functions specific to Sets, without equivalent
     ones for Maps *)

  let toString f t = toString_aux f (elements t)

  let make l     = List.fold_right add l empty

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

  let rec elect f t = match reveal t with
    | Empty      -> raise Not_found
    | Leaf(k,()) -> k
    | Branch (_,_,s,t) -> f (elect f s) (elect f t)

end

let empty_info_build = ((),(fun _ _ ->()),(fun _ _-> ()))

type 'a mmc_infos = ('a option)*('a option)*int
let mmc_info_build tag = (
  (None,None,0),
  (fun x _ ->(Some x,Some x,1)),
  (fun (x1,y1,z1) (x2,y2,z2)
     -> ((match x1,x2 with
	      None,_ -> x2
	    | _,None -> x1
	    | Some(v1),Some(v2)-> if(tag v1<tag v2)then x1 else x2),
	 (match y1,y2 with
	      None,_ -> y2
	    | _,None -> y1
	    | Some(v1),Some(v2)-> if(tag v1>tag v2)then y1 else y2),
	 z1+z2))
)

type 'a m_infos = 'a option
let m_info_build tag ccompare = (
  None,
  (fun x _ -> Some x),
  (fun x1 x2
     -> match x1,x2 with
	 None,_ -> x2
       | _,None -> x1
       | Some(v1),Some(v2)-> if ccompare (tag v1) (tag v2)<0 then x1 else x2)
)
