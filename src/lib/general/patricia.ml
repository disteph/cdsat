open Format

open Sums

open Patricia_interfaces

let pattreenum = ref 0

type ('keys,'values,'common,'branching,'infos) poly =
  {reveal: (('keys,'values,'common,'branching,'infos) poly,
            'keys,
            'values,
            'common,
            'branching) poly_rev ;
   id:int ;
   info: 'infos}


module Poly(I:Intern) = struct

  open I

  module ToInclude = struct
    type ('v,'i) param = (keys,'v,common,branching,'i) poly
  end
    
  let equal rec_eq kcompare vequal t1 t2 =
    match t1.reveal,t2.reveal with
    | Empty, Empty                             -> true
    | Leaf(key1,value1), Leaf(key2,value2)     -> (kcompare key1 key2==0) && (vequal value1 value2)
    | Branch(c1,b1,t3,t3'),Branch(c2,b2,t4,t4')-> (rec_eq t3 t4)&&(rec_eq t3' t4')&&(bcompare b1 b2==0)&&(match_prefix c1 c2 b1)
    | _                                        -> false

  let hash rec_hash khash vhash t1 = match t1.reveal with
    | Empty            -> 1
    | Leaf(key,value)  -> 2*(khash key)+3*(vhash value)
    | Branch(_,b,t2,t3)-> 5*(rec_hash t2)+7*(rec_hash t3)


  module MapMake(D:MapDestType with type keys=I.keys) = struct

  (* This module BackOffice will be share by the Patricia tree
     structure for sets, below *)
    module BackOffice = struct

      include D
      include ToInclude
      type branching = I.branching
      type common = I.common

      (* Our Patricia trees can be HConsed *)

      let is_hcons = match treeHCons with
        | None -> false
        | Some _ -> true

      let reveal f = f.reveal
      let id f     = f.id
      let info f   = f.info

      (* Primitive type of Patricia trees to feed the HashTable Make
         functor *)

      module PATPrimitive = struct

        type t = (keys,values,common,branching,infos) poly

        let equal =
          match treeHCons with
          | None -> fun t t' -> failwith "No function equal when patricia trees are not HConsed"
          | Some(_,_,vequal) -> equal (fun t t' -> t==t') kcompare vequal

        let hash = match treeHCons with
          | None -> fun t -> failwith "No function hash when patricia trees are not HConsed"
          | Some(khash,vhash,_) -> hash id khash vhash

      end

      include PATPrimitive

      type pat = (t,keys,values,common,branching) poly_rev

      let rec size t = match reveal t with
        | Empty               -> 0
        | Leaf (j,_)          -> 1
        | Branch (_, _, l, r) -> size l + size r


      let info_gen = function
        | Empty            -> info_build.empty_info
        | Leaf(k,x)        -> info_build.leaf_info k x
        | Branch(_,_,t0,t1)-> info_build.branch_info (info t0) (info t1)  

      module H = Hashtbl.Make(PATPrimitive)

      let num = !pattreenum

      let table = incr pattreenum; H.create 5003 

      let uniqq =ref 0
      let build a =
        let f = {reveal =  a; id = !uniqq ; info = info_gen a} in
	match treeHCons with
        | None   -> f
        | Some _ ->
	  try H.find table f
	  with Not_found -> incr uniqq; H.add table f f; f

      let clear() = uniqq := 0;H.clear table; let _ = build Empty in ()

      let compare t1 t2 = 
        match treeHCons with 
        | None   -> failwith "No function compare when patricia trees are not HConsed"
        | Some _ -> Pervasives.compare (id t1) (id t2)

    (* Now we start the standard functions on maps/sets *)

      let rec checktree aux t = match reveal t with
        | Empty               -> true
        | Leaf (j,_)          -> List.fold_left (fun b m -> let g =(check (tag j) m) in if not g then print_endline("Warning leaf"); b&&g) true aux
        | Branch (_, m, l, r) ->
    	  let aux' = m::aux in
    	  let o = match aux with
    	    | []   -> true
    	    | a::l when bcompare a m < 0 -> true
    	    | _    -> false
    	  in
    	  if not o then print_endline("Warning Branch");
    	  o&&(checktree aux r)&&(checktree aux' l)

      let is_empty t = match reveal t with Empty -> true | _ -> false

      let rec mem k t = match reveal t with
        | Empty               -> false
        | Leaf (j,_)          -> kcompare k j == 0
        | Branch (_, m, l, r) -> mem k (if check (tag k) m then l else r)

      let rec find k t = match reveal t with
        | Empty               -> raise Not_found
        | Leaf (j,x)          -> if kcompare k j == 0 then x else raise Not_found
        | Branch (_, m, l, r) -> find k (if check (tag k) m then l else r)

      let rec cardinal t = match reveal t with
        | Empty  -> 0
        | Leaf _ -> 1
        | Branch (_,_,t0,t1) -> cardinal t0 + cardinal t1

    (* Smart constructors, using both the HConsing techniques and
       assuring some invariant of Patricia trees *)

      let empty      = build Empty
      let leaf (k,x) = build (Leaf(k,x))
      let singleton k x = leaf(k,x)
      let branch = function
        | (_,_,e,t) when (reveal e=Empty) -> t
        | (_,_,t,e) when (reveal e=Empty) -> t
        | (c,b,t0,t1)   -> build(Branch (c,b,t0,t1))

    (* Assumed in function join:
       p0 is the common part of tree t0
       p1 is the common part of tree t1
       p0 and p1 are not equal *)

      let join (p0,t0,p1,t1) =
        let (c,m,b) = disagree p0 p1 in
	if b then branch (c, m, t0, t1) else branch (c, m, t1, t0)

    (* remove_aux function: argument f says what to do in case the key is found *)

      let remove_aux f k t =
        let rec rmv t = match reveal t with
	  | Empty      -> failwith "Remove: Was not there -empty"
	  | Leaf (j,x) -> if  kcompare k j == 0 then f k x else failwith "Remove: Was not there -leaf"
	  | Branch (p,m,t0,t1) ->
	    if match_prefix (tag k) p m then
	      if check (tag k) m then branch (p, m, rmv t0, t1)
              else branch (p, m, t0, rmv t1)
	    else failwith "Remove: Was not there -branch"
        in rmv t

    (* remove function: argument f of remove_aux says "delete the key altogether" *)

      let remove = remove_aux (fun _ _ -> empty)

    (* argument f says how to print a key,
       argument b decides whether to print the map as a list [b=None]
       or as a tree [b=Some(g,h)], with g printing prefixes and h printing branchings *)

      let print_in_fmt b f fmt t =
        let rec aux indent fmt t = match reveal t with
	  | Empty            -> fprintf fmt "{}"
	  | Leaf(j,x)        -> 
            (match b with
	    | None -> fprintf fmt "%a" f (j,x) 
	    | _    -> fprintf fmt "%t%s%a" indent "   " f (j,x))
	  | Branch(p,m,t0,t1)->
	    match b with
	    | None     -> let auxd = aux indent in
                          fprintf fmt "%a,%a" auxd t0 auxd t1
	    | Some(g,h)-> let auxd s = aux (fun fmt -> fprintf fmt "%t%a%s%a" indent g p s h m) in
                          fprintf fmt "%a\n%a" (auxd "+") t0 (auxd "-") t1
        in match b with
	| None   -> aux (fun fmt -> ()) fmt t
	| Some _ -> fprintf fmt "\n%a" (aux (fun fmt -> ())) t


  (* Now we have finished the material that is common to Maps AND Sets,
     closing module BackOffice *)
    end

    include BackOffice

  (* Now starting functions specific to Maps, not Sets *)

  (* argument f says what to do in case a binding is already found *)

    let add k f t =
      let rec ins t = match reveal t with
        | Empty      -> leaf(k,f None)
        | Leaf (j,y) ->
	  if  kcompare k j ==0 then leaf (k,f (Some y))
	  else join(tag k, leaf(k,f None), tag j, t)
        | Branch (c,b,t0,t1) ->
	  if match_prefix (tag k) c b then
	    if check (tag k) b then 
	      branch (c,b, ins t0, t1)
	    else
	      branch (c,b, t0, ins t1)
	  else
	    join (tag k, leaf(k,f None), c, t)
      in ins t

  (* In merge, union, inter, subset, diff,
     argument f says what to do in case a key is assigned a value in both u1 and u2 *)

    let rec merge f (u1,u2) =
      let newf x = function
        | None  -> x
        | Some y-> f x y
      in match reveal u1,reveal u2 with
      | Empty, _     -> u2
      | _, Empty     -> u1
      | Leaf(k,x), _ -> add k (newf x) u2
      | _, Leaf(k,x) -> add k (newf x) u1
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

    let inter_trans reccall f s1 s2 =
        match reveal s1, reveal s2 with
      | Empty, _      -> empty
      | _, Empty      -> empty
      | Leaf(k,x), _  -> if mem k s2 then let y = find k s2 in leaf(k,f k x y) else empty
      | _, Leaf(k,y)  -> if mem k s1 then let x = find k s1 in leaf(k,f k x y) else empty
      | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	if (bcompare m1 m2==0) && match_prefix p1 p2 m1 then 
	  union (fun _ -> failwith("Should not be called")) (reccall f l1 l2) (reccall f r1 r2)
	else if bcompare m1 m2<0 && match_prefix p2 p1 m1 then
	  reccall f (if check p2 m1 then l1 else r1) s2
	else if bcompare m2 m1<0 && match_prefix p1 p2 m2 then
	  reccall f s1 (if check p1 m2 then l2 else r2)
	else
	  empty

    let rec inter f s1 s2 =
      if is_hcons && s1==s2 then s1 else inter_trans inter f s1 s2

    let rec inter_poly f s1 s2 = inter_trans inter_poly f s1 s2

    let diff_trans reccall f s1 s2 =
      match reveal s1, reveal s2 with
      | Empty, _      -> empty
      | _, Empty      -> s1
      | Leaf(k,x), _  -> if mem k s2 then let y = find k s2 in f k x y else s1
      | _, Leaf(k,y)  -> if mem k s1 then remove_aux (fun k x -> f k x y) k s1 else s1
      | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	 if (bcompare m1 m2==0) && match_prefix p1 p2 m1 then
	   union (fun _ -> failwith("Should not be called")) (reccall f l1 l2) (reccall f r1 r2)
	 else if bcompare m1 m2<0 && match_prefix p2 p1 m1 then
	   if check p2 m1 then 
	     union (fun _ -> failwith("Should not be called")) (reccall f l1 s2) r1 
	   else 
	     union (fun _ -> failwith("Should not be called")) l1 (reccall f r1 s2)
	 else if bcompare m2 m1<0 && match_prefix p1 p2 m2 then
	   if check p1 m2 then reccall f s1 l2 else reccall f s1 r2
	 else
	   s1

    let rec diff f s1 s2 =
      if is_hcons && s1==s2 then empty else diff_trans diff f s1 s2

    let rec diff_poly f s1 s2 = diff_trans diff_poly f s1 s2

    let rec subset f s1 s2 =
      if is_hcons && s1==s2 then true
      else match reveal s1, reveal s2 with
      | Empty, _            -> true
      | _, Empty            -> false
      | Leaf(k,x), _        -> mem k s2 &&(let y = find k s2 in f x y)
      | Branch _, Leaf(k,_) -> false
      | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	 if (bcompare m1 m2==0) && match_prefix p1 p2 m1 then
	   subset f l1 l2 && subset f r1 r2
	 else if bcompare m2 m1 < 0 && match_prefix p1 p2 m2 then
	   if check p1 m2 then 
	     subset f l1 l2 && subset f r1 l2
	   else 
	     subset f l1 r2 && subset f r1 r2
	 else
	   false


  (* Advanced version of subset, returning 
     Yes()     if it is a subset,
     No        if not
     If alm=true, it can also output
     Almost(a) if it is almost a subset, were it not for element a
     Almost may only be produced by the call to f 
     
     We start with an auxiliary function for sub below *)

    let aux_and v1 v2 alm = match v1 alm with
      | Yes()    -> (*print_endline("left Yes");*)v2 alm
      | Almost x -> ((*print_endline("left Almost");*)match v2 false with Yes() -> Almost x | _-> No)
      | No       -> (*print_endline("left No");*)No

    let sub f locprune alm s1 s2 =
      let rec aux s1 s2 alm=
      if is_hcons && s1==s2 then Yes() else
        match reveal(locprune s1), reveal(locprune s2) with
        | Empty, _                        -> Yes()
        | Leaf(k,x), _      when mem k s2 -> let y = find k s2 in f alm k x (Some y)
        | Leaf(k,x), _                    -> f alm k x None
        | Branch(_,_,l,r),Leaf _ when alm -> aux_and(aux l s2)(aux r s2)true
        | Branch(p1,m1,l1,r1),Branch (p2,m2,l2,r2) when bcompare m1 m2==0 && match_prefix p1 p2 m1
	    -> aux_and(aux l1 l2)(aux r1 r2) alm
        | Branch(p1,m1,l1,r1),Branch (p2,m2,l2,r2) when bcompare m2 m1 < 0 && match_prefix p1 p2 m2
	    -> if check p1 m2 then 
	        aux_and(aux l1 l2)(aux r1 l2)alm
	      else 
	        aux_and(aux l1 r2)(aux r1 r2)alm
        | Branch(p1,m1,l1,r1),Branch (p2,m2,l2,r2) when bcompare m1 m2 < 0 && match_prefix p2 p1 m1
	    -> if check p2 m1 then 
	        aux_and(aux r1 empty)(aux l1 s2)alm
	      else 
	        aux_and(aux l1 empty)(aux r1 s2)alm
        | _ -> No
      in aux s1 s2 alm

  (* first_diff indicates where 2 patricia trees s1 and s2 start
     differing: It produces (g,b), where
     * g indicates the smallest element -if it exists- contained in
     one map and not the other (g=None is the two maps are equal),
     according to order cfompare.
     * b indicates whether this element is contained in s1 [true] or
     s2 [false]

     It requires min, that computes the smallest element of a map
     according to that order, and f, that says what to do if a key is
     found in the two maps, possibly with different values assigned.

     The notion of "element contained in a map" is deliberately vague,
     it can be taylored to your needs by choosing f, min and
     cfompare. 

     We start with the obvious lifting of cfompare to option types. *)

    let opt_st cfompare = function
      | None,None    -> 0
      | None,_       -> 1
      | _, None      -> -1
      | Some a,Some a' -> cfompare a a'

    let first_diff f cfompare min s1 s2 =
      let ocompare = opt_st cfompare in
      let select (d1,b1)(d2,b2)= if ocompare(d1,d2)<0 then (d1,b1) else (d2,b2) in
      let rec aux s1 s2 =
        let (m1,m2)=(min s1,min s2) in
	if ocompare (m1,m2) ==0 then match reveal s1,reveal s2 with
	| Empty,Empty -> (None,true)
	| Leaf(k,x), _  when mem k s2 -> (let y = find k s2 in match f k x y with
	  | (None,_) -> (min(remove k s2),false)
	  | a        -> a)
	| _,Leaf _     -> let (b,c) = aux s2 s1 in (b,not c)
	| Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	  let (rec1,rec2,i)=
	    if (bcompare m1 m2==0) &&  match_prefix p1 p2 m1 then 
	      (aux l1 l2,aux r1 r2,1)
	    else if bcompare m1 m2<0 && match_prefix p2 p1 m1 then
	      let (friend,foe) = if check p2 m1 then (l1,r1) else (r1,l1) in
	      (aux friend s2,aux foe empty,2)
	    else if bcompare m2 m1<0 && match_prefix p1 p2 m2 then
	      let (friend,foe) = if check p1 m2 then (l2,r2) else (r2,l2) in
	      (aux s1 friend,aux empty foe,3)
	    else (aux s1 empty,aux empty s2,4)
	  in select rec1 rec2
	| _ -> failwith("Should not happen, mins must be the same!")
	else select (m1,true) (m2,false)
      in aux s1 s2

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
	
    let make f l     = List.fold_right (function (k,x)->add k (f x)) l empty

    let elements s =
      let rec elements_aux acc t = match reveal t with
        | Empty      -> acc
        | Leaf(k,x) -> (k,x) :: acc
        | Branch (_,_,l,r) -> elements_aux (elements_aux acc l) r
      in
      elements_aux [] s

  (* find_su looks for element in a patricia tree, in a completely modular way. *)

    let find_su yes singleton empty union su bp cond cond' k t =
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

  module SetMake(D:SetDestType with type keys=I.keys) = struct

  (* A Set is just a Map with codomain unit.  Constructing this Map
     structure *)

    module MapDest = struct
      type keys    = D.keys
      let kcompare = D.kcompare
      type values  = unit
      type infos   = D.infos
      let info_build = D.info_build
      let treeHCons  = match D.treeHCons with
        | None       -> None
        | Some khash -> Some(khash, 
                             (fun _   -> 0),
                             (fun _ _ -> true))
    end

    module PM = MapMake(MapDest)

    include PM.BackOffice
    
    type e = keys

  (* Now starting functions specific to Sets, not Maps.
     Starting with similar functions *)

    let singleton k= PM.singleton k ()
    let add k t    = PM.add k (fun _ -> ()) t
    let union      = PM.union (fun _ _ -> ())
    let inter      = PM.inter (fun _ _ _ -> ())
    let inter_poly a = PM.inter_poly (fun _ _ _ -> ()) a
    let subset: t -> t -> bool = PM.subset(fun _ _ -> true)
    let diff       = PM.diff  (fun _ _ _ -> empty)
    let diff_poly a= PM.diff_poly (fun _ _ _ -> empty) a
    let first_diff = PM.first_diff (fun _ ()()->(None,true)) D.kcompare
    let sub        = PM.sub (fun alm k () -> function
      | Some()         -> Yes()
      | None  when alm -> Almost k
      | _              -> No)
    let iter f     = PM.iter (fun k x -> f k)
    let map f      = PM.map  (fun k x -> f k)	  
    let fold f     = PM.fold (fun k x -> f k)
    let choose t   = let (k,_) = PM.choose t in k
    let elements s = List.map (function (k,x)->k) (PM.elements s)
    let find_su yes single = PM.find_su (fun j () -> yes j) (fun j () m->single j m)

  (* Now starting functions specific to Sets, without equivalent
     ones for Maps *)

    let print_in_fmt b f = print_in_fmt b (fun fmt (x,y)->f fmt x)

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


end


module PATSet = struct

  module type S = PATSetType

  module Make(D:SetDestType)(I:Intern with type keys=D.keys) = struct
      module Base = Poly(I)
      include Base.SetMake(D)
  end

end

module PATMap = struct

  module type S = PATMapType

  module Make(D:MapDestType)(I:Intern with type keys=D.keys) = struct
      module Base = Poly(I)
      include Base.MapMake(D)
  end

end



let empty_info_build = {
  empty_info  = ();
  leaf_info   = (fun _ _ ->());
  branch_info = (fun _ _-> ())
}

type 'a m_infos = 'a option
let m_info_build kcompare = {
  empty_info  = None;
  leaf_info   = (fun x _ -> Some x);
  branch_info = (fun x1 x2 -> match x1,x2 with
  | None,_ -> failwith "Bad1"
  | _,None -> failwith "Bad2"
  | Some(v1),Some(v2)-> if kcompare v1 v2<0 then x1 else x2
  )
}

let c_info_build = {
  empty_info  = 0;
  leaf_info   = (fun x _ -> 1);
  branch_info = (fun x1 x2 -> x1 + x2)
}

type 'keys mmc_infos = ('keys option)*('keys option)*int
let mmc_info_build kcompare = {
  empty_info  = (None,None,0);
  leaf_info   = (fun x _ ->(Some x,Some x,1));
  branch_info = (fun (x1,y1,z1) (x2,y2,z2)
     -> ((match x1,x2 with
	      None,_ -> x2
	    | _,None -> x1
	    | Some(v1),Some(v2)-> if kcompare v1 v2 < 0 then x1 else x2),
	 (match y1,y2 with
	      None,_ -> y2
	    | _,None -> y1
	    | Some(v1),Some(v2)-> if kcompare v1 v2 > 0 then y1 else y2),
	 z1+z2))
}

type 'a mm_infos = ('a*('a option)) option
let mm_info_build kcompare = {
  empty_info  = None;
  leaf_info   = (fun x _ -> Some (x,None));
  branch_info = (fun x1 x2 -> match x1,x2 with
  | None,_ -> failwith "Bad1"
  | _,None -> failwith "Bad2"
  | Some(v1,g1),Some(v2,g2) ->
    let (first,loose,nknow) =
      if compare v1 v2<0
      then (v1,v2,g1) else (v2,v1,g2)
    in let second = match nknow with
    | Some h when compare h loose<0 -> h
    | _      -> loose
       in Some(first,Some second))
}
