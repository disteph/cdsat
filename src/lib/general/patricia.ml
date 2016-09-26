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

      let equal = match treeHCons with
        | None -> fun t t' -> failwith "No function equal when patricia trees are not HConsed"
        | Some _ -> fun t t' -> t==t'

      let hash = match treeHCons with
        | None -> fun t -> failwith "No function hash when patricia trees are not HConsed"
        | Some _ -> id

                                       
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
        let c,m,b = disagree p0 p1 in
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

      let print_in_fmt ?tree f fmt t =
        let rec aux indent fmt t = match reveal t with
	  | Empty            -> fprintf fmt "{}"
	  | Leaf(j,x)        -> 
            (match tree with
	    | None -> fprintf fmt "%a" f (j,x) 
	    | _    -> fprintf fmt "%t%s%a" indent "   " f (j,x))
	  | Branch(p,m,t0,t1)->
	    match tree with
	    | None     -> let auxd = aux indent in
                          fprintf fmt "%a, %a" auxd t0 auxd t1
	    | Some(g,h)-> let auxd s = aux (fun fmt -> fprintf fmt "%t%a%s%a" indent g p s h m) in
                          fprintf fmt "%a\n%a" (auxd "+") t0 (auxd "-") t1
        in match tree with
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

    let rec map f t = match reveal t with
      | Empty              -> empty
      | Leaf (k,x)         -> leaf (k, f k x)
      | Branch (p,m,t0,t1) -> branch (p, m, map f t0, map f t1)
	                             
    let rec fold f t accu = match reveal t with
      | Empty              -> accu
      | Leaf (k,x)         -> f k x accu
      | Branch (_,_,t0,t1) -> fold f t0 (fold f t1 accu)

    let iter f t =
      let aux k x () = f k x in
      fold aux t ()
             
  (* In merge, union, inter, subset, diff,
     argument f says what to do in case a key is assigned a value in both u1 and u2 *)

    let prefix t = match reveal t with
      | Empty -> failwith "Patricia.prefix: empty"
      | Leaf(k,_) -> tag k
      | Branch(p,_,_,_) -> p

    let join u1 u2 =
      if is_empty u1 then u2
      else if is_empty u2 then u1
      else join (prefix u1, u1, prefix u2, u2)

    type ('v1,'i1,'v2,'i2,'a) merge = {
        sameleaf  : keys -> 'v1 -> 'v2 -> 'a;
        emptyfull : ('v2,'i2) param -> 'a;
        fullempty : ('v1,'i1) param -> 'a;
        combine   : 'a -> 'a -> 'a
      }

    let disjoint action s1 s2 =
      action.combine (action.fullempty s1) (action.emptyfull s2)
                                     
    let merge_trans reccall action s1 s2 =
      match reveal s1, reveal s2 with

      | Empty, _ -> action.emptyfull s2

      | _, Empty -> action.fullempty s1

      | Leaf(k1,x1), Leaf(k2,x2) ->
	 if  kcompare k1 k2 ==0
         then action.sameleaf k1 x1 x2
	 else disjoint action s1 s2

      | Leaf(k,x), Branch(p,m,t1,t2) ->
         let tagk = tag k in
	  if match_prefix tagk p m then
	    if check tagk m then 
	      action.combine (reccall s1 t1) (action.emptyfull t2)
	    else
	      action.combine (action.emptyfull t1) (reccall s1 t2)
	  else
	    disjoint action s1 s2
                
      | Branch(p,m,t1,t2), Leaf(k,x) ->
         let tagk = tag k in
	  if match_prefix tagk p m then
	    if check tagk m then 
	      action.combine (reccall t1 s2) (action.fullempty t2)
	    else
	      action.combine (action.fullempty t1) (reccall t2 s2)
	  else
	    disjoint action s1 s2

      | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	 if (bcompare m1 m2==0) && match_prefix p1 p2 m1 then 
	   action.combine (reccall l1 l2) (reccall r1 r2)
	 else if bcompare m1 m2<0 && match_prefix p2 p1 m1 then
	   if check p2 m1
           then action.combine (reccall l1 s2) (action.fullempty r1)
	   else action.combine (action.fullempty l1) (reccall r1 s2)
	 else if bcompare m2 m1<0 && match_prefix p1 p2 m2 then
	   if check p1 m2
           then action.combine (reccall s1 l2) (action.emptyfull r2)
	   else action.combine (action.emptyfull l2) (reccall s1 r2)
	 else
	    disjoint action s1 s2

    let merge_poly action s1 s2 =
      let rec aux s1 s2 = merge_trans aux action s1 s2
      in aux s1 s2

    let merge =
      if is_hcons
      then
        (fun ?equal action ->
          match equal with
          | Some f ->
             let rec aux s1 s2 =
               if s1==s2 then f s1 else merge_trans aux action s1 s2
             in aux
          | None -> merge_poly action )
      else
        (fun ?equal action ->
          match equal with
          | Some f -> failwith "Patricia tries not hconsed"
          | None -> merge_poly action )
          
    let union_poly_action f emptyfull fullempty = {
        sameleaf = (fun k v1 v2 -> singleton k (f k v1 v2));
        emptyfull = emptyfull;
        fullempty = fullempty;
        combine   = join
      }
                           
    let union_poly f fullempty emptyfull s1 s2 = merge_poly (union_poly_action f emptyfull fullempty) s1 s2

    let union_action f = {
        sameleaf = (fun k v1 v2 -> singleton k (f v1 v2));
        emptyfull = (fun a -> a);
        fullempty = (fun a -> a);
        combine   = join
      }

    let union =
      if is_hcons
      then (fun f s1 s2 -> merge ~equal:(fun a->a) (union_action f) s1 s2)
      else (fun f s1 s2 -> merge_poly (union_action f) s1 s2)


    let inter_action f = {
        sameleaf = (fun k v1 v2 -> singleton k (f k v1 v2));
        emptyfull = (fun _ -> empty);
        fullempty = (fun _ -> empty);
        combine   = join
      }
                           
    let inter_poly f s1 s2 = merge_poly (inter_action f) s1 s2

    let inter =
      if is_hcons
      then (fun f s1 s2 -> merge ~equal:(fun a->a) (inter_action f) s1 s2)
      else inter_poly

    let diff_action f = {
        sameleaf  = f;
        emptyfull = (fun _ -> empty);
        fullempty = (fun a -> a);
        combine   = join
      }
                           
    let diff_poly f s1 s2 = merge_poly (diff_action f) s1 s2

    let diff =
      if is_hcons
      then (fun f s1 s2 -> merge ~equal:(fun a->empty) (diff_action f) s1 s2)
      else diff_poly

    let sub_action f = {
        sameleaf = (fun _ x y -> f x y);
        emptyfull = (fun _ -> true);
        fullempty = (fun _ -> false);
        combine   = (&&)
      }
                           
    let subset_poly f s1 s2 = merge_poly (sub_action f) s1 s2

    let subset =
      if is_hcons
      then (fun f s1 s2 -> merge ~equal:(fun a->true) (sub_action f) s1 s2)
      else subset_poly

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
        | Empty      -> Case2 empty
        | Leaf (j,x) -> (match su (tag j) k None with
	  | Yes _                -> Case1(yes j x) 
	  | Almost n when cond n -> Case2(singleton j x n)
	  | _                    -> Case2 empty)
        | Branch (p,m,l,r) ->
	  let (prems,deuz)=if bp then (r,l) else (l,r) in
	  let f b = match aux prems with
	    | Case2 c when b c ->(match aux deuz with
	      | Case2 d -> Case2(union c d)
	      | v   -> v)
	    | v -> v
	  in match su p k (Some m) with
	  | Yes _                -> f(fun c-> cond' c &&((bp=check k m)||cond m))
	  | Almost n when cond n -> f(fun c-> cond' c &&  bp=check k m)
	  | _                    -> Case2 empty
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
    let inter_poly a b = PM.inter_poly (fun _ _ _ -> ()) a b
    let subset: t -> t -> bool = PM.subset(fun _ _ -> true)
    let diff       = PM.diff  (fun _ _ _ -> empty)
    let diff_poly a b = PM.diff_poly (fun _ _ _ -> empty) a b
    let first_diff = PM.first_diff (fun _ ()()->(None,true)) D.kcompare
    let sub        = PM.sub (fun alm k () -> function
      | Some()         -> Yes()
      | None  when alm -> Almost k
      | _              -> No)
    let iter f a   = PM.iter (fun k x -> f k) a
    let map f a    = PM.map  (fun k x -> f k) a
    let fold f a init = PM.fold (fun k x -> f k) a init
    let choose t   = let (k,_) = PM.choose t in k
    let elements s = List.map (function (k,x)->k) (PM.elements s)
    let find_su yes single = PM.find_su (fun j () -> yes j) (fun j () m->single j m)

  (* Now starting functions specific to Sets, without equivalent
     ones for Maps *)

    let print_in_fmt ?tree f fmt a = print_in_fmt ?tree (fun fmt (x,y)->f fmt x) fmt a

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
