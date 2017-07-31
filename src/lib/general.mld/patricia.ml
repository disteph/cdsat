open Format
open Sums
open Patricia_interfaces

let pattreenum = ref 0

(* Direct type of Patricia tries. 'a is the type of the sub-tries *)
type ('a,'keys,'values,'common,'branching) poly_rev =
  | Empty
  | Leaf of 'keys * 'values
  | Branch of 'common * 'branching * 'a * 'a

(* Type of Patricia tries used in hash-consing *)
type ('keys,'values,'common,'branching,'infos) poly =
  {reveal: (('keys,'values,'common,'branching,'infos) poly,
            'keys,
            'values,
            'common,
            'branching) poly_rev ;
   id:int ;
   info: 'infos}

let print_nothing _ _ = ()
let print_dot fmt _ = fprintf fmt "."

module Poly(I:Intern) = struct

  open I

  module ToInclude = struct
    type ('v,'i) param = (keys,'v,common,branching,'i) poly
    type branching = I.branching
    type common = I.common
    let reveal f = f.reveal
    let info f   = f.info
    let rec checktree aux t = match reveal t with
      | Empty               -> true
      | Leaf (j,_)          ->
         List.fold (fun m b ->
             let g = check (tag j) m in
             if not g then print_endline("Warning leaf");
             b&&g)
           aux true
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

    let rec cardinal t = match reveal t with
      | Empty  -> 0
      | Leaf _ -> 1
      | Branch (_,_,t0,t1) -> cardinal t0 + cardinal t1

  end
    
  let equal rec_eq kcompare vequal t1 t2 =
    match t1.reveal,t2.reveal with
    | Empty, Empty                             -> true
    | Leaf(key1,value1), Leaf(key2,value2)     -> (kcompare key1 key2=0) && (vequal value1 value2)
    | Branch(c1,b1,t3,t3'),Branch(c2,b2,t4,t4')-> (rec_eq t3 t4)&&(rec_eq t3' t4')&&(bcompare b1 b2=0)&&(match_prefix c1 c2 b1)
    | _                                        -> false

  let hash rec_hash khash vhash t1 = match t1.reveal with
    | Empty            -> 1
    | Leaf(key,value)  -> 2*(khash key)+3*(vhash value)
    | Branch(_,b,t2,t3)-> 5*(rec_hash t2)+7*(rec_hash t3)


  module MapMake(K:MapArg with type t=I.keys) = struct

  (* This module BackOffice will be share by the Patricia tree
     structure for sets, below *)
    module BackOffice = struct

      include ToInclude
      type keys   = K.t
      type values = K.values
      type infos  = K.infos

      (* Our Patricia trees can be HConsed *)

      let is_hcons = match K.treeHCons with
        | None -> false
        | Some _ -> true

      let id =
        if is_hcons then fun f -> f.id
        else fun _ -> failwith "No function id when patricia trees are not HConsed"

      (* Primitive type of Patricia trees to feed the HashTable Make
         functor *)

      module PATPrimitive = struct

        type t = (keys,values,common,branching,infos) poly

        let equal =
          match K.treeHCons with
          | None -> fun t t' -> failwith "No function equal when patricia trees are not HConsed"
          | Some(_,_,vequal) -> equal (==) K.compare vequal

        let hash = match K.treeHCons with
          | None -> fun t -> failwith "No function hash when patricia trees are not HConsed"
          | Some(khash,vhash,_) -> hash id khash vhash

      end

      type t = PATPrimitive.t

      let info_gen = function
        | Empty            -> K.info_build.empty_info
        | Leaf(k,x)        -> K.info_build.leaf_info k x
        | Branch(_,_,t0,t1)-> K.info_build.branch_info (info t0) (info t1)  

      module H = Hashtbl.Make(PATPrimitive)

      let num = !pattreenum

      let table = incr pattreenum; H.create 5003 

      let uniqq =ref 0
      let build =
        let f a = {reveal =  a; id = !uniqq ; info = info_gen a} in
        if is_hcons
        then (fun a ->
          let f = f a in
          try H.find table f
	  with Not_found -> incr uniqq; H.add table f f; f)
        else f

      let clear() = uniqq := 0;H.clear table; let _ = build Empty in ()

      let compare =
        if is_hcons then fun a b -> id2compare id a b
        else fun _ _ -> failwith "No function compare when patricia trees are not HConsed"

      let equal =
        if is_hcons then (==)
        else fun t t' -> failwith "No function equal when patricia trees are not HConsed"

      let hash =
        if is_hcons then id
        else fun t -> failwith "No function hash when patricia trees are not HConsed"
                               
      (* Now we start the standard functions on maps/sets *)

      let rec mem k t = match reveal t with
        | Empty               -> false
        | Leaf (j,_)          -> K.compare k j = 0
        | Branch (_, m, l, r) -> mem k (if check (tag k) m then l else r)

      let rec find k t = match reveal t with
        | Empty               -> raise Not_found
        | Leaf (j,x)          -> if K.compare k j = 0 then x else raise Not_found
        | Branch (_, m, l, r) -> find k (if check (tag k) m then l else r)


      (* Smart constructors, using both the HConsing techniques and
       assuring some invariant of Patricia trees *)

      let empty      = build Empty
      let leaf (k,x) = build (Leaf(k,x))
      let singleton k x = leaf(k,x)
      let branch = function
        | (_,_,e,t) when is_empty e -> t
        | (_,_,t,e) when is_empty e -> t
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
	  | Leaf (j,x) -> if  K.compare k j = 0 then f k x else failwith "Remove: Was not there -leaf"
	  | Branch (p,m,t0,t1) ->
	     if match_prefix (tag k) p m then
	       if check (tag k) m then branch (p, m, rmv t0, t1)
               else branch (p, m, t0, rmv t1)
	     else failwith "Remove: Was not there -branch"
        in rmv t

      (* remove function: argument f of remove_aux says "delete the key altogether" *)

      let remove = remove_aux (fun _ _ -> empty)

    (* Now we have finished the material that is common to Maps AND Sets,
     closing module BackOffice *)
    end

    include BackOffice

    (* Now starting functions specific to Maps, not Sets *)

    (* Printing as a tree
       argument common says how to print a common,
       argument branching says how to print a branching,
       argument keyvalue says how to print a pair (key,value) *)

    let print_tree_in_fmt
          ?(common   =print_nothing)
          ?(branching=print_nothing)
          keyvalue
          fmt t =
      let rec aux indent fmt t = match reveal t with
	| Empty            -> fprintf fmt "{}"
	| Leaf(j,x)        -> fprintf fmt "%t%s%a" indent "   "
                                (match keyvalue with
                                 | Some f -> f
                                 | None -> print_dot)
                                (j,x)
	| Branch(p,m,t0,t1)->
           let auxd s = aux (fun fmt -> fprintf fmt "%t%a%s%a" indent common p s branching m) in
           fprintf fmt "%a\n%a" (auxd "+") t0 (auxd "-") t1
      in fprintf fmt "\n%a" (aux (fun fmt -> ())) t

    (* argument f says how to print a pair (key,value) *)

    let print_in_fmt ?tree keyvalue fmt t =
      match tree with
      | Some(common,branching) ->
         print_tree_in_fmt ~common ~branching (Some keyvalue) fmt t
      | None -> 
         let rec aux indent fmt t = match reveal t with
	   | Empty            -> fprintf fmt "{}"
	   | Leaf(j,x)        -> fprintf fmt "%a" keyvalue (j,x) 
	   | Branch(p,m,t0,t1)->
              let auxd = aux indent in
              fprintf fmt "%a, %a" auxd t0 auxd t1
         in fprintf fmt "%a" (aux (fun fmt -> ())) t

  (* argument f says what to do in case a binding is already found *)

    let add k f t =
      let rec ins t = match reveal t with
        | Empty      -> leaf(k,f None)
        | Leaf (j,y) ->
	  if  K.compare k j =0 then leaf (k,f (Some y))
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

    let rec fold_monad ~return ~bind f t seed =
      let rec aux t sofar = 
        match reveal t with
        | Empty              -> return sofar
        | Leaf (k,x)         -> f k x sofar
        | Branch (_,_,t0,t1) -> sofar |> aux t0 |> bind aux t1 
      in
      aux t seed

    let fold f = fold_monad ~return:(fun x->x) ~bind:(fun x -> x) f
          
    let iter f t =
      let aux k x () = f k x in
      fold aux t ()
             
    let prefix t = match reveal t with
      | Empty -> failwith "Patricia.prefix: empty"
      | Leaf(k,_) -> tag k
      | Branch(p,_,_,_) -> p

    let join u1 u2 =
      if is_empty u1 then u2
      else if is_empty u2 then u1
      else join (prefix u1, u1, prefix u2, u2)


    (* Following type used in merge, union, inter, subset, diff, etc.
       each field says what to do in case 
       - same key is found on both sides
       - first arg is empty
       - second arg is empty
       - we separated into 2 tasks, computed 2 results, what to do with them *)

    module Merge = struct
      type ('v1,'i1,'v2,'i2,'a) t = {
          sameleaf  : keys -> 'v1 -> 'v2 -> 'a;
          emptyfull : ('v2,'i2) param -> 'a;
          fullempty : ('v1,'i1) param -> 'a;
          combine   : 'a -> 'a -> 'a
        }
    end

    module Fold2 = struct
      type ('v1,'i1,'v2,'i2,'a,'b) t = {
          sameleaf  : keys -> 'v1 -> 'v2 -> 'a -> 'b;
          emptyfull : ('v2,'i2) param -> 'a -> 'b;
          fullempty : ('v1,'i1) param -> 'a -> 'b;
          combine   : (('v1,'i1) param -> ('v2,'i2) param -> 'a -> 'b)
                      -> (('v1,'i1) param -> ('v2,'i2) param -> 'b -> 'b)
                         * (('v2,'i2) param -> 'b -> 'b)
                         * (('v1,'i1) param -> 'b -> 'b)
        }

      let make_combine empty1 empty2 combine reccall =
        combine reccall,
        (fun t2 b -> combine reccall empty1 t2 b),
        (fun t1 b -> combine reccall t1 empty2 b)
                                         
      let merge2fold2 m = {
          sameleaf  = Merge.(fun k v1 v2 () -> m.sameleaf k v1 v2);
          emptyfull = Merge.(fun a () -> m.emptyfull a);
          fullempty = Merge.(fun a () -> m.fullempty a);
          combine   = Merge.(fun reccall ->
            (fun a1 a2 b -> m.combine b (reccall a1 a2())),
            (fun a b -> m.combine b (m.emptyfull a)),
            (fun a b -> m.combine b (m.fullempty a)))
        }
    end

    let fold2_trans reccall action s1 s2 seed =
      let open Fold2 in
      let combine,combineEF,combineFE = action.combine reccall in
      let disjoint s1 s2 seed = seed |> action.fullempty s1 |> combineEF s2 in
        
      match reveal s1, reveal s2 with

      | Empty, _ -> seed |> action.emptyfull s2

      | _, Empty -> seed |> action.fullempty s1

      | Leaf(k1,x1), Leaf(k2,x2) ->
	 if  K.compare k1 k2 =0
         then action.sameleaf k1 x1 x2 seed
	 else seed |> disjoint s1 s2

      | Leaf(k,x), Branch(p,m,t1,t2) ->
         let tagk = tag k in
	 if match_prefix tagk p m then
	   if check tagk m then
	     seed |> reccall s1 t1 |> combineEF t2
	   else
             seed |> action.emptyfull t1 |> combine s1 t2
         else
           seed |> disjoint s1 s2
                
      | Branch(p,m,t1,t2), Leaf(k,x) ->
         let tagk = tag k in
	 if match_prefix tagk p m then
	   if check tagk m then 
	     seed |> reccall t1 s2 |> combineFE t2
	   else
	     seed |> action.fullempty t1 |> combine t2 s2
	 else
           seed |> disjoint s1 s2

      | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	 if (bcompare m1 m2=0) && match_prefix p1 p2 m1 then 
	   seed |> reccall l1 l2 |> combine r1 r2
	 else if bcompare m1 m2<0 && match_prefix p2 p1 m1 then
	   if check p2 m1 then
             seed |> reccall l1 s2 |> combineFE r1
	   else
             seed |> action.fullempty l1 |> combine r1 s2
	 else if bcompare m2 m1<0 && match_prefix p1 p2 m2 then
	   if check p1 m2 then
             seed |> reccall s1 l2 |> combineEF r2
	   else
             seed |> action.emptyfull l2 |> combine s1 r2
	 else
           seed |> disjoint s1 s2

    let fold2_poly action =
      let rec aux s1 s2 = fold2_trans aux action s1 s2
      in aux
           
    let fold2 =
      if is_hcons
      then
        (fun ?equal action ->
          match equal with
          | Some f ->
             let rec aux s1 s2 =
               if s1==s2 then f s1 else fold2_trans aux action s1 s2
             in aux
          | None -> fold2_poly action )
      else
        (fun ?equal action ->
          match equal with
          | Some f -> failwith "Patricia tries not hconsed"
          | None -> fold2_poly action )

    let merge_poly action t1 t2 =
      fold2_poly (Fold2.merge2fold2 action) t1 t2 ()

    let equal2merge = function
      | Some f -> Some(fun a () -> f a)
      | None -> None
      
    let merge ?equal action t1 t2 =
      fold2 ?equal:(equal2merge equal) (Fold2.merge2fold2 action) t1 t2 ()
          
          
    let union_poly_action f emptyfull fullempty = Merge.{
        sameleaf = (fun k v1 v2 -> singleton k (f k v1 v2));
        emptyfull = emptyfull;
        fullempty = fullempty;
        combine   = join
      }
                           
    let union_poly f fullempty emptyfull = merge_poly (union_poly_action f emptyfull fullempty)

    let union_action f = Merge.{
        sameleaf = (fun k v1 v2 -> singleton k (f v1 v2));
        emptyfull = (fun a -> a);
        fullempty = (fun a -> a);
        combine   = join
      }

    let union =
      if is_hcons
      then (fun f -> merge ~equal:(fun a->a) (union_action f))
      else (fun f -> merge_poly (union_action f))


    let inter_action f = Merge.{
        sameleaf = (fun k v1 v2 -> singleton k (f k v1 v2));
        emptyfull = (fun _ -> empty);
        fullempty = (fun _ -> empty);
        combine   = join
      }
                           
    let inter_poly f = merge_poly (inter_action f)

    let inter =
      if is_hcons
      then (fun f -> merge ~equal:(fun a->a) (inter_action f))
      else inter_poly

    let diff_action f = Merge.{
        sameleaf  = f;
        emptyfull = (fun _ -> empty);
        fullempty = (fun a -> a);
        combine   = join
      }
                           
    let diff_poly f = merge_poly (diff_action f)

    let diff =
      if is_hcons
      then (fun f -> merge ~equal:(fun a->empty) (diff_action f))
      else diff_poly

    let subset_action f = Fold2.{
        sameleaf  = (fun _ x y () -> f x y);
        emptyfull = (fun _ () -> true);
        fullempty = (fun _ () -> false);
        combine   = fun reccall ->
                    (fun a1 a2 l -> l && reccall a1 a2 ()),
                    (fun a2 l    -> l),
                    (fun a1 l    -> false)
      }

    let subset_poly f =
      let action = subset_action f in
      let rec aux s1 s2 () = fold2_trans aux action s1 s2 ()
      in fun s1 s2 -> aux s1 s2 ()

    let subset =
      if is_hcons
      then 
        (fun f ->
          let action = subset_action f in
          let rec aux s1 s2 () =
            if s1==s2 then true else fold2_trans aux action s1 s2 ()
          in fun s1 s2 -> aux s1 s2 ())
      else
        subset_poly 

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
        let m1,m2 = min s1,min s2 in
        if ocompare (m1,m2) =0 then match reveal s1,reveal s2 with
        | Empty,Empty -> (None,true)
        | Leaf(k,x), _  when mem k s2 -> (let y = find k s2 in match f k x y with
          | (None,_) -> (min(remove k s2),false)
          | a        -> a)
        | _,Leaf _     -> let (b,c) = aux s2 s1 in (b,not c)
        | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
          let (rec1,rec2,i)=
            if (bcompare m1 m2=0) &&  match_prefix p1 p2 m1 then
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
      | Empty     -> raise Not_found
      | Leaf(k,x) -> (k,x)
      | Branch (_, _,t0,_) -> choose t0   (* we know that [t0] is non-empty *)
	
    let make f l = List.fold (fun (k,x)->add k (f x)) l empty

    let elements s =
      let rec elements_aux acc t = match reveal t with
        | Empty      -> acc
        | Leaf(k,x) -> (k,x) :: acc
        | Branch (_,_,l,r) -> elements_aux (elements_aux acc l) r
      in
      elements_aux [] s

  end



  (* Construction of a Patricia tree structure for sets, given the above.
     Most of it is imported from PATMap *)

  module SetMake(E:SetArg with type t=I.keys) = struct

  (* A Set is just a Map with codomain unit.  Constructing this Map
     structure *)

    module MapDest = struct
      type t       = E.t [@@deriving ord]
      type values  = unit
      type infos   = E.infos
      let info_build = E.info_build
      let treeHCons  = match E.treeHCons with
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
    let union      = PM.union (fun _ () -> ())
    let inter      = PM.inter (fun _ () () -> ())
    let inter_poly a b = PM.inter_poly (fun _ _ _ -> ()) a b
    let subset a b = PM.subset(fun () () -> true) a b
    let subset_poly a b = PM.subset_poly (fun () _ -> true) a b
    let diff       = PM.diff  (fun _ _ _ -> empty)
    let diff_poly a b = PM.diff_poly (fun _ _ _ -> empty) a b
    let first_diff = PM.first_diff (fun _ ()()-> None,true) E.compare
    let iter f a   = PM.iter (fun k () -> f k) a
    let fold_monad ~return ~bind f = PM.fold_monad ~return ~bind (fun k _ -> f k)
    let fold f     = PM.fold (fun k x -> f k)
    let choose t   = let k,_ = PM.choose t in k
    let elements s = List.map (function (k,x)->k) (PM.elements s)

    (* Now starting functions specific to Sets, without equivalent
     ones for Maps *)

    let print_tree_in_fmt ?common ?branching f =
      PM.print_tree_in_fmt ?common ?branching
        (match f with Some f -> Some(fun fmt (x,())->f fmt x) | None -> None)

    let print_in_fmt ?tree f fmt a =
      PM.print_in_fmt ?tree (fun fmt (x,())->f fmt x) fmt a

    let make l     = List.fold add l empty

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


module PatSet = struct

  module type S = PatSet

  module Make(E:SetArg)(I:Intern with type keys=E.t) = struct
    module Basis = Poly(I)
    include Basis.SetMake(E)
  end

end

module PatMap = struct

  module type S = PatMap

  module Make(K:MapArg)(I:Intern with type keys=K.t) = struct
    module Basis = Poly(I)
    include Basis.MapMake(K)
  end

end
