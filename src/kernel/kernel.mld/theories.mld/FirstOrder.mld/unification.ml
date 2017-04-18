(********************************************)
(* This is where unification is implemented *)
(********************************************)

open Format

open Top
open Basic
open Variables

module IU = Unifier

(* This module is the datastructure establishing the correspondence
   between meta-variables and their keys.
   Some keys do not correspond to metas (as indicated by function is_meta.
   We call them "wild keys".
*)

module MKcorr = (struct

  type t    = {get_key: IU.MMap.t; get_meta: IU.KMap.t}
  let empty = {get_key = IU.MMap.empty; get_meta = IU.KMap.empty}

  let get_key  f mv  = IU.MMap.find mv f.get_key
  let get_meta f key = IU.KMap.find key f.get_meta
  let is_meta  f key = IU.KMap.mem key f.get_meta

  let add f mv key = {get_key  = IU.MMap.add mv (function None -> key | _ -> failwith "meta already mapped") f.get_key;
                      get_meta = IU.KMap.add key (function None -> mv | _ -> failwith "key already mapped") f.get_meta}

  let remove f mv = let key = get_key f mv in
                    {get_key  = IU.MMap.remove mv  f.get_key;
                     get_meta = IU.KMap.remove key f.get_meta}

  let fold f mk = IU.MMap.fold f mk.get_key
  let pp fmt mk = 
    IU.MMap.print_in_fmt
      (fun fmt (mv,key) -> Format.fprintf fmt "(?%a -> k%a)" Meta.pp mv IU.ppK key)
      fmt
      mk.get_key
  let show = Dump.stringOf pp
end: sig
  type t [@@deriving show]
  val empty: t
  val get_key : t -> Meta.t -> IU.keys
  val get_meta: t -> IU.keys -> Meta.t
  val is_meta : t -> IU.keys -> bool
  val add   : t -> Meta.t -> IU.keys -> t
  val remove: t -> Meta.t -> t
  val fold: (Meta.t -> IU.keys -> 'b -> 'b) -> t -> 'b -> 'b
end)


(* This module is the datastructure establishing the correspondence
   between the two sides of unification problems
   mk1: correspondence between meta and keys for side 1
   mk2: reverse map
   loc12: map from side1 wild keys to side2 wild keys
   loc21: reverse map
*)

module KKcorr = (struct

  type t = {mk1: MKcorr.t;
            mk2: MKcorr.t;
            loc12: IU.KKMap.t;
            loc21: IU.KKMap.t}

  let init f1 f2 = {mk1 = f1;
                    mk2 = f2;
                    loc12 = IU.KKMap.empty; 
                    loc21 = IU.KKMap.empty}

  let swap f = {mk1 = f.mk2;
                mk2 = f.mk1;
                loc12 = f.loc21; 
                loc21 = f.loc12}

  let get12 f key1 = 
    if MKcorr.is_meta f.mk1 key1
    then Some(MKcorr.get_key f.mk2 (MKcorr.get_meta f.mk1 key1))
    else if IU.KKMap.mem key1 f.loc12
    then Some(IU.KKMap.find key1 f.loc12)
    else None

  let get21 f key2 = 
    let f' = swap f in
    get12 f' key2

  let add f key1 key2 =
    {mk1 = f.mk1;
     mk2 = f.mk2;
     loc12 = IU.KKMap.add key1 (function None -> key2 | _ -> failwith "key already mapped in loc12") f.loc12; 
     loc21 = IU.KKMap.add key2 (function None -> key1 | _ -> failwith "key already mapped in loc21") f.loc12}
      
end: sig
  type t
  val init: MKcorr.t -> MKcorr.t -> t
  val swap: t -> t
  val get12: t -> IU.keys -> IU.keys option
  val get21: t -> IU.keys -> IU.keys option
  val add: t -> IU.keys -> IU.keys -> t
end)


module Unification = (struct
    
  exception WrongArgumentNumber
  exception NonUnifiable

  (* Implementation of occurs_check

     We are about to assign a value to key0. We want to check that
     the value does not depend on key0. We recursively go down into
     the value (and then, recursively, into a key, an eigen) in
     order to check this. By calling these functions with lax =
     true, we accept the case where the value is key0 itself.
     
     fold is into a function which, given an eigen, provides an
     iterator over all the keys upon which this eigen depends.  *)

  let rec occurs_check_v lax fold key0 c =
    let (t,u) = IU.expose c in
    match t with
    | IU.Eigen ei    -> occurs_check_ei fold key0 u ei
    | IU.Key  key    -> occurs_check_k  lax fold key0 u key
    | IU.C(_,l)      -> 
      List.fold 
        (fun t u' -> occurs_check_v false fold key0 (t,u'))
        l u

  and occurs_check_k lax fold key0 u key =
    if (not lax)&&(IU.kcompare key key0 =0) then raise NonUnifiable
    else
      match IU.get key u with
      | None   -> u
      | Some c -> occurs_check_v lax fold key0 c

  and occurs_check_ei fold key0 u ei =
    fold ei (fun key u'' -> occurs_check_k false fold key0 u'' key) u

  (* In function translate, we are trying to assign, to the side1
     key key0, a side1 value, which reflects the side2 value t2 in
     its enviroment u2.

     fold is a function which, given an eigen, provides an iterator
     over all the side1 keys representing those metas upon which
     this eigen depends.

     b is a boolean saying...

     We work in CPS, so cont is a continuation of type
  *)

  let translate b fold key0 u2 t2 cont =
    Dump.print ["unification",1] (fun p->p "translate k%a -> %a in %a" IU.ppK key0 IU.ppV t2 IU.pp u2);

    (* In the recursive auxiliary function aux,
       - lax says whether we accept key0 to be mapped to itself.
       - cont is the continuation that takes the side1 value to be
       assigned to key0, the current side1 environment, the
       current side2 environment, and outputs in the return type
       'a
       - t2, its environment u2, is the side2 value to be mimicked
       in side1
       - the side1 environment u1, in which we want to construct the
       side1 value for key0, is taken as the next argument, in
       each branch of the pattern-matching
    *)

    let rec aux lax cont t2 u2 =
      let (t2',u2') = IU.expose(t2,u2) in
      match t2' with

      | IU.Eigen ei when b -> 
        fun u1 ->
          (* Here, we want to map key0 to ei, we just run occurs
             check to verify that this assignment respects
             dependencie *)
          let u1' = 
            Dump.print ["unification",1] (fun p->p "occurs_check_ei on k%a -> %a" IU.ppK key0 Eigen.pp ei);
            occurs_check_ei fold key0 u1 ei
          in
          cont (IU.eigen2val ei) u1' u2'

      | IU.C(a,l) when b -> 
        fun u1 ->
          (* Here, we want to map key0 to C(a,l'), where l' is the
             translation in side1 of l. We do this in CPS with
             auxiliary function aux_tl. Function aux_tl expects a
             continuation working just like those continuations
             accepted by aux, except the first argument is a list of
             side1 values instead of a side1 value. *)
          let rec aux_tl cont = function
            | []   -> cont []
            | h::l -> let newcont1 h' =
                        let newcont2 l' =
                          cont (h'::l')
                        in aux_tl newcont2 l
                      in aux false newcont1 h
          in aux_tl (fun l' -> cont(IU.bC a l')) l u1 u2'

      | IU.Key i2 -> 
        fun u1 kk_corr ->
          begin
            match KKcorr.get21 kk_corr i2 with
            | None ->  
              let newkey,u1' = IU.new_key (IU.get_sort i2) u1 in
              let kk_corr' = KKcorr.add kk_corr newkey i2 in
              cont (IU.key2val newkey) u1' u2' kk_corr'
            | Some key1 when b && (lax||(key1!=key0)) -> 
              let (t0,u1') = 
                match IU.get key1 u1 with
                | None             -> (IU.key2val key1,u1)
                | Some((t,_) as c) -> (t,occurs_check_v lax fold key0 c)
              in cont t0 u1' u2' kk_corr
            | Some key1 -> 
              Dump.print ["unification",1] (fun p->p "occurs_check1 with b=%b key0=%a i2=%a key1=%a" b IU.ppK key0 IU.ppK i2 IU.ppK key1);
              raise NonUnifiable
          end

      | _ -> raise NonUnifiable

    in
    let newcont t1 u1 = 
      let u1' = IU.add key0 t1 u1 in
      cont u1'
    in
    aux true newcont t2 u2

  (* combine takes a list l of unification constraints, and a pair
     of lists of terms of the same length. The function generates new
     unification constraints by the pairwise analysis of the two term
     lists *)

  let rec combine l = function
    | [],[]             -> l
    | (a1::l1),(a2::l2) -> combine ((a1,a2)::l) (l1,l2)
    | _,_               -> raise WrongArgumentNumber


  (* Main unification function!!!
     - l is the list of unification constraints to solve
     - u1 is the current substitution for side 1 (type IU.t)
     - u2 is the current substitution for side 2 (type IU.t)
     - mk1 is the input meta-key correspondence for side 1 (type MKcorr.t)
     - mk2 is the input meta-key correspondence for side 2 (type MKcorr.t)
     - fold contains the dependency constraints between eigen and meta
     (/ keys), and is used for occurs_check 
     - b is a boolean that is passed to the sub-routines
  *)

  let unif b w mk1 mk2 u1 u2 l =

    (* We start by creating 2 functions to check the dependencies
       for occurs_check, for side 1 and side 2 *)

    let fold1 ei f = World.fold w ei (fun k -> f (MKcorr.get_key mk1 k)) in
    let fold2 ei f = World.fold w ei (fun k -> f (MKcorr.get_key mk2 k)) in

    (* The unification function is recursive, with the arguments u1,
       u2, and l that may change along the recursive calls.
       A new argument, kkcorr, will also change along the recursive
       calls: a correspondence between the keys of side 1 and the
       keys of side 2.
       At the beginning, this correspondence is limited to "being
       the key for the same meta-variable", which we compute as
       follows: *)

    let kkcorr_init = KKcorr.init mk1 mk2 in

    (* Now we can forget about fold, mk1 and mk2. We come to the
       recursive unification function: *)

    let rec aux l u1 u2 kkcorr = match l with

      (* We have solved all unification constraints, we output the 2
         unifiers constructed so far (one for side 1, one for side 2 *)

      | []         -> Some(u1,u2)

      (* We find a unification constraint between 2 terms *)

      | (t1,t2)::l -> 
        Dump.print ["unification",1] (fun p->p "unifying %a in %a to %a in %a" IU.ppV t1 IU.pp u1 IU.ppV t2 IU.pp u2);

        (* We expose the head shape of the 2 terms, and match them *)

        let (t1',u1') = IU.expose (t1, u1) in
        let (t2',u2') = IU.expose (t2, u2) in
        try match t1', t2' with

        (* Same eigenvariable on both side -> we dismiss the
           constraint and continue *)

        | IU.Eigen i1, IU.Eigen i2 when Eigen.equal i1 i2  -> aux l u1' u2' kkcorr

        (* Same function symbol on both side -> we add the
           unification constraints on the arguments (pairwise) and
           continue *)

        | IU.C(a1,l1), IU.C(a2,l2) when Symbols.equal a1 a2 -> aux (combine l (l1,l2)) u1' u2' kkcorr

        (* Two keys: we look at whether they correspond to each
           other according to kkcorr. If they do, we dismiss the
           constraint and continue, otherwise see the next
           pattern-matching cases *)

        | IU.Key key1, IU.Key key2 when 
            (match KKcorr.get21 kkcorr key2 with
            | Some i -> IU.kcompare i key1 =0
            | None   -> false)
            -> aux l u1' u2' kkcorr

        (* Key on the left (key1): we try to see if we can map it to
           the term on the right (t2). For this we need to
           *translate* the term t2 into the left-hand
           world. Occurs_check will be done during that translation.

           Also, if the term on the right t2 is itself a key key2,
           we also need to edit the right-hand side to map that key
           to (the translation in the right-hand world of) the key
           on the left.
           
           The combination of both editions is done in
           continuation-passing style *)
          
        | IU.Key key1, _  ->
          let cont u1' u2' kkcorr = match t2' with
            | IU.Key key2 ->
              translate b fold2 key2 u1' t1 (fun u2'' u1'' kkcorr'' -> aux l u1'' u2'' (KKcorr.swap kkcorr'')) u2' (KKcorr.swap kkcorr)
            | _ -> aux l u1' u2' kkcorr
          in
          translate b fold1 key1 u2' t2 cont u1' kkcorr

        (* Key on the right (key2): as above, swapping left and right.

           Except that now we know that the left-hand term is not a
           key, so there is no need to edit the left-hand side *)

        | _, IU.Key key2  ->
          translate b fold2 key2 u1' t1 (fun u2'' u1'' kkcorr'' -> aux l u1'' u2'' (KKcorr.swap kkcorr'')) u2' (KKcorr.swap kkcorr)

        (* All other cases are non-unifiable problems *)

        | _, _            -> raise NonUnifiable

        with NonUnifiable -> None
    in

    (* Finally, we initialise a call to the recursive function *)

    aux l u1 u2 kkcorr_init

end: sig
  val combine: ('l * 'm) list -> 'l list * 'm list -> ('l * 'm) list
  val unif: bool -> World.t -> MKcorr.t -> MKcorr.t -> IU.t -> IU.t -> (IU.values * IU.values) list -> (IU.t * IU.t) option
end)
