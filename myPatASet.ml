open Formulae
open Collection
open Patricia
open SetConstructions

(* Generic implementation of sets,
   particular implementation of sets of atoms,
   using patricia trees *)

module MyPat(UT:sig
	       include Intern
	       val compare : keys->keys->int
	       val toString: keys->string
	       val tString: ((common -> string)*(branching->string)) option
	     end) = struct
  module D = struct
    type keys        = UT.keys
    type values      = unit
    let vcompare _ _ = 0
      (* type infos = unit *)
      (* let info_build = empty_info_build *)
      (* Alternative, recording min *)
    type infos       = keys m_infos
    let info_build   = m_info_build UT.tag UT.ccompare
    let treeHCons    = !Flags.memo
  end

  module SS = PATSet(D)(UT)

  module CI = struct
    type e       = UT.keys
    type t       = SS.t
    let is_empty = SS.is_empty
    let is_in    = SS.mem
    let empty    = SS.empty
    let add      = SS.add
    let union    = SS.union
    let inter    = SS.inter
    let remove   = SS.remove
      (* Alternative, if min or max is recorded:

	 let choose t = match SS.info t with 
	 | _,Some y,_ -> y 
	 | _-> failwith("No Element!")
	 end	 
      *)
    let hash  = SS.hash
    let equal = SS.equal

    let toString = SS.toString UT.tString UT.toString
    let next  t1 = let e1 = SS.choose t1 in (e1, SS.remove e1 t1) 

  end

  module Ext = struct
    include CI
    let compare    = SS.compare
    let compareE   = UT.compare
    let min        = SS.info
    let diff       = SS.diff
    let first_diff = SS.first_diff min
  end

  include Ext

  let choose     = SS.choose
  type common    = UT.common
  type branching = UT.branching
  let find_su    = SS.find_su
  let clear ()   = SS.clear ()

end


(* Generic implementation of sets *)

module MyPatriciaCollectImplem(M:sig
				 type t
				 val id: t->int
				 val compare : t->t->int
				 val toString: t->string
			       end) =
  MyPat(struct include TypesFromHConsed(M)
	       let compare  = M.compare
	       let toString = M.toString
	       let tString = None
	end)


(* Particular implementation of sets of atoms *)

module MyPatA = struct

  module AtSet = MyPatriciaCollectImplem(Atom)
  module M = struct
    type t       = bool*Atom.Predicates.t
    let id (b,f) = 2*(Atom.Predicates.id f)+(if b then 1 else 0)
  end
  module Destination = struct
    type keys          = M.t
    type values        = AtSet.t
    let vcompare s1 s2 = match AtSet.Ext.first_diff s1 s2 with
      | (None,_) -> 0
      | (_,true) -> -1
      | (_,false)-> 1
    type infos     = (keys*values) option
    let info_build = (
      None,
      (fun x y -> Some(x,y)),
      (fun x1 x2
	 -> match x1,x2 with
	   | None,_ -> x2
	   | _,None -> x1
	   | Some(y1,v1),Some(y2,v2)->
	       if Pervasives.compare (M.id y1) (M.id y2)<0 || ((M.id y1)==(M.id y2)&&vcompare v1 v2<0)
	       then x1 else x2)
    )
    let treeHCons = !Flags.memo
  end

  module SS = PATMap(Destination)(TypesFromHConsed(M))

  let lleaf(k,x) = if AtSet.is_empty x then SS.empty else SS.leaf(k,x)

  module CI = struct
    type e        = Atom.t
    type t        = SS.t
    let hash      = SS.hash
    let equal     = SS.equal
    let empty     = SS.empty
    let is_empty  = SS.is_empty
    let union     = SS.union AtSet.union
    let inter     = SS.inter AtSet.inter
    let is_in l t =
      let (b,p,tl) = Atom.reveal l in
	(SS.mem (b,p) t)
	&&(AtSet.is_in l (SS.find (b,p) t))
    let add l     =
      let (b,p,tl) = Atom.reveal l in
      let f c = function
	| None   -> AtSet.SS.singleton c
	| Some(d)-> AtSet.add c d
      in SS.add f (b,p) l
    let remove l t =
      let (b,p,tl) = Atom.reveal l in
	SS.remove_aux (fun k x -> lleaf(k,AtSet.remove l x)) (b,p) t
    let next t1 =
      let (_,y) = SS.choose t1 in
      let l = AtSet.SS.choose y in
      (l, remove l t1)
    let toString = SS.toString None (fun (k,l)->AtSet.toString l)
    let filter b pred t=
      if SS.mem (b,pred) t
      then SS.leaf((b,pred),SS.find (b,pred) t)
      else empty
  end

  module Ext =struct
    include CI
    let compare  = SS.compare
    let compareE t1 t2  =
      let (b1,pred1,tl1) = Atom.reveal t1 in 
      let (b2,pred2,tl2) = Atom.reveal t2 in 
      let c = Pervasives.compare (M.id (b1,pred1))(M.id (b2,pred2)) in
	if c==0 then Pervasives.compare (Atom.id t1) (Atom.id t2) else c
    let min s      = match SS.info s with
      | None       -> None
      | Some(k,v)  -> AtSet.SS.info v 
    let diff       = SS.diff (fun k x y -> lleaf(k,AtSet.SS.diff x y))
    let first_diff s1 s2 = match SS.first_diff SS.info s1 s2 with
      | (None,b)      -> (None,b)
      | (Some(k,x),b) -> let other = if b then s2 else s1 in
	  if SS.mem k other
	  then  AtSet.SS.first_diff (AtSet.SS.info) x (SS.find k other)
	  else (AtSet.SS.info x,b)
  end

  include Ext

  let clear () = SS.clear();AtSet.clear()
  let id=SS.id

end

(* Interface, see .mli *)

module type MyPatCollect =
sig
  module CI  : CollectImplem
  module Ext : Memoisation.CollectImplemExt with type e = CI.e and type t = CI.t

  type e = CI.e
  type t = CI.t

  val is_empty : t -> bool
  val is_in : e -> t -> bool
  val empty : t
  val add   : e -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val remove : e -> t -> t
  val hash : t -> int
  val equal : t -> t -> bool
  val next : t -> e * t
  val toString : t -> string
  val compare : t -> t -> int
  val compareE : e -> e -> int
  val min : t -> e option
  val diff : t -> t -> t
  val first_diff : t -> t -> e option * bool
  val choose : t -> e
  val clear: unit->unit
  type common
  type branching
  val find_su :
    (common -> common -> branching option -> ('a, branching) almost) ->
    bool ->
    (branching -> bool) ->
    ('b -> bool) ->
    (e -> 'c) ->
    'b ->
    (e -> branching -> 'b) ->
    ('b -> 'b -> 'b) ->
    common ->
    t -> 
    ('c, 'b) sum

end
