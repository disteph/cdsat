(* Generic implementation of sets,
   particular implementation of sets of atoms,
   using patricia trees *)

open Lib
open Kernel

open Formulae
open Collection
open Sums
open Patricia
open SetConstructions

module MyPat(UT:sig
	       include Intern
	       val compare : keys->keys->int
	       val toString: keys->string
	       val tString: ((common -> string)*(branching->string)) option
	     end) = struct

  module D = struct
    type keys        = UT.keys
    let kcompare     = UT.compare
    type values      = unit
    let vcompare _ _ = 0
    type infos       = keys m_infos
    let info_build   = (None,(fun x _ -> Some x),splmin kcompare)
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
    let hash     = SS.hash
    let equal    = SS.equal
    let toString = SS.toString UT.tString UT.toString
    let next  t1 = let e1 = SS.choose t1 in (e1, SS.remove e1 t1) 
  end

  module Ext = struct
    include CI
    let compare    = SS.compare
    let compareE   = UT.compare
    let first_diff = SS.first_diff SS.info
    let sub alm s1 s2 limit =
      let locprune t = match limit,SS.info t with
	| Some b,Some x when not (UT.compare x b<0) -> SS.Empty
	| _                                         -> SS.reveal t
      in SS.sub locprune alm s1 s2
  end

  include Ext

  let choose     = SS.choose
  type common    = UT.common
  type branching = UT.branching
  let find_su    = SS.find_su
  let clear      = SS.clear
  let cardinal   = SS.cardinal
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
	       let tString  = None
	end)


(* Particular implementation of sets of atoms *)

module MyPatA = struct

  module AtSet = MyPatriciaCollectImplem(Atom)
  module M = struct
    type t       = bool*Atom.Predicates.t
    let id (b,f) = 2*(Atom.Predicates.id f)+(if b then 1 else 0)
  end

  let pcompare p1 p2 = Pervasives.compare (M.id p1) (M.id p2)
    
  let atcompare at1 at2=
    let (b1,p1,tl1)=Atom.reveal at1 in
    let (b2,p2,tl2)=Atom.reveal at2 in
    let c= pcompare(b1,p1)(b2,p2) in
      if c==0 then Atom.compare at1 at2 else c

  module Destination = struct
    type keys      = M.t
    let kcompare   = pcompare  
    type values    = AtSet.t
    let vcompare   = AtSet.compare
    type infos     = Atom.t m_infos
    let info_build = (None,(fun _ v -> AtSet.SS.info v),splmin atcompare)
    let treeHCons  = !Flags.memo
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
	(SS.mem (b,p) t)&&(AtSet.is_in l (SS.find (b,p) t))
    let add l     =
      let (b,p,tl) = Atom.reveal l in
      let f c = function
	| None   -> AtSet.SS.singleton c
	| Some d -> AtSet.add c d
      in SS.add f (b,p) l
    let remove l  =
      let (b,p,tl) = Atom.reveal l in
	SS.remove_aux (fun k x -> lleaf(k,AtSet.remove l x)) (b,p)
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
    let compare    = SS.compare
    let compareE   = atcompare
    let first_diff = SS.first_diff (fun _->AtSet.first_diff) compareE SS.info
    let sub alm s1 s2 limit =
      (*      let singl e = let (b,p,_)= Atom.reveal e in SS.Leaf((b,p),AtSet.SS.singleton e) in	  *)
      let f alm k x = function
	| Some y -> AtSet.sub alm x y limit
	| None   -> AtSet.sub alm x AtSet.empty limit
      in 
      let locprune t = match limit,SS.info t with
	| Some b,Some x when not (compareE x b<0) -> SS.Empty
	    (*	| Some b,Some(x,Some y) when not (compareE y b<0) -> singl x*)
	| _                                       -> SS.reveal t
      in SS.sub f locprune alm s1 s2

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
  type common
  type branching

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
  val sub  : bool->t->t->e option->(unit,e) almost
  val first_diff : t -> t -> e option * bool
  val choose : t -> e
  val clear: unit->unit
  val cardinal: t->int
  val find_su :
    (e->'a) ->
    (e->branching->'b) ->
    'b ->
    ('b -> 'b -> 'b) ->
    (common -> common -> branching option -> ('c, branching) almost) ->
    bool ->
    (branching -> bool) ->
    ('b -> bool) ->
    common ->
    t -> 
    ('a, 'b) sum

end
