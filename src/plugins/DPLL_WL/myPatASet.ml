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
    let fold     = SS.fold
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

  let iter       = SS.iter
  let diff       = SS.diff
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

module MyPatA(Atom:AtomType) = struct

  module AtSet = MyPatriciaCollectImplem(Atom)

(*  module M = struct
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
*)

  module CI = struct
    type e              = Atom.t
    type t              = AtSet.t*(e option)
    let hash(a,_)       = AtSet.hash a
    let equal(a,_)(a',_)= AtSet.equal a a'
    let empty           = (AtSet.empty, None)
    let is_empty(a,_)   = AtSet.is_empty a
    let union(a,_)(a',_)= (AtSet.union a a',None)
    let inter(a,_)(a',_)= (AtSet.inter a a',None)
    let is_in l (t,_)   = AtSet.is_in l t
    let add l (h,_)     = (AtSet.add l h,Some l)
    let remove l (h,_)  = (AtSet.remove l h, None)
    let next (t1,a)     = 
      let (l,t2) = AtSet.next t1 in
	(l, (t2,None))
    let fold f (a,_)   = AtSet.fold f a
    let toString (h,_)= AtSet.toString h 
      (*    let toString (h,_)= SS.toString (Some((fun i->string_of_int i^"|"),(fun i->string_of_int i^"|"))) (fun (k,l)->("F"^string_of_int(AtSet.SS.id l)^AtSet.toString l)) h *)
  end

  let diff (t1,_)(t2,_) = (AtSet.diff t1 t2,None)

  module Ext =struct
    include CI
    let compare(a,_)(a',_)    = AtSet.compare a a'
    let compareE              = AtSet.compareE
    let first_diff(a,_)(a',_) = AtSet.first_diff a a'
    let sub alm (s1,f) (s2,g) limit = AtSet.sub alm s1 s2 limit
 end

include Ext

  let choose (t,_) = AtSet.choose t
  let clear ()     = AtSet.clear()
  let id (a,_)     = AtSet.SS.id a
  let latest (_,b) = b
  let cardinal (s,_) = AtSet.cardinal s
  let negations (s,_) = AtSet.SS.fold (fun k accu -> add (Atom.negation k) accu) s empty

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
  val fold : (e -> 'a -> 'a) -> t -> 'a -> 'a
  val toString : t -> string
  val compare : t -> t -> int
  val compareE : e -> e -> int
  val sub  : bool->t->t->e option->(unit,e) almost
  val first_diff : t -> t -> e option * bool
  val iter : (e->unit)->t->unit
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
