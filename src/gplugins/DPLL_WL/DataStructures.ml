(* ******************************************* *)
(* Implementation of sets of atoms for DPLL,
   Implementation of formulae for DPLL,
   Implementation of sets of formulae for DPLL *)
(* ******************************************* *)

open Lib
open Kernel

open Interfaces
open Formulae
open Sums
open SetConstructions
open Common.SetInterface


module Generate(Atom:AtomType) = struct

  (* **************************************** *)
  (* Implementation of sets of atoms for DPLL *)

  module ASet = struct

    module AtSet = Common.Patricia_ext.MyPatriciaCollectImplem(Atom)

    type e              = Atom.t
    type t              = AtSet.t*(e option)
    let hash(a,_)       = AtSet.hash a
    let equal(a,_)(a',_)= AtSet.equal a a'
    let empty           = (AtSet.empty, None)
    let is_empty(a,_)   = AtSet.is_empty a
    let union(a,_)(a',_)= (AtSet.union a a',None)
    let inter(a,_)(a',_)= (AtSet.inter a a',None)
    let subset(a,_)(a',_)= AtSet.subset a a'
    let is_in l (t,_)   = AtSet.is_in l t
    let add l (h,_)     = (AtSet.add l h,Some l)
    let remove l (h,_)  = (AtSet.remove l h, None)
    let next (t1,a)     = 
      let (l,t2) = AtSet.next t1 in
	(l, (t2,None))
    let fold f (a,_)    = AtSet.fold f a
    let toString (h,_)  = AtSet.toString h 
    let diff (t1,_)(t2,_)     = (AtSet.diff t1 t2,None)
    let compare(a,_)(a',_)    = AtSet.compare a a'
    let compareE              = AtSet.compareE
    let first_diff(a,_)(a',_) = AtSet.first_diff a a'
    let sub alm (s1,f) (s2,g) limit = AtSet.sub alm s1 s2 limit
    let choose (t,_)    = AtSet.choose t
    let clear ()        = AtSet.clear()
    let id (a,_)        = AtSet.id a
    let latest (_,b)    = b
    let cardinal (s,_)  = AtSet.cardinal s
    let negations (s,_) = AtSet.fold (fun k accu -> add (Atom.negation k) accu) s empty

  end


  (* *********************************** *)
  (* Implementation of formulae for DPLL *)

  module F = struct

    type lit = Atom.t

    type tt = {reveal: (tt,Atom.t) form; id:int; aset: ASet.t; fset: bool}

    let id f   = f.id
    let aset f = f.aset
    let fset f = f.fset
      
    (* HashedType for formulae *)

    module MySmartFormulaImplemPrimitive = 
      (struct
	 type t = tt
	 let equal t1 t2 =
	   match t1.reveal,t2.reveal with
	     | Lit l1, Lit l2             -> l1==l2
	     | AndP (x1,x2), AndP (y1,y2) -> x1==y1 && x2==y2
	     | OrP (x1,x2), OrP (y1,y2)   -> x1==y1 && x2==y2
	     | AndN (x1,x2), AndN (y1,y2) -> x1==y1 && x2==y2
	     | OrN (x1,x2), OrN (y1,y2)   -> x1==y1 && x2==y2
	     | ForAll x, ForAll y         -> x==y
	     | Exists x, Exists y         -> x==y
	     | a, b                       -> a=b
	 let hash t1 =
	   match t1.reveal with
	     | Lit l        -> Atom.id l
	     | TrueP        -> 1
	     | TrueN        -> 2
	     | FalseP       -> 3
	     | FalseN       -> 4
	     | AndP (x1,x2) -> 5*x1.id+17*x2.id
	     | OrP (x1,x2)  -> 7*x1.id+19*x2.id
	     | AndN (x1,x2) -> 11*x1.id+23*x2.id
	     | OrN (x1,x2)  -> 13*x1.id+29*x2.id
             | ForAll x     -> 31*x.id
             | Exists x     -> 37*x.id
       end: Hashtbl.HashedType with type t=tt)

    include MySmartFormulaImplemPrimitive

    module H = Hashtbl.Make(MySmartFormulaImplemPrimitive)

    let aset_build = function
      | Lit l        -> ASet.add l ASet.empty
      | AndP (x1,x2) -> ASet.union x1.aset x2.aset
      | _            -> ASet.empty

    let fset_build = function
      | Lit l        -> false
      | AndP (x1,x2) -> x1.fset||x2.fset
      | _            -> true

    (* Constructing a formula with HConsing techniques *)

    let table = H.create 5003
    let funique =ref 0
    let build a =
      let f = {reveal =  a; id = !funique; aset = aset_build a; fset = fset_build a} in
      try H.find table f
      with Not_found -> incr funique; H.add table f f; f
    let reveal f = f.reveal

    let compare t1 t2 = Pervasives.compare t1.id t2.id	  
    let clear() = H.clear table
  end




  (* ******************************************* *)
  (* Implementation of sets of formulae for DPLL *)

  module FSet = struct

    module UF   = PrintableFormula(Atom)(F)
    module UT4  = TypesFromCollect(
      struct
	include ASet 
        type keys=F.t
	let tag = F.aset
      end)

    module UT3  = TypesFromHConsed(struct
				     type t = F.t 
				     let id = F.id 
				   end)

    module UT   = struct
      include LexProduct(UT4)(UT3)
      let compare = F.compare
      let toString = UF.toString
      let cstring (a,b) = match a with
	| None -> "NC"
	| Some aa-> string_of_int (ASet.id aa)
      let bstring = function
	| A(Some at)-> Atom.toString at
	| A(None)   -> "NC"
	| _ -> "Bits"
      let tString = None
	(* Some(cstring,bstring) *)
    end

    include Common.Patricia_ext.MyPat(UT)

    let byes j         = j
    let bempty         = None
    let bsingleton j m = Some j
    let bunion a b = match a,b with
      | None, None   -> None
      | None, Some bb-> Some bb
      | _            -> failwith("Shouldn't be a union here")

    let filter atms =function
      | A a-> not (ASet.is_in (Atom.negation a) atms)
      | _  -> true

    let yes _ _ _ = Yes() 

    let rchoose atms l =
      find_su byes bsingleton bempty bunion yes true (filter atms) (function None -> true | _ -> false) (atms,-1) l

  end

end
