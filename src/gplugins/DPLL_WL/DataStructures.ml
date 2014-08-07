(* ******************************************* *)
(* Implementation of sets of atoms for DPLL,
   Implementation of formulae for DPLL,
   Implementation of sets of formulae for DPLL *)
(* ******************************************* *)

open Format

open Lib
open Kernel

open Interfaces_I
open Formulae
open Sums
open SetConstructions
open Common.SetInterface


module Generate(IAtom:IAtomType) = struct

  module MyIAtomNeg = IAtomNeg(IAtom)

  (* **************************************** *)
  (* Implementation of sets of atoms for DPLL *)

  module ASet = struct

    module UT  = struct
      include TypesFromHConsed(IAtom)
      let compare  = IAtom.compare
      let print_in_fmt = IAtom.print_in_fmt
      let tString  = None
    end

    module AtSet = Common.Patricia_ext.MyPat(UT)

    type e              = IAtom.t
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
    let print_in_fmt fmt (h,_)= AtSet.print_in_fmt fmt h 
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
    let negations (s,_) = AtSet.fold (fun k accu -> add (MyIAtomNeg.negation k) accu) s empty

  end


  (* *********************************** *)
  (* Implementation of formulae for DPLL *)

  module F = struct

    type lit = IAtom.Atom.t
    type t   = IAtom.DSubst.t -> ASet.t * bool

    let aset (f,tl) = let (r,_) = GForm.data f tl in r
    let fset (f,tl) = let (_,b) = GForm.data f tl in b

    let fdata_build = function
      | Lit l        -> fun tl -> ASet.add (IAtom.build(l,tl)) ASet.empty, false
      | AndP (x1,x2) -> fun tl -> 
        ASet.union (aset(x1,tl)) (aset(x2,tl)),
        (fset(x1,tl) || fset(x2,tl))
      | _            -> fun tl -> ASet.empty, true

  end




  (* ******************************************* *)
  (* Implementation of sets of formulae for DPLL *)

  module FSet = struct

    module UT0  = TypesFromCollect(struct 
      type t = ASet.t
      type e = IAtom.t
      let is_in = ASet.is_in
      let inter = ASet.inter
      let compare    = ASet.compare
      let compareE   = ASet.compareE
      let first_diff = ASet.first_diff 
      type keys = (F.t,F.lit)GForm.t*IAtom.DSubst.t 
      let tag = F.aset
    end)
    module UT1  = TypesFromHConsed(struct 
      type t = (F.t,F.lit)GForm.t*IAtom.DSubst.t 
      let id (f,_) = GForm.id f 
    end)
    module UT2  = TypesFromHConsed(struct 
      type t = (F.t,F.lit)GForm.t*IAtom.DSubst.t 
      let id (_,tl) = IAtom.DSubst.id tl 
    end)

    module UT   = struct
      include LexProduct(UT0)(LexProduct(UT1)(UT2))
      let compare      = GForm.icompare IAtom.DSubst.compare
      let print_in_fmt = GForm.iprint_in_fmt IAtom.Atom.print_in_fmt IAtom.DSubst.print_in_fmt
      let cstring fmt (a,b) = fprintf fmt "%i" (ASet.id a)
      let bstring fmt = function
	| A(at)-> fprintf fmt "%a" IAtom.Atom.print_in_fmt at
	| _    -> fprintf fmt "Bits"
      let tString = None (*Some(cstring,bstring)*)
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
      | A a-> not (ASet.is_in (MyIAtomNeg.negation a) atms)
      | _  -> true

    let yes _ _ _ = Yes() 

    let rchoose atms l =
      find_su byes bsingleton bempty bunion yes true (filter atms) (function None -> true | _ -> false) (atms,(-1,-1)) l

  end

end
