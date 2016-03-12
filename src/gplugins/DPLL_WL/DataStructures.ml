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
      let compare = IAtom.compare
      let print_in_fmt = IAtom.print_in_fmt
      let tString = None
      let keyhash = tag
    end

    module AtSet = Common.Patricia_ext.MyPat(UT)

    type e              = IAtom.t
    type t              = AtSet.t*(e option)
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
    let print_in_fmt fmt (h,a)= 
      match a with
      | None   -> Format.fprintf fmt "{ %a }" AtSet.print_in_fmt h
      | Some a -> Format.fprintf fmt "{ %a }" AtSet.print_in_fmt h

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

    type e    = (F.t,F.lit)GForm.t*IAtom.DSubst.t
    type mykeys = ASet.t*e

    let aset (a,_) = a
    let form (_,b) = b
    let build f    = (F.aset f,f)

    module UT0  = TypesFromCollect(struct 
      type t = ASet.t
      type e = IAtom.t
      let is_in = ASet.is_in
      let inter = ASet.inter
      let compare = ASet.compare
      let compareE   = ASet.compareE
      let first_diff = ASet.first_diff 
      type keys = mykeys
      let tag = aset
    end)
    module UT1  = TypesFromHConsed(struct 
      type t = mykeys
      let id (_,(f,_)) = GForm.id f 
    end)
    module UT2  = TypesFromHConsed(struct 
      type t = mykeys
      let id (_,(_,tl)) = IAtom.DSubst.id tl 
    end)

    module UT   = struct
      include LexProduct(UT0)(LexProduct(UT1)(UT2))
      let print_in_fmt fmt a = 
        GForm.iprint_in_fmt IAtom.Atom.print_in_fmt IAtom.DSubst.print_in_fmt fmt (form a)
      let compare a b = GForm.icompare IAtom.DSubst.compare (form a) (form b) 
      let cstring fmt (a,_) = () (* fprintf fmt "%i" (ASet.id a) *)
      let bstring fmt = function
	| A(at)-> fprintf fmt "%a" IAtom.print_in_fmt at
	| _    -> fprintf fmt "Bits"
      let tString = None (* Some(cstring,bstring) *)
      let keyhash = UT2.tag
    end

    module FoSet = Common.Patricia_ext.MyPat(UT)

    type t              = FoSet.t

    let empty           = FoSet.empty
    let is_empty        = FoSet.is_empty
    let union           = FoSet.union
    let inter           = FoSet.inter
    let subset          = FoSet.subset
    let is_in l t       = FoSet.is_in (build l) t
    let add l t         = FoSet.add (build l) t
    let remove l t      = FoSet.remove (build l) t
    let next t          = let (l,t') = FoSet.next t in (form l,t')
    let fold f          = FoSet.fold (fun b-> f (form b))
    let iter            = FoSet.iter
    let print_in_fmt    = FoSet.print_in_fmt
    let compare         = FoSet.compare
    let compareE        = GForm.icompare IAtom.DSubst.compare
    let first_diff t t' = match FoSet.first_diff t t' with
      | Some a,b -> Some(form a),b
      | None,b -> None,b
    let sub alm t t' limit =
      match
        FoSet.sub alm t t' (match limit with None -> None | Some e -> Some (F.aset e,e))
      with
      | Yes a    -> Yes a
      | Almost b -> Almost(form b)
      | No       -> No

    let choose t        = form(FoSet.choose t)
    let clear ()        = FoSet.clear()

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
      match 
        FoSet.find_su byes bsingleton bempty bunion yes true (filter atms) (function None -> true | _ -> false) (atms,(-1,-1)) l
      with
      | A a       -> A(form a)
      | F(Some a) -> F(Some(form a))
      | F None    -> F None

  end

end
