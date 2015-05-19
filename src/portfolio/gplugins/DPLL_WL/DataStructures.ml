(* ******************************************* *)
(* Implementation of sets of atoms for DPLL,
   Implementation of formulae for DPLL,
   Implementation of sets of formulae for DPLL *)
(* ******************************************* *)

open Format

open General
open Kernel

open Interfaces_theory
open Formulae
open Sums
open SetConstructions
open Gplugins_tools.SetInterface


module Generate(ThDS:TheoryDSType) = struct

  open ThDS

  (* **************************************** *)
  (* Implementation of sets of atoms for DPLL *)

  module UASet = struct

    module UT  = struct
      include TypesFromHConsed(IAtom)
      let compare = IAtom.compare
      let print_in_fmt = IAtom.print_in_fmt
      let tString = None
      let keyhash = tag
    end

    module AtSet = Gplugins_tools.Patricia_ext.MyPat(UT)

    type e              = IAtom.t
    type t              = AtSet.t*(e option)
    let empty           = (AtSet.empty, None)
    let is_empty(a,_)   = AtSet.is_empty a
    let union(a,_)(a',_)= (AtSet.union a a',None)
    let inter(a,_)(a',_)= (AtSet.inter a a',None)
    let subset(a,_)(a',_)= AtSet.subset a a'
    let mem l (t,_)   = AtSet.mem l t
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
    let negations (s,_) = AtSet.fold (fun k accu -> add (IAtom.negation k) accu) s empty

  end


  (* *********************************** *)
  (* Implementation of formulae for DPLL *)

  module UF = struct

    type lit = Atom.t
    type t   = DSubst.t -> UASet.t * bool

    let aset (f,tl) = let (r,_) = Formula.data f tl in r
    let fset (f,tl) = let (_,b) = Formula.data f tl in b

    let build = function
      | Lit l        -> fun tl -> UASet.add (iatom_build(l,tl)) UASet.empty, false
      | AndP (x1,x2) -> fun tl -> 
        UASet.union (aset(x1,tl)) (aset(x2,tl)),
        (fset(x1,tl) || fset(x2,tl))
      | _            -> fun tl -> UASet.empty, true

  end




  (* ******************************************* *)
  (* Implementation of sets of formulae for DPLL *)

  module UFSet = struct

    type e    = (UF.lit,UF.t)Formula.generic*DSubst.t
    type mykeys = UASet.t*e

    let aset (a,_) = a
    let form (_,b) = b
    let build f    = (UF.aset f,f)

    module UT0  = TypesFromCollect(struct 
      type t = UASet.t
      type e = IAtom.t
      let mem = UASet.mem
      let inter = UASet.inter
      let compare = UASet.compare
      let compareE   = UASet.compareE
      let first_diff = UASet.first_diff 
      type keys = mykeys
      let tag = aset
    end)

    module UT1  = TypesFromHConsed(struct 
      type t = mykeys
      let id (_,(f,_)) = Formula.id f 
    end)

    module UT2  = TypesFromHConsed(struct 
      type t = mykeys
      let id (_,(_,tl)) = DSubst.id tl 
    end)

    module UT   = struct
      include LexProduct(UT0)(LexProduct(UT1)(UT2))
      let print_in_fmt fmt a = 
        Formula.iprint_in_fmt Atom.print_in_fmt DSubst.print_in_fmt fmt (form a)
      let compare a b = Formula.icompare DSubst.compare (form a) (form b) 
      let cstring fmt (a,_) = () (* fprintf fmt "%i" (ASet.id a) *)
      let bstring fmt = function
	| A(at)-> fprintf fmt "%a" IAtom.print_in_fmt at
	| _    -> fprintf fmt "Bits"
      let tString = None (* Some(cstring,bstring) *)
      let keyhash = UT2.tag
    end

    module FoSet = Gplugins_tools.Patricia_ext.MyPat(UT)

    type t              = FoSet.t

    let empty           = FoSet.empty
    let is_empty        = FoSet.is_empty
    let union           = FoSet.union
    let inter           = FoSet.inter
    let subset          = FoSet.subset
    let mem l t         = FoSet.mem (build l) t
    let add l t         = FoSet.add (build l) t
    let remove l t      = FoSet.remove (build l) t
    let next t          = let (l,t') = FoSet.next t in (form l,t')
    let fold f          = FoSet.fold (fun b-> f (form b))
    let iter            = FoSet.iter
    let print_in_fmt    = FoSet.print_in_fmt
    let compare         = FoSet.compare
    let compareE        = Formula.icompare DSubst.compare
    let first_diff t t' = match FoSet.first_diff t t' with
      | Some a,b -> Some(form a),b
      | None,b -> None,b
    let sub alm t t' limit =
      match
        FoSet.sub alm t t' (match limit with None -> None | Some e -> Some (UF.aset e,e))
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
      | A a-> not (UASet.mem (IAtom.negation a) atms)
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
