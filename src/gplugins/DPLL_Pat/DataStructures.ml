(* ******************************************* *)
(* Implementation of sets of atoms for DPLL,
   Implementation of formulae info for DPLL,
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

    include Common.Patricia_ext.MyPat(UT)

    let negations s = fold (fun k accu -> add (MyIAtomNeg.negation k) accu) s empty
  end


  (* **************************************** *)
  (* Implementation of formulae info for DPLL *)

  module F = struct

    type lit = IAtom.Atom.t
    type t   = IAtom.DSubst.t -> ASet.t

    let aset (f,tl) = GForm.data f tl

    let fdata_build f tl = match f with
      | Lit l        -> ASet.add (IAtom.build(l,tl)) ASet.empty
      | AndP (x1,x2) -> ASet.union (aset(x1,tl)) (aset(x2,tl))
      | _            -> ASet.empty

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
      let tag   = aset
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
      let compare a b = GForm.icompare IAtom.DSubst.compare (form a) (form b)
      let print_in_fmt fmt a = 
        GForm.iprint_in_fmt IAtom.Atom.print_in_fmt IAtom.DSubst.print_in_fmt fmt (form a)
      let cstring fmt ((a,_):common) = fprintf fmt "%a" ASet.print_in_fmt a
      let bstring fmt (g:branching) = match g with
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

    let sous = UT.sub ASet.sub (fun _ _ _ _->Yes()) true

    let byes j         = j
    let bempty         = None
    let bsingleton j m = Some j
    let bunion a b = match a,b with
      | None, None   -> None
      | None, Some bb-> Some bb
      | _            -> failwith("Shouldn't be a union here")

    let filter atms = function
      | A a-> not (ASet.is_in (MyIAtomNeg.negation a) atms)
      | _  -> true

    let schoose atms l =
      match 
        FoSet.find_su byes bsingleton bempty bunion sous true (filter atms) (function None -> true | _ -> false) (atms,(-1,-1)) l
      with
      | A a       -> A(form a)
      | F(Some a) -> F(Some(form a))
      | F None    -> F None

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
