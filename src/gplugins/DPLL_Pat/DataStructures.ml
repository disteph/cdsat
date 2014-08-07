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
      let compare  = IAtom.compare
      let print_in_fmt = IAtom.print_in_fmt
      let tString  = None
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

    module UT0  = TypesFromCollect(struct 
      type t = ASet.t
      type e = IAtom.t
      let is_in = ASet.is_in
      let inter = ASet.inter
      let compare = ASet.compare
      let compareE   = ASet.compareE
      let first_diff = ASet.first_diff 
      type keys = (F.t,F.lit)GForm.t*IAtom.DSubst.t 
      let tag   = F.aset
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
      let cstring fmt ((a,_):common) = fprintf fmt "%a" ASet.print_in_fmt a
      let bstring fmt (g:branching) = match g with
	| A(at)-> fprintf fmt "%a" IAtom.print_in_fmt at
	| _    -> fprintf fmt "Bits"
      let tString = None (* Some(cstring,bstring) *)
    end

    include Common.Patricia_ext.MyPat(UT)

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
      find_su byes bsingleton bempty bunion sous true (filter atms) (function None -> true | _ -> false) (atms,(-1,-1)) l

    let yes _ _ _ = Yes() 

    let rchoose atms l =
      find_su byes bsingleton bempty bunion yes true (filter atms) (function None -> true | _ -> false) (atms,(-1,-1)) l

  end

end
