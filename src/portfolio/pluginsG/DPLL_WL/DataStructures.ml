(* ******************************************* *)
(* Implementation of sets of atoms for DPLL,
   Implementation of formulae for DPLL,
   Implementation of sets of formulae for DPLL *)
(* ******************************************* *)

open Format

open General
open Kernel
open Prop

open Interfaces_theory
open Literals
open Formulae
open Sums
open SetConstructions
open Tools.PluginsG.SetInterface


(* **************************************** *)
(* Implementation of sets of atoms for DPLL *)

module UT  = struct
  include TypesFromHConsed(LitF)
  let compare = LitF.compare
  let print_in_fmt fmt = LitF.print_in_fmt fmt
  let tString = None
  let keyhash = tag
end

module UASet = struct

  module AtSet = Tools.PluginsG.Patricia_ext.MyPat(UT)

  type e              = LitF.t
  type t              = AtSet.t*(e option)
  let empty           = (AtSet.empty, None)
  let singleton l     = (AtSet.singleton l, Some l)
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
  let negations (s,_) = AtSet.fold (fun k accu -> add (LitF.negation k) accu) s empty

end


(* *********************************** *)
(* Implementation of formulae for DPLL *)

module UF = struct

  type t = UASet.t

  let build = function
    | LitF l       -> UASet.add l UASet.empty
    | AndP (x1,x2) -> UASet.union (FormulaF.data x1) (FormulaF.data x2)
    | _            -> UASet.empty

  let rec fset f = match FormulaF.reveal f with
    | LitF l       -> false
    | AndP (x1,x2) -> (fset x1 || fset x2)
    | _            -> true

end




(* ******************************************* *)
(* Implementation of sets of formulae for DPLL *)

module UFSet = struct

  module UT0  = TypesFromCollect(struct 
    include UASet
    type keys = UF.t FormulaF.generic
    let tag = FormulaF.data
  end)

  module UT1  = TypesFromHConsed(struct 
    type t = UF.t FormulaF.generic
    let id = FormulaF.id
  end)

  module UT   = struct
    include LexProduct(UT0)(UT1)
    let compare = FormulaF.compare
    let print_in_fmt fmt = FormulaF.print_in_fmt fmt
    let cstring fmt ((a,_):common) = fprintf fmt "%a" UASet.print_in_fmt a
    let bstring fmt (g:branching) = match g with
      | Case1 at -> fprintf fmt "%a" (fun fmt->LitF.print_in_fmt fmt) at
      | Case2 _  -> fprintf fmt "Bits"
    let tString = None (* Some(cstring,bstring) *)
    let keyhash = FormulaF.id
  end

  include Tools.PluginsG.Patricia_ext.MyPat(UT)

  let byes j         = j
  let bempty         = None
  let bsingleton j m = Some j
  let bunion a b = match a,b with
    | None, None   -> None
    | None, Some bb-> Some bb
    | _            -> failwith("Shouldn't be a union here")

  let filter atms =function
    | Case1 a -> not (UASet.mem (LitF.negation a) atms)
    | Case2 _ -> true

  let yes _ _ _ = Yes() 

  let rchoose atms l =
    find_su byes bsingleton bempty bunion yes true (filter atms) (function None -> true | _ -> false) (atms,-1) l
    
end

