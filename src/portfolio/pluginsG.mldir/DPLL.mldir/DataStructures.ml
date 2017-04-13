(* ******************************************* *)
(* Implementation of sets of atoms for DPLL,
   Implementation of formulae info for DPLL,
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
  include Tools.PluginsG.Patricia_ext.MyPat(UT)
  let negations s = fold (fun k accu -> add (LitF.negation k) accu) s empty
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
      | Case1 at -> fprintf fmt "%a" (fun fmt -> LitF.print_in_fmt fmt) at
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

