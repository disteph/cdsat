open Formulae
open MyPatASet
open Patricia
open SetConstructions

(* Implementation of formulae for DPLL,
   Implementation of sets of formulae for DPLL *)


(* Implementation of formulae for DPLL *)

module MyDPLLForm = struct

  type tt = {reveal: tt form; id:int; size:int; aset:MyPatA.t option}

  let id f   = f.id
  let aset f = f.aset
    
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
	   | _                          -> false 
       let hash t1 =
	 match t1.reveal with
	   | Lit l        -> Atom.hash l
	   | AndP (x1,x2) -> 5*x1.id+17*x2.id
	   | OrP (x1,x2)  -> 7*x1.id+19*x2.id
	   | AndN (x1,x2) -> 11*x1.id+23*x2.id
	   | OrN (x1,x2)  -> 13*x1.id+29*x2.id
     end: Hashtbl.HashedType with type t=tt)

  include MySmartFormulaImplemPrimitive

  module H = Hashtbl.Make(MySmartFormulaImplemPrimitive)

  let aset_build = function
    | Lit l        -> Some(MyPatA.add l MyPatA.empty)
    | AndP (x1,x2) -> (match x1.aset, x2.aset with
			 | Some(a),Some(b) -> Some(MyPatA.union a b)
			 | _ -> None) 
    | OrP (x1,x2)  -> None
    | AndN (x1,x2) -> None
    | OrN (x1,x2)  -> None


  (* Computes the size of the formula *)

  let prior = function
    | Lit l        -> - 1
    | AndP (x1,x2) -> - x1.size - x2.size
    | OrP (x1,x2)  -> - x1.size - x2.size
    | AndN (x1,x2) -> - x1.size - x2.size
    | OrN (x1,x2)  -> - x1.size - x2.size

  (* Constructing a formula with HConsing techniques *)

  let table = H.create 5003
  let unique =ref 0

  module FI = struct
    type t = tt
    let build a =
      let f = {reveal =  a; id = !unique; size = prior a; aset = aset_build a} in
	try H.find table f
	with Not_found -> incr unique; H.add table f f; f

    let reveal f = f.reveal
  end

  let build = FI.build
  let reveal = FI.reveal

  let compare t1 t2 =
    let a = (Pervasives.compare t1.size t2.size) in
      if (a<>0) then  a else (Pervasives.compare t1.id t2.id)
end


(* Implementation of sets of formulae for DPLL *)

module MyDPLLFSet = struct

  module UF = MyDPLLForm

  let subA alm a1 a2 limit =
    let diffA = MyPatA.diff a1 a2 in
    let treatA x = if alm&&MyPatA.is_empty (MyPatA.remove x diffA) then Almost(A (Some x)) else No in
      match limit,MyPatA.min diffA with
	| Some a, Some x when MyPatA.compareE x a<0 -> treatA x
	| None  , Some x -> treatA x
	| _     ,_      -> Yes()

  let sub alm (a,b) (a',b') limit = match a,a',limit with
    | Some aa,Some aa',Some(A (Some a)) -> subA alm aa aa' (Some a)
    | Some aa,Some aa', _        -> subA alm aa aa' None
    | None,None,_                -> Yes()
    | None,Some(aa),_            -> No
    | Some(aa),None,_            -> No

  module UT2   = TypesFromHConsed(struct type t = UF.t let id = UF.id end)
  module EASet = struct include MyPatA type keys=t let tag s= s end
  module UT0   = struct include TypesFromCollect(EASet) type newkeys=UF.t let project = UF.aset end
  module UT1   = Lift(UT0)
  module UT    = LexProduct(UT1)(UT2)

  module D = struct
    type keys        = UF.t
    type values      = unit
    let vcompare _ _ = 0
    type infos       = keys m_infos
    let info_build   = m_info_build UT.tag UT.ccompare
    let treeHCons    = !Flags.memo
  end

  module SS = PATSet(D)(UT)

  let byes j = j
  let bempty   = None
  let bsingleton j m = Some j
  let bunion a = function
    | None -> a
    | b    -> b

  let schoose atms l =
    SS.find_su sub true (fun _ _ ->true) byes bempty bsingleton bunion true (Some(atms),-1) l

  module PF=PrintableFormula(UF)

  module CI = struct
    type e       = D.keys
    type t       = SS.t
    let is_empty = SS.is_empty
    let is_in    = SS.mem
    let empty    = SS.empty
    let toString = SS.toString PF.toString
    let add k t  = SS.add k t (*print_endline(toString t^"///"^PF.toString k);let t'=SS.add k t in print_endline("added-> "^toString  t');t'*)
    let union    = SS.union
    let inter    = SS.inter
    let remove k t = SS.remove k t (* print_endline(toString t^"///"^PF.toString k);let t'= in print_endline("removed-> "^toString  t');t'*)
    let hash     = SS.hash
    let equal    = SS.equal
    let next  t1 = let e1 = SS.choose t1 in (e1, remove e1 t1)
  end

  module Ext = struct
    include CI
    let compare    = SS.compare
    let compareE   = UF.compare
    let min        = SS.info
    let diff       = SS.diff
    let first_diff = SS.first_diff min
  end

  include Ext

  let choose = SS.choose

end
