open Formulae
open MyPatASet
open Patricia

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
    let treatA x = if alm&&MyPatA.is_empty (MyPatA.remove x diffA) then Almost(A(x)) else No in
      match limit,MyPatA.min diffA with
	| Some a, Some x when MyPatA.compareE x a<0 -> treatA x
	| None  , Some x -> treatA x
	| _     ,_      -> Yes()

  let sub alm (a,b) (a',b') limit = match a,a',limit with
    | Some aa,Some aa',Some(A a) -> subA alm aa aa' (Some a)
    | Some aa,Some aa', _        -> subA alm aa aa' None
    | None,None,_                -> Yes()
    | None,Some(aa),_            -> print_endline("D"^MyPatA.toString aa);No
    | Some(aa),None,_            -> print_endline("G"^MyPatA.toString aa);No

  module UT    = struct

    type keys    = UF.t
    type common  = MyPatA.t option*int
    let tag s    = (UF.aset s,UF.id s)

    let ccompare (a,b)(a',b')= match a,a' with
      | Some aa,Some aa' ->
	  let d = MyPatA.compare aa aa' in
	    if d=0 then Pervasives.compare b b' else d
      | None,Some _    -> 1
      | Some _ ,None   -> -1
      | None,None      -> Pervasives.compare b b'
	  

    type values      = unit
    let vcompare _ _ = 0

    type infos     = keys m_infos
    let info_build = m_info_build tag ccompare
    let treeHCons  = !Flags.memo

    type branching     = (Atom.t,int)sum
    let bcompare b1 b2 = match b1,b2 with
      | A(a),A(a') -> MyPatA.compareE a a'
      | F(a),F(a') -> Pervasives.compare a a'
      | A(a),F(a') -> -1 
      | F(a),A(a') -> 1 

    let check (a,b) = function
      | A(atm)-> (match a with
		    | None   -> false
		    | Some aa-> MyPatA.is_in atm aa)
      | F(i)  -> (b land i) == 0

    let lowest_bit x        = x land (-x)
    let branching_bit p0 p1 = lowest_bit (p0 lxor p1)
    let mask p m            = p land (m-1)

    let match_prefix (a,b)(a',b') g =
      print_endline("match prefix: ");
      match
	sub false (a,b) (a',b') (Some g),
	sub false (a',b') (a,b) (Some g)
      with
	| Yes _,Yes _ -> (match g with
			    | A atm-> true
			    | F i  -> (mask b i) == b')
	| _    , _    -> false

    let inter a a' =match a,a' with
      | Some aa,Some aa' -> Some(MyPatA.inter aa aa')
      | None,None        -> None
      | _                -> Some(MyPatA.empty)

    let disagree (a,b) (a',b') =
      let m= branching_bit b b' in
      let p = mask b m in
      let bit_side = (b land m) == 0 in
	match a,a' with
	  | Some aa,Some aa' ->
	      (match MyPatA.first_diff aa aa' with
		 | (Some d,c) -> ((Some(MyPatA.inter aa aa'),p),A(d),c)
		 | (None  ,_) -> ((Some(MyPatA.inter aa aa'),p),F(m),bit_side))
	  | Some aa, None  ->
	      (match MyPatA.Ext.min aa with
		 | Some d -> ((Some(MyPatA.empty),p),A(d),true)
		 | None   -> ((Some(MyPatA.empty),p),F(m),bit_side))
	  | None , Some aa ->
	      (match MyPatA.Ext.min aa with
		 | Some d -> ((Some(MyPatA.empty),p),A(d),false)
		 | None   -> ((Some(MyPatA.empty),p),F(m),bit_side))
	  | None,None  -> ((None,p),F(m),bit_side)

  end

  module SS = PATSet(UT)

  let byes j x = j
  let bempty   = None
  let bsingleton j x m = Some j
  let bunion a = function
    | None -> a
    | b    -> b

  let choose atms l =
    SS.PM.find_su sub true (fun _ _ ->true) byes bempty bsingleton bunion true (Some(atms),-1) l

  module PF=PrintableFormula(UF)

  module CI = struct
    type e       = SS.keys
    type t       = SS.t
    let is_empty = SS.is_empty
    let is_in    = SS.mem
    let empty    = SS.empty
    let toString = SS.toString PF.toString
    let add k t  = print_endline(toString t^"///"^PF.toString k);let t'=SS.add k t in print_endline("added-> "^toString  t');t'
    let union    = SS.union
    let inter    = SS.inter
    let remove k t = print_endline(toString t^"///"^PF.toString k);let t'=SS.remove k t in print_endline("removed-> "^toString  t');t'
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

end
