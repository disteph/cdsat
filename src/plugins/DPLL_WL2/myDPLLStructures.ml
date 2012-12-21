open Lib
open Kernel
open Formulae
open MyPatASet
open Patricia
open Sums
open SetConstructions

(* Implementation of formulae for DPLL,
   Implementation of sets of formulae for DPLL *)


(* Implementation of formulae for DPLL *)

module MyDPLLForm = struct

  type tt = {reveal: tt form; id:int; aset:MyPatA.t option}

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

  (* Constructing a formula with HConsing techniques *)

  let table = H.create 5003
  let unique =ref 0

  module FI = struct
    type t = tt
    let build a =
      let f = {reveal =  a; id = !unique; aset = aset_build a} in
	try H.find table f
	with Not_found -> incr unique; H.add table f f; f

    let reveal f = f.reveal
  end

  let build  = FI.build
  let reveal = FI.reveal

  let compare t1 t2 = Pervasives.compare t1.id t2.id
  let clear() = H.clear table
end




(* Implementation of sets of formulae for DPLL *)

module MyDPLLFSet = struct

  module UF   = PrintableFormula(MyDPLLForm)
  module UT0  = TypesFromCollect(struct include MyPatA type keys=t let tag s= s end)
  module UT1  = Lift(struct include UT0 type newkeys=MyDPLLForm.t let project = MyDPLLForm.aset end)
  module UT2  = struct include UT1 let pequals = UT1.pequals UT0.pequals end
  module UT3  = TypesFromHConsed(struct type t = MyDPLLForm.t let id = MyDPLLForm.id end)

  module UT   = struct
    include LexProduct(UT2)(UT3)
    let compare = MyDPLLForm.compare
    let toString = UF.toString
    let cstring (a,b) = match a with
      | None -> "NC"
      | Some aa-> string_of_int (MyPatA.id aa)
    let bstring = function
      | A(Some at)-> Atom.toString at
      | A(None)   -> "NC"
      | _ -> "Bits"
    let tString = None (*Some(cstring,bstring)*)
  end

  module D = struct
    type keys        = UT.keys
    let kcompare     = UT.compare
    type values      = unit
    let vcompare _ _ = 0
    type infos       = (keys m_infos)*MyPatA.t
    let info_build   = ((None,MyPatA.empty),
			(fun x _ -> (Some x,match MyDPLLForm.aset x with None->MyPatA.empty | Some a->a)),
			(fun (a,b)(a',b') ->
			   (splmin kcompare a a',MyPatA.union b b'))
		       )

    let treeHCons    = !Flags.memo
  end

  module SS = PATSet(D)(UT)

  module CI = struct
    type e       = UT.keys
    type t       = SS.t
    let is_empty = SS.is_empty
    let is_in    = SS.mem
    let empty    = SS.empty
    let add      = SS.add
    let union    = SS.union
    let inter    = SS.inter
    let remove   = SS.remove
    let hash     = SS.hash
    let equal    = SS.equal
    let toString = SS.toString UT.tString UT.toString
    let next  t1 = let e1 = SS.choose t1 in (e1, SS.remove e1 t1) 
  end

  module Ext = struct
    include CI
    let compare    = SS.compare
    let compareE   = UT.compare
    let first_diff = SS.first_diff (fun x->let (a,_)= SS.info x in a)
    let sub alm s1 s2 limit =
      let locprune t = match limit,let (a,_)= SS.info t in a with
	| Some b,Some x when not (UT.compare x b<0) -> SS.Empty
	| _                                         -> SS.reveal t
      in SS.sub locprune alm s1 s2
  end

  include Ext

  let choose     = SS.choose
  let clear      = SS.clear
  let cardinal   = SS.cardinal

  let sub = UT.sub (UT1.sub (MyPatA.sub)) (fun _ _ _ _->Yes()) true

  let byes j         = j
  let bempty         = None
  let bsingleton j m = Some j
  let bunion a b = match a,b with
    | None, None   -> None
    | None, Some bb-> Some bb
    | _            -> failwith("Shouldn't be a union here")

  let filter atms =function
    | A(Some a)-> not (MyPatA.is_in (Atom.negation a) atms)
    | _        -> true

  let schoose atms l =
    SS.find_su byes bsingleton bempty bunion sub true (filter atms) (function None -> true | _ -> false) (Some(atms),-1) l

  let smartchoose atms t treat_UP treat_N =
    let rec aux t = match SS.reveal t with
      | SS.Empty           -> (None,t)
      | SS.Leaf(j,x)       -> (match sub (UT.tag j) (Some(atms),-1) None with
				 | Yes _                       -> (Some j,t)
				 | Almost n when filter atms n -> (treat_UP n;(None,t))
				 | _                           -> (treat_N j;(None,empty))
			      )
      | SS.Branch(p,m,l,r) -> (match aux r with
				 | (None,r') -> let (v,l') = aux l in (v,SS.union l' r')
				 | v -> v
			      )
    in aux t
	 
  let yes _ _ _ = Yes() 

  let rchoose atms l =
    SS.find_su byes bsingleton bempty bunion yes true (filter atms) (function None -> true | _ -> false) (Some(atms),-1) l

end
