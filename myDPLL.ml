open Formulae
open MyPatASet
open Patricia
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
  module UT2  = TypesFromHConsed(struct type t = MyDPLLForm.t let id = MyDPLLForm.id end)

  module UT   = struct
    include LexProduct(UT1)(UT2)
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

  include MyPat(UT)

  let sub1 a1 a2 limit =
    let diffA = MyPatA.diff a1 a2 in
    let treatA x = if MyPatA.is_empty (MyPatA.remove x diffA) then Almost x else No in
      match limit,MyPatA.min diffA with
	| Some a, Some x when MyPatA.compareE x a<0 -> treatA x
	| None  , Some x -> treatA x
	| _     ,_       -> Yes()

  let sub = UT.sub (UT1.sub (fun _->sub1)) (fun _ _ _ _->Yes()) true

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
    find_su sub true (filter atms) (function None -> true | _ -> false) byes bempty bsingleton bunion (Some(atms),-1) l

  let yes _ _ _ = Yes() 

  let rchoose atms l =
    find_su yes true (filter atms) (function None -> true | _ -> false) byes bempty bsingleton bunion (Some(atms),-1) l

end
