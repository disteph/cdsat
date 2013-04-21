(* ******************************************* *)
(* Implementation of sets of atoms for DPLL,
   Implementation of formulae for DPLL,
   Implementation of sets of formulae for DPLL *)
(* ******************************************* *)

open Lib
open Kernel

open Formulae
open Interfaces
open Sums
open SetConstructions
open Common.SetInterface


module Generate(Atom:AtomType) = struct

  (* **************************************** *)
  (* Implementation of sets of atoms for DPLL *)


  module ASet = struct

    module AtSet = Common.Patricia_ext.MyPatriciaCollectImplem(Atom)
    include AtSet

    let id          = AtSet.id
    let negations s = AtSet.fold (fun k accu -> add (Atom.negation k) accu) s empty

  end


  (* *********************************** *)
  (* Implementation of formulae for DPLL *)

  module F = struct

    type lit = Atom.t

    type tt = {reveal: (tt,Atom.t) form; id:int; aset: ASet.t option}

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
	     | a, b when a=b              -> true
	     | _                          -> false 
	 let hash t1 =
	   match t1.reveal with
	     | Lit l        -> Atom.hash l
	     | TrueP        -> 1
	     | TrueN        -> 2
	     | FalseP       -> 3
	     | FalseN       -> 4
	     | AndP (x1,x2) -> 5*x1.id+17*x2.id
	     | OrP (x1,x2)  -> 7*x1.id+19*x2.id
	     | AndN (x1,x2) -> 11*x1.id+23*x2.id
	     | OrN (x1,x2)  -> 13*x1.id+29*x2.id
       end: Hashtbl.HashedType with type t=tt)

    include MySmartFormulaImplemPrimitive

    module H = Hashtbl.Make(MySmartFormulaImplemPrimitive)

    let aset_build = function
      | Lit l        -> Some(ASet.add l ASet.empty)
      | AndP (x1,x2) -> (match x1.aset, x2.aset with
			   | Some(a),Some(b) -> Some(ASet.union a b)
			   | Some a,None -> Some a
			   | None,Some b -> Some b
			   | None,None   -> None)
      | _            -> None

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
    let clear() = unique := 0; H.clear table
  end




  (* ******************************************* *)
  (* Implementation of sets of formulae for DPLL *)

  module FSet = struct

    module UF   = PrintableFormula(Atom)(F)
    module UT0  = TypesFromCollect(struct include ASet type keys=t let tag s= s end)
    module UT1  = Lift(struct include UT0 type newkeys=F.t let project = F.aset end)
    module UT2  = struct include UT1 let pequals = UT1.pequals UT0.pequals end
    module UT3  = TypesFromHConsed(struct type t = F.t let id = F.id end)

    module UT   = struct
      include LexProduct(UT2)(UT3)
      let compare = F.compare
      let toString = UF.toString
      let cstring (a,b) = match a with
	| None -> "NC"
	| Some aa-> string_of_int (ASet.id aa)
      let bstring = function
	| A(Some at)-> Atom.toString at
	| A(None)   -> "NC"
	| _ -> "Bits"
      let tString = None (*Some(cstring,bstring)*)
    end

    module SS = Common.Patricia_ext.MyPat(UT)
    include SS

    let sous = UT.sub (UT1.sub (ASet.sub)) (fun _ _ _ _->Yes()) true

    let byes j         = j
    let bempty         = None
    let bsingleton j m = Some j
    let bunion a b = match a,b with
      | None, None   -> None
      | None, Some bb-> Some bb
      | _            -> failwith("Shouldn't be a union here")

    let filter atms =function
      | A(Some a)-> not (ASet.is_in (Atom.negation a) atms)
      | _        -> true

    let schoose atms l =
      find_su byes bsingleton bempty bunion sous true (filter atms) (function None -> true | _ -> false) (Some(atms),-1) l

    let yes _ _ _ = Yes() 

    let rchoose atms l =
      find_su byes bsingleton bempty bunion yes true (filter atms) (function None -> true | _ -> false) (Some(atms),-1) l

  end

end
