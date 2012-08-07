open Formulae
open Collection
open Strategy
open Sequents

module MySmartFormulaImplem = struct

  type tt = {reveal: tt form;id:int;size:int}

  let id f = f.id
    
  (* HashedType for formulae *)

  module MySmartFormulaImplemPrimitive = 
    (struct
       type t = tt
       let equal t1 t2 =
	 (* print_endline "equal"; *)
	 match t1.reveal,t2.reveal with
	   | Lit l1, Lit l2             -> l1==l2
	   | AndP (x1,x2), AndP (y1,y2) -> x1==y1 && x2==y2
	   | OrP (x1,x2), OrP (y1,y2)   -> x1==y1 && x2==y2
	   | AndN (x1,x2), AndN (y1,y2) -> x1==y1 && x2==y2
	   | OrN (x1,x2), OrN (y1,y2)   -> x1==y1 && x2==y2
	   | _                          -> false 
       let hash t1 =
	 (* print_endline "hash"; *)
	 match t1.reveal with
	   | Lit l        -> Atom.hash l
	   | AndP (x1,x2) -> 5*x1.id+17*x2.id
	   | OrP (x1,x2)  -> 7*x1.id+19*x2.id
	   | AndN (x1,x2) -> 11*x1.id+23*x2.id
	   | OrN (x1,x2)  -> 13*x1.id+29*x2.id
     end: Hashtbl.HashedType with type t=tt)

  include MySmartFormulaImplemPrimitive

  module H = Hashtbl.Make(MySmartFormulaImplemPrimitive)

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
      let f = {reveal =  a; id = !unique; size = prior a} in
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


open Patricia

module MyPatriciaCollectImplem(M:sig
				 type t
				 val id: t->int
				 val toString: t->string
			       end) = struct
  module TFHC = TypesFromHConsed(struct
				   type keys=M.t
				   let tag f = M.id f
				   type values = unit
				   let vcompare _ _ = 0
				     (* type infos = unit *)
				     (* let info_build = empty_info_build *)
				     (* Alternative, recording min, max, cardinal: *)
				   type infos = keys m_infos
				   let info_build = m_info_build tag Pervasives.compare
				   let treeHCons = true
				 end) 
  module SS = PATSet(TFHC)

  module CI = struct
    type e       = SS.keys
    type t       = SS.t
    let is_empty = SS.is_empty
    let is_in    = SS.mem
    let empty    = SS.empty
    let add      = SS.add
    let union    = SS.union
    let inter    = SS.inter
    let remove   = SS.remove
      (* Alternative, if min or max is recorded:

	 let choose t = match SS.info t with 
	 | _,Some y,_ -> y 
	 | _-> failwith("No Element!")
	 end	 
      *)

    let hash = SS.hash
    let equal = SS.equal

    let next  t1 = let e1 = SS.choose t1 in (e1, remove e1 t1)
    let toString = SS.toString M.toString

  end

  include CI

end

module MyPatriciaACollectImplem = struct

  module AtSet = MyPatriciaCollectImplem(Atom)
  module TFHC = TypesFromHConsed(struct
				   type keys      = bool*Atom.Predicates.t
				   let tag (b,f)  = 2*(Atom.Predicates.id f)+(if b then 1 else 0)
				   type values    = AtSet.t
				   let vcompare   = AtSet.SS.compare
				   type infos     = unit
				   let info_build = empty_info_build
				   let treeHCons  = true
				 end)
  module SS    = PATMap(TFHC)

  module CI = struct
    type e        = Atom.t
    type t        = SS.t
    let empty     = SS.empty
    let is_empty  = SS.is_empty

    let is_in l t =
      let (b,p,tl) = Atom.reveal l in
	(SS.mem (b,p) t)
	&&(not (AtSet.is_empty (SS.find (b,p) t)))

    let add l t   =
      let (b,p,tl) = Atom.reveal l in
      let f c = function
	| None   -> AtSet.SS.singleton c
	| Some(d)-> AtSet.add c d
      in 
	SS.add f (b,p) l t

    let union = 
      let f = AtSet.union in 
      SS.union f

    let inter =
      let f = AtSet.inter in 
      SS.inter f

    let remove l t =
      let (b,p,tl) = Atom.reveal l in
	if not (SS.mem (b,p) t) then t
	else
	  let y = SS.find (b,p) t in
	  let y' = AtSet.remove l y in 
	    if AtSet.is_empty y' then SS.remove (b,p) t
	    else SS.add (fun x _ -> x) (b,p) y' t

    let hash     = SS.hash
    let equal    = SS.equal

    let next t1 =
      let (_,y) = SS.choose t1 in
      let l = AtSet.SS.choose y in
      (l, remove l t1)

    let toString = SS.toString (fun (k,l)->AtSet.toString l)

  end

  include CI

end

module MyPAT =
  (struct

     (* User uses the smart datastructures with hconsing and sets from
	above *)

     module OF    = MySmartFormulaImplem
     module OFSet = MyPatriciaCollectImplem(struct 
					      include OF
					      let id = OF.id
					      module PF = PrintableFormula(OF)
					      let toString = PF.toString
					    end)

     module OASet = MyPatriciaCollectImplem(Atom)

     module UF = OF.FI
     module UFSet = OFSet.CI
     module UASet = struct
       include OASet.CI
       let filter (_:bool) (_:Atom.Predicates.t) (t:t) = t
     end
     module Strategy =
       functor (FE:FrontEndType with module F=UF and module FSet=UFSet and module ASet=UASet) -> struct
	 include FE

	 type data = unit
	 let initial_data=()

	 module M:MemoType = struct
	   let compareF    = OF.compare
	   let minA        = OASet.SS.info
	   let subsetA     = OASet.SS.subset
	   let diffA       = OASet.SS.diff
	   let first_diffA = OASet.SS.first_diff  minA
	   let minF        = OFSet.SS.info
	   let subsetF     = OFSet.SS.subset
	   let diffF       = OFSet.SS.diff
	   let first_diffF = OFSet.SS.first_diff minF
	 end

	 module Me = Memo(M)

	 let rec solve =
	   function
	     | Local ans                    -> ans
	     | Fake(Notify(_,machine,_))    -> solve (machine (Search(Me.tosearch,Accept,(),fun _ -> Mem(Me.tomem,Accept))))
	     | Fake(AskFocus(l,_,machine,_))-> solve (machine (Focus(OFSet.SS.choose l, accept)))
	     | Fake(AskSide(seq,machine,_)) -> solve (machine true)
	     | Fake(Stop(b1,b2, machine))   -> solve (machine ())

       end
   end:User)

