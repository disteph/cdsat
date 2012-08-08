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
				   let treeHCons = !Flags.treeHCons
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
				   let vcompare s1 s2 = match AtSet.SS.first_diff (AtSet.SS.info) s1 s2 with
				     | (None,_) -> 0
				     | (_,true) -> -1
				     | (_,false)-> 1
				   type infos     = (keys*values) option
				   let info_build = (
				     None,
				     (fun x y -> Some(x,y)),
				     (fun x1 x2
					-> match x1,x2 with
					  | None,_ -> x2
					  | _,None -> x1
					  | Some(y1,v1),Some(y2,v2)->
					      if (tag y1)<(tag y2) || ((tag y1)=(tag y2)&&vcompare v1 v2<0)
					      then x1 else x2)
				   )
				   let treeHCons = !Flags.treeHCons
				 end)
  module SS      = PATMap(TFHC)

  let lleaf(k,x) = if AtSet.is_empty x then SS.empty else SS.leaf(k,x)

  module CI = struct
    type e        = Atom.t
    type t        = SS.t
    let hash      = SS.hash
    let equal     = SS.equal
    let empty     = SS.empty
    let is_empty  = SS.is_empty
    let union     = SS.union AtSet.union
    let inter     = SS.inter AtSet.inter
    let is_in l t =
      let (b,p,tl) = Atom.reveal l in
	(SS.mem (b,p) t)
	&&(AtSet.is_in l (SS.find (b,p) t))
    let add l     =
      let (b,p,tl) = Atom.reveal l in
      let f c = function
	| None   -> AtSet.SS.singleton c
	| Some(d)-> AtSet.add c d
      in SS.add f (b,p) l
    let remove l t =
      let (b,p,tl) = Atom.reveal l in
	SS.remove_aux (fun k x -> lleaf(k,AtSet.remove l x)) (b,p) t
    let next t1 =
      let (_,y) = SS.choose t1 in
      let l = AtSet.SS.choose y in
      (l, remove l t1)
    let toString = SS.toString (fun (k,l)->AtSet.toString l)
    let filter b pred t=
      if SS.mem (b,pred) t
      then SS.leaf((b,pred),SS.find (b,pred) t)
      else empty
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

     module OASet = MyPatriciaACollectImplem

     module UF = OF.FI
     module UFSet = OFSet.CI
     module UASet = OASet.CI
     module Strategy =
       functor (FE:FrontEndType with module F=UF and module FSet=UFSet and module ASet=UASet) -> struct
	 include FE

	 type data = unit
	 let initial_data=()

	 module M:MemoType = struct
	   let compareA t1 t2  =
	     let (b1,pred1,tl1) = Atom.reveal t1 in 
	     let (b2,pred2,tl2) = Atom.reveal t2 in 
	     let c = Pervasives.compare (OASet.SS.tag (b1,pred1))(OASet.SS.tag (b2,pred2)) in
	       if c=0 then Pervasives.compare (Atom.id t1) (Atom.id t2) else c
	   let minA s      = match OASet.SS.info s with
	     | None       -> None
	     | Some(k,v)  -> OASet.AtSet.SS.info v 
	   let diffA       = OASet.SS.diff (fun k x y -> OASet.lleaf(k,OASet.AtSet.SS.diff x y))
	   let first_diffA s1 s2 = match OASet.SS.first_diff OASet.SS.info s1 s2 with
	     | (None,b)      -> (None,b)
	     | (Some(k,x),b) -> let other = if b then s2 else s1 in
		 if OASet.SS.mem k other
		 then  OASet.AtSet.SS.first_diff (OASet.AtSet.SS.info) x (OASet.SS.find k other)
		 else (OASet.AtSet.SS.info x,b)

	   let compareF    = OF.compare
	   let minF        = OFSet.SS.info
	   let diffF       = OFSet.SS.diff
	   let first_diffF = OFSet.SS.first_diff minF
	 end

	 module Me = Memo(M)

	 let rec solve = function
	   | Local ans                    -> Me.clear();ans

	   | Fake(AskFocus(l,seq,machine,_)) when OFSet.is_empty l
	       -> begin match seq with
		 | Seq.EntUF(_,_,formP,formPSaved,_) -> solve(machine(Restore))
		 | _ -> failwith("You gave me a wrong sequent")
	       end
	   | Fake(AskFocus(l,_,machine,_)) when !Flags.treeHCons
	       -> solve (machine (Search(Me.search4failure,accept,Focus(OFSet.SS.choose l, accept))))
	   | Fake(AskFocus(l,_,machine,_))
	     -> solve (machine (Focus(OFSet.SS.choose l, accept)))

	   | Fake(Notify(_,machine,_))     when !Flags.treeHCons
	       -> solve (machine (Some(Me.search4success,accept),(),fun _ -> Mem(Me.tomem,Accept)))
	   | Fake(Notify(_,machine,_))    
	     -> solve (machine (None,(),fun _ -> Exit(Accept)))

	   | Fake(AskSide(seq,machine,_)) -> solve (machine true)
	   | Fake(Stop(b1,b2, machine))   -> solve (machine ())

       end
   end:User)
