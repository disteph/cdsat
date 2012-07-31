open Formulae;;
open Collection;;
open Strategy;;
open Sequents;;

module MyOrderedSmartFormulaImplem = struct

  type tt = {reveal: tt form;id:int;priority:int}

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

  (* Function computing a priority for each 
   * When picking a formula among a set, the formula with highest priority will be picked first
   * Here, smallest formulae have highest priority
   *)

  let prior = function
    | Lit l        -> - 1
    | AndP (x1,x2) -> - x1.priority - x2.priority
    | OrP (x1,x2)  -> - x1.priority - x2.priority
    | AndN (x1,x2) -> - x1.priority - x2.priority
    | OrN (x1,x2)  -> - x1.priority - x2.priority

  (* Constructing a formula with HConsing techniques *)

  let table = H.create 5003
  let unique =ref 0

  module FI = struct
    type t = tt
    let build a =
      let f = {reveal =  a; id = !unique; priority = prior a} in
	try H.find table f
	with Not_found -> incr unique; H.add table f f; f

    let reveal f = f.reveal
  end

  let build = FI.build
  let reveal = FI.reveal

  let compare t1 t2 =
    let a = (Pervasives.compare t1.priority t2.priority) in
      if (a<>0) then  a else (Pervasives.compare t1.id t2.id)
end;;


open Patricia;;

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
  module SS    = PATSet(TFHC)

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
    let choose   = SS.choose
      (* Alternative, if min or max is recorded:

	 let choose t = match SS.info t with 
	 | _,Some y,_ -> y 
	 | _-> failwith("No Element!")
	 end	 
      *)

    let hash = SS.hash
    let equal = SS.equal

    let next  t1 = let e1 = choose t1 in (e1, remove e1 t1)
    let toString = SS.toString M.toString

  end

  include CI

end;;

module MyPAT =
  (struct

     (* User uses the smart datastructures with hconsing and sets from
     above *)

     module OF    = MyOrderedSmartFormulaImplem
     module OFSet = MyPatriciaCollectImplem(struct 
					      include OF
					      let id = OF.id
					      module PF = PrintableFormula(OF)
					      let toString = PF.toString
					    end)
     module OASet = MyPatriciaCollectImplem(Atom)

     module UF = OF.FI
     module UFSet = OFSet.CI
     module UASet = OASet.CI



     module Strategy =
       functor (FE:FrontEndType with module F=UF and module FSet=UFSet and module ASet=UASet) -> struct
	 include FE

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
	 (*(fun t->print_endline(OFSet.toString t);
	   let f = minF t in
	   match f with
	   | Some(a)-> print_endline("SomeF");f
	   | None   -> print_endline("NoneF");f)*)
	 end

	 module Mem = Memo(M)

	 let rec solve = function
	   | Local ans                  -> ans
	   | Fake(AskFocus(seq,machine))->
	       let receive = function
		 | Local(_) -> Mem(Mem.tomem,Accept)
		 | Fake(_)  -> Accept
	       in
	       let worthatry l = function
		 | Local(_) -> Accept
		 | Fake(_)  -> Action(Focus(FSet.choose l, receive))
	       in
		 solve (machine (match seq with
				   | Seq.EntUF(_,_, l, _, _,_) -> Search(Mem.tosearch,worthatry l)
				   | _ -> failwith("No more formula to place focus on.")
				))
	   | Fake(AskSide(seq,machine)) -> solve(machine true)
	   | Fake(Stop(b1,b2, machine)) -> solve(machine ())

       end
   end:User)
;;
