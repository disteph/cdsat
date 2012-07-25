open Formulae;;
open Collection;;
open Strategy;;


open Hashtbl;;

module MyOrderedSmartFormulaImplem = 
  (struct
     type tt = {reveal: tt form;id:int;priority:int}

     (* HashedType for formulae *)

     module MySmartFormulaImplemPrimitive = 
       (struct
	  type t = tt
	  let equal t1 t2 =
	    (* print_endline "equal"; *)
	    match t1.reveal,t2.reveal with
	      | Lit (b,x1,x2), Lit (c,y1,y2) -> b=c && x1=y1 && x2=y2
	      | AndP (x1,x2), AndP (y1,y2)   -> x1==y1 && x2==y2
	      | OrP (x1,x2), OrP (y1,y2)     -> x1==y1 && x2==y2
	      | AndN (x1,x2), AndN (y1,y2)   -> x1==y1 && x2==y2
	      | OrN (x1,x2), OrN (y1,y2)     -> x1==y1 && x2==y2
	      | _                            -> false 
	  let hash t1 =
	    (* print_endline "hash"; *)
	    match t1.reveal with
	      | Lit (b,x1,x2)   -> Hashtbl.hash (b,x1,x2)
	      | AndP (x1,x2)    -> 5*x1.id+17*x2.id
	      | OrP (x1,x2)     -> 7*x1.id+19*x2.id
	      | AndN (x1,x2)    -> 11*x1.id+23*x2.id
	      | OrN (x1,x2)     -> 13*x1.id+29*x2.id
	end: Hashtbl.HashedType with type t=tt)

     type t = tt
     module H = Hashtbl.Make(MySmartFormulaImplemPrimitive)
     let reveal f = f.reveal

     (* Function computing a priority for each 
      * When picking a formula among a set, the formula with highest priority will be picked first
      * Here, smallest formulae have highest priority
      *)

     let prior = function
       | Lit (b,x1,x2)   -> - 1
       | AndP (x1,x2)    -> - x1.priority - x2.priority
       | OrP (x1,x2)     -> - x1.priority - x2.priority
       | AndN (x1,x2)    -> - x1.priority - x2.priority
       | OrN (x1,x2)     -> - x1.priority - x2.priority

     (* Constructing a formula with HConsing techniques *)

     let table = H.create 5003
     let unique =ref 0
     let build a =
       let f = {reveal =  a; id = !unique; priority = prior a} in
	 try H.find table f
	 with Not_found -> incr unique; H.add table f f; f

     (* Constructing a formula with HConsing techniques *)

     let compare t1 t2 =
       let a = (Pervasives.compare t1.priority t2.priority) in
       if (a<>0) then  a else (Pervasives.compare t1.id t2.id)
   end)
;;

module type PrintableOrderedType = sig 
  include PrintableType
  val compare: t->t->int
end;;

open Set

module MySmartCollectImplem =
  functor (MyPOType:PrintableOrderedType) ->
    (struct
       module SS = Set.Make(MyPOType)
       type e       = SS.elt
       type t       = SS.t
       let is_empty = SS.is_empty
       let is_in    = SS.mem
       let empty    = SS.empty
       let add      = SS.add
       let union    = SS.union
       let remove   = SS.remove
       let choose   = SS.min_elt
       let next  t1 = let e1 = choose t1 in (e1, remove e1 t1)
       let toString t1 = 
	 let rec toString_aux = function
	     [] -> ""
	   | f::[] -> MyPOType.toString(f)
	   | f::l -> MyPOType.toString(f)^", "^(toString_aux l)
	 in
	   toString_aux (SS.elements t1)
     end)
;;

module MySmartUserStrategy =
  (struct
     module F = MyOrderedSmartFormulaImplem
     module FSet = MySmartCollectImplem(struct 
					  include PrintableFormula(F)
					  let compare a b = F.compare a b
					end)
     module ASet = MyCollectImplem(Atom)
     let focus_pick (atomsN, l, formuPTried, formuPSaved, polar) = Focus(FSet.choose l, accept)
     let side_pick _ = true
   end:UserStrategy)
;;
