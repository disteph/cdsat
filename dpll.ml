open Formulae;;
open Collection;;
open Strategy;;
open MySmart;;
open Sequents;;
open Hashtbl;;

type ('a,'b,'c) dpllaction =
  | Decide of 'b
  | UnitPropagate of 'a
  | Fail of 'a
  | Backtrack of 'a
  | Backjump of 'a*'b*'c  (* provides (
				backjump clause w/o backjump lit C_0,
				backjump lit l_bj,
				remaining model Gamma1
				) 
			     *)
;;

module type DPLLStrategy = 
sig
  module F: FormulaImplem
  module FSet: CollectImplem with type e = F.t
  module ASet: CollectImplem with type e = Atom.t
  val action_pick : ASet.t*FSet.t*FSet.t*FSet.t -> (F.t,Atom.t,FSet.t) dpllaction
end
;;

module DPLLFrontEnd =
  functor (D:DPLLStrategy) ->
    (struct

       include D

       module MySeq = (
	 struct
	   module TMP = FrontEnd(F)(FSet)(ASet)
	   include TMP.Seq
	   let compare = compare
	   let hash = Hashtbl.hash
	   let equal = (=)
	 end)

       (* module Memo = Hashtbl.Make(MySeq)
	 
       let table = Memo.create 5003 *)

       let memo = ref (fun _ -> None)

       let focus_pick (atomsN, formuP, formuPTried, formuPSaved, polar) =
	 let seq = MySeq.EntUF(atomsN, FSet.empty, formuP, formuPTried, formuPSaved, polar) in
	   match !memo seq with
	     | Some x -> x
	     | None   ->
		 match action_pick(atomsN, formuP, formuPTried, formuPSaved) with
		   | Decide(l)                  -> Cut(7,F.build(Lit(l)),accept,accept)
		   | UnitPropagate(c)           -> Focus(c,accept)
		   | Fail(c)                    -> Focus(c,accept)
		   | Backtrack(c)               -> Focus(c,accept)
		   | Backjump(cbj,lbj,gamma1)   -> Cut(3,F.build(OrP(cbj,F.build(Lit(lbj)))),accept,accept)

       let side_pick _ = true

     end:UserStrategy)
;;



module DPLLS =
  (struct
     module F = MyOrderedSmartFormulaImplem
     module FSet = MySmartCollectImplem(struct 
					  include PrintableFormula(F)
					  let compare a b = F.compare a b
					end)
     module ASet = MyCollectImplem(Atom)

     let action_pick (atomsN, formuP, formuPTried, formuPSaved) =
       let c = FSet.choose formuP in
	 UnitPropagate(c)

   end:DPLLStrategy)
;;
