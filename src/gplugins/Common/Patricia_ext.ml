(* *************************************************** *)
(* Generic implementation of sets using Patricia trees *)
(* (Extension of the library for Patricia trees and set
   construction)                                       *)
(* *************************************************** *)

open Lib

open Sums
open Patricia
open SetConstructions
open SetInterface

module MyPat(UT:sig
	       include Intern
	       val compare : keys->keys->int
	       val toString: keys->string
	       val tString: ((common -> string)*(branching->string)) option
	     end)
  :MyPatCollect with type e = UT.keys
		and  type common=UT.common
		and  type branching = UT.branching
  = struct

    module D = struct
      type keys        = UT.keys
      let kcompare     = UT.compare
      type values      = unit
      let vcompare _ _ = 0
      type infos       = keys m_infos
      let info_build   = (None,(fun x _ -> Some x),splmin kcompare)
      let treeHCons    = !Flags.memo
    end

    module SS = PATSet(D)(UT)
    include SS

    type e       = UT.keys
    type common    = UT.common
    type branching = UT.branching

    let is_in    = SS.mem
    let toString = SS.toString UT.tString UT.toString
    let next  t1 = let e1 = SS.choose t1 in (e1, SS.remove e1 t1) 
    let compareE   = UT.compare
    let first_diff = SS.first_diff SS.info
    let sub alm s1 s2 limit =
      let locprune t = match limit,SS.info t with
	| Some b,Some x when not (UT.compare x b<0) -> SS.Empty
	| _                                         -> SS.reveal t
      in SS.sub locprune alm s1 s2

  end


(* Generic implementation of sets *)

module MyPatriciaCollectImplem(M:sig
				 type t
				 val id: t->int
				 val compare : t->t->int
				 val toString: t->string
			       end)
  :MyPatCollect with type e = M.t
  =
  MyPat(struct include TypesFromHConsed(M)
	       let compare  = M.compare
	       let toString = M.toString
	       let tString  = None
	end)
