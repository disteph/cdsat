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
	       val print_in_fmt: Format.formatter -> keys -> unit
	       val tString: ((Format.formatter -> common -> unit)*(Format.formatter -> branching->unit)) option
               val keyhash: keys -> int
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
      let info_build   = m_info_build kcompare
      let treeHCons    = Some(UT.keyhash,fun ()->0)
    end

    include PATSet(D)(UT)

    type e         = UT.keys
    type common    = UT.common
    type branching = UT.branching

    let is_in      = mem
    let print_in_fmt = print_in_fmt UT.tString UT.print_in_fmt
    let next  t1   = let e1 = choose t1 in (e1, remove e1 t1) 
    let compareE   = UT.compare
    let first_diff = first_diff info
    let sub alm s1 s2 limit =
      let locprune t = match limit,info t with
	| Some b,Some x when not (UT.compare x b<0) -> empty
	| _                                         -> t
      in sub locprune alm s1 s2

  end
