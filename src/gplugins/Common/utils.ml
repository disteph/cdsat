(*******************************************************************)
(* This module contains extensions of the kernel's FrontEnd module:
   some useful abbreviations/functions, and most importantly, a
   memoisation module.

   These have no reason to be in the kernel, which we try to keep as
   small as possible.*)
(*******************************************************************)


open Kernel.Interfaces_I
open Kernel.Formulae
open Kernel.Interfaces_II

open Lib
open Sums
open Patricia
open SetConstructions
open SetInterface

module PHCons_ext(A:PHCons) = struct
  include A
  let equal a1 a2 = (id a1 = id a2)
  let hash = id
end

module FEext(FE:FrontEndType)
  = struct
    open FE
    (* A function to systematically accept answers *)
    let accept _ = ()
    let fNone () = None
    let isProvable = function Provable _ -> true | _ -> false
    let isNotProvable = function NotProvable _ -> true | _ -> false
    let model seq = let (a,_)=Seq.simplify seq in a

  end


(* API for Memoisation of results.
   
   Now we provide tools for memoising the proof-search function.
   Provided that plugin gives a bit more information about its data
   structures, module Memo provides 4 memoisation handling
   functions, and one function to clear the memoisation table.

   A plugin can store the result of a search with f:tomem
   It can search for a previously obtained result with g:tosearch

   For the plugin to use these functions, it must construct a
   memoisation table by providing more structure than just F, FSet,
   ASet, i.e. providing small extensions of FSet and ASet *)

module Memo
  (IAtom:IAtomType)
  (FE:FrontEndType with type dsubsts = IAtom.DSubst.t and type Form.lit = IAtom.Atom.t)
  (FSet: CollectImplemExt with type e = (FE.Form.datatype,FE.Form.lit) GForm.t * IAtom.DSubst.t and type t=FE.fsetType)
  (ASet: CollectImplemExt with type e = IAtom.t and type t=FE.asetType)
  = struct
    open FE

    module D = struct
      type keys    =  ASet.t*FSet.t
      let kcompare (a1,f1)(a2,f2) =
	let c=ASet.compare a1 a2 in
	  if c==0 then FSet.compare f1 f2 else c
      type values  = t*int
      let vcompare = Pervasives.compare
      type infos     = unit
      let info_build = empty_info_build
      let treeHCons  = false
    end

    module EASet = struct include ASet type keys=D.keys let tag(a,b)=a end
    module EFSet = struct include FSet type keys=D.keys let tag(a,b)=b end

    module UT=LexProduct(TypesFromCollect(EASet))(TypesFromCollect(EFSet))

    let sub = UT.sub ASet.sub FSet.sub
    let sup alm f f' = sub alm f' f

    module MP = PATMap(D)(UT)

    let byes j x = x
    let bempty   = (ASet.empty,FSet.empty)
    let bsingleton j x = function
      | A(a) -> (ASet.add a ASet.empty,FSet.empty)
      | F(a) -> (ASet.empty,FSet.add a FSet.empty)
    let bunion (a,b)(a',b')=if ASet.is_empty a&&FSet.is_empty b then (a',b') else (a,b)
    (* let bunion (a,b)(a',b')=(ASet.union a a',FSet.union b b')  *)

    let find_sub alm (k1,k2) =
      let filter =function
	| F f  -> alm
	| A ia -> alm &&
          let (a,tl) = IAtom.reveal ia in
          (not (ASet.is_in (IAtom.build (IAtom.Atom.negation a,tl)) k1)) && not (FSet.is_in (Form.lit a,tl) k2)
      in
	MP.find_su byes bsingleton bempty bunion (sub alm) true filter (fun _-> true) (k1,k2)

    let find_sup alm = MP.find_su byes bsingleton bempty bunion (sup alm) false (fun _-> true) (fun _-> true) 

    let tableS = ref MP.empty
    let tableF = ref MP.empty

    let size () = (MP.cardinal !tableS) + (MP.cardinal !tableF)

    let tomem ans = 
      let (table,algo,b) = match ans with
	| Provable(s,_,_)-> (tableS,find_sub false,true)
	| NotProvable(s) -> (tableF,find_sup false,false)
      in
      let s = sequent ans in
      let k = Seq.simplify s in
      match algo k !table with
      | F _ -> Dump.Plugin.incr_count 4;
	Dump.msg None
          (Some(fun p->p "%i/%i Recording %s for\n%a" (Dump.Plugin.read_count 4) (Dump.Plugin.read_count 5) (if b then "success" else "failure") Seq.print_in_fmt s))
          (Some 4);
	table := MP.add k (fun _ -> (ans, 1)) !table;
      | A a -> Dump.Plugin.incr_count 5;
	Dump.msg None
          (Some(fun p->p "%i/%i Already know better %s than\n%a" (Dump.Plugin.read_count 4) (Dump.Plugin.read_count 5) (if b then "success" else "failure") Seq.print_in_fmt s))
          (Some 5);
        Dump.msg None (Some(fun p->p "%a" )) (Some 5)

    let search4success b s = find_sub b (Seq.simplify s) !tableS
    let search4failure b s = find_sup b (Seq.simplify s) !tableF

    let cut_series seq alternative (a,f) =
      if ASet.is_empty a then
	if FSet.is_empty f then
          (Dump.msg None (Some(fun p->p "Found no previous success for %a" Seq.print_in_fmt seq)) None;
           Dump.Plugin.incr_count 9;
	   alternative())
	else let (toCut,_)=FSet.next f in
	     (Dump.Plugin.incr_count 6; (*Never happens in DPLL_WL*)
              let (toCutf,_)= toCut in
              Dump.msg None (Some (fun p->p "Found approx. in pos form of\n%a\n%a" Seq.print_in_fmt seq Form.print_in_fmt toCutf)) None;
	      Some(Cut(7,toCut,(fun _->()),(fun _->()),(fun _-> None))))
      else let (toCut,_)=ASet.next a in
	   Dump.Plugin.incr_count 7;
           Dump.msg None (Some(fun p->p "Found approx. in atoms of\n%a\n%a" Seq.print_in_fmt seq IAtom.print_in_fmt toCut)) None;
	   let (toCut,tl)= IAtom.reveal toCut in
           Some(Cut(7,(Form.lit toCut,tl),(fun _->()),(fun _->()),(fun _->None)))

    let get_usage_stats4provable ans =
      snd (MP.find (Seq.simplify (sequent ans)) !tableS)

    let reset_stats4provable ans =
      tableS := MP.add (Seq.simplify (sequent ans)) (function None -> failwith "Sequent should be in the table" | Some _ -> (ans, 0)) !tableS

    let search4provableNact seq alternative () =
      match search4success !Flags.almo seq with
      | A(a) 
        -> Dump.Plugin.incr_count 8;
          Dump.msg None (Some(fun p->p "Found previous success for %a" Seq.print_in_fmt seq)) None;
          let ans, count = a in
          tableS := MP.add (Seq.simplify (sequent ans)) (function None -> failwith "Sequent should be in the table" | Some _ -> (ans, count+1)) !tableS;
          Some(Propose ans)
      | F(d1,d2) -> cut_series seq alternative (d1,d2)

    let search4notprovableNact seq alternative =
      match search4failure false seq with
      | A(a) -> Dump.msg None (Some(fun p->p "Found previous failure for %a" Seq.print_in_fmt seq)) None;
	Propose (fst a)
      | _    -> Dump.msg None (Some(fun p->p "Found no previous failure for %a" Seq.print_in_fmt seq)) None;
	alternative()

    let report() = 
      print_endline("   Memoisation report:");
      print_endline("Table of provables had "^(string_of_int (MP.cardinal !tableS))^" entries, "
		    ^"Table of non-provables had "^(string_of_int (MP.cardinal !tableF))^" entries")
	
    let clear () = tableS := MP.empty; tableF := MP.empty; MP.clear()
      
  end
