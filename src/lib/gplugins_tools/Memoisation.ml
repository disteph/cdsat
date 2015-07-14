(*******************************************************************)
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
(*******************************************************************)


open Kernel.Top.Interfaces_basic
open Kernel.Prop.Interfaces_theory
open Kernel.Prop.Formulae
open Kernel.Prop.Interfaces_plugin

open General
open Sums
open Patricia
open Patricia_interfaces
open SetConstructions
open SetInterface

module Make
  (FE: FrontEndType)
  (FS: CollectImplemExt with type e = FE.FSet.e and type t=FE.FSet.ps)
  (AS: CollectImplemExt with type e = FE.ASet.e and type t=FE.ASet.ps)
  = struct
    open FE

    module D = struct
      type keys    =  AS.t*FS.t
      let kcompare (a1,f1)(a2,f2) =
	let c=AS.compare a1 a2 in
	  if c==0 then FS.compare f1 f2 else c
      type values  = answer*int
      type infos     = unit
      let info_build = empty_info_build
      let treeHCons  = None
    end

    module EASet = struct include AS type keys=D.keys let tag(a,b)=a end
    module EFS = struct include FS type keys=D.keys let tag(a,b)=b end

    module UT=LexProduct(TypesFromCollect(EASet))(TypesFromCollect(EFS))

    let sub = UT.sub AS.sub FS.sub
    let sup alm f f' = sub alm f' f

    module MP = PATMap.Make(D)(UT)

    let byes j x = x
    let bempty   = (AS.empty,FS.empty)
    let bsingleton j x = function
      | A(a) -> (AS.add a AS.empty,FS.empty)
      | F(a) -> (AS.empty,FS.add a FS.empty)
    let bunion (a,b)(a',b')=if AS.is_empty a&&FS.is_empty b then (a',b') else (a,b)
    (* let bunion (a,b)(a',b')=(AS.union a a',FS.union b b')  *)

    let find_sub alm (k1,k2) =
      let filter =function
	| F f  -> alm
	| A ia -> alm && not (AS.mem (LitF.negation ia) k1) (* && not (FS.is_in (Form.lit a,tl) k2) *)
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
      let k = Seq.forPlugin s in
      match algo k !table with
      | F _ -> Dump.Plugin.incr_count 4;
	Dump.msg None
          (Some(fun p->p "%i/%i Recording %s for\n%a" (Dump.Plugin.read_count 4) (Dump.Plugin.read_count 5) (if b then "success" else "failure") Seq.print_in_fmt s))
          (Some 4);
	table := MP.add k (fun _ -> (ans, 1)) !table;
      | A a -> Dump.Plugin.incr_count 5;
	Dump.msg None
          (Some(fun p->p "%i/%i Already know better %s than\n%a" (Dump.Plugin.read_count 4) (Dump.Plugin.read_count 5) (if b then "success" else "failure") Seq.print_in_fmt s))
          (Some 5)

    let search4success b s = find_sub b (Seq.forPlugin s) !tableS
    let search4failure b s = find_sup b (Seq.forPlugin s) !tableF

    let cut_series seq data alternative (a,f) =
      if AS.is_empty a then
	if FS.is_empty f then
          (Dump.msg None (Some(fun p->p "Found no previous success for %a" Seq.print_in_fmt seq)) None;
           Dump.Plugin.incr_count 9;
	   alternative())
	else let (toCut,_)=FS.next f in
	     (Dump.Plugin.incr_count 6; (*Never happens in DPLL_WL*)
              Dump.msg None (Some (fun p->p "Found approx. in pos form of\n%a\n%a" Seq.print_in_fmt seq IForm.print_in_fmt toCut)) None;
	      Some(Cut(7,toCut,data,(fun _->()),(fun _->()),(fun _-> None))))
      else let (toCut,_) = AS.next a in
	   Dump.Plugin.incr_count 8;
           Dump.msg None (Some(fun p->p "Found approx. in atoms of\n%a\n%a" Seq.print_in_fmt seq LitF.print_in_fmt toCut)) None ;
           Some(Cut(7,IForm.lit toCut,data,(fun _->()),(fun _->()),(fun _->None)))

    let get_usage_stats4provable ans =
      snd (MP.find (Seq.forPlugin (sequent ans)) !tableS)

    let reset_stats4provable ans =
      tableS := MP.add (Seq.forPlugin (sequent ans)) (function None -> failwith "Sequent should be in the table - stats" | Some _ -> (ans, 0)) !tableS

    let search4provableNact seq data alternative () =
      match search4success !Flags.almo seq with
      | A(a) 
        -> Dump.Plugin.incr_count 7;
          Dump.msg None (Some(fun p->p "Found previous success for %a" Seq.print_in_fmt seq)) None;
          let ans, count = a in
          tableS := MP.add (Seq.forPlugin (sequent ans)) (function None -> failwith "Sequent should be in the table" | Some _ -> (ans, count+1)) !tableS;
          Some(Propose ans)
      | F(d1,d2) -> cut_series seq data alternative (d1,d2)
     

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
