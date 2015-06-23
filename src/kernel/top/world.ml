(**********)
(* Worlds *)
(**********)

open Format
open Basic

module FreeVar = IntSort

let asIntSort a = a

type t = {
  next_eigen : int;
  next_meta  : int;
  ith : FreeVar.t IntMap.t;
  dependencies : int IntMap.t;
}

let init = {next_eigen = 0;
            next_meta  = -1;
            ith = IntMap.empty;
            dependencies = IntMap.empty;
           }

let projE ar =
  let ei = IntMap.find (ar.next_eigen-1) ar.ith in
  let newar = {
    next_eigen = ar.next_eigen-1;
    next_meta  = ar.next_meta;
    ith        = IntMap.remove (ar.next_eigen-1) ar.ith;
    dependencies = IntMap.remove (ar.next_eigen-1) ar.dependencies;
  }
  in ei,newar

let projM ar =
  let mv = IntMap.find (ar.next_meta+1) ar.ith in
  let newar = {
    next_eigen = ar.next_eigen;
    next_meta  = ar.next_meta+1;
    ith        = IntMap.remove (ar.next_meta+1) ar.ith;
    dependencies = IntMap.remove (ar.next_meta+1) ar.dependencies;
  }
  in mv,newar

let liftE so ar =
  let ei = FreeVar.build(ar.next_eigen,so) in
  let newar = {
    next_eigen = ar.next_eigen+1;
    next_meta  = ar.next_meta; 
    ith = IntMap.add ar.next_eigen ei ar.ith;
    dependencies = IntMap.add ar.next_eigen ar.next_meta ar.dependencies;
  }
  in ei,newar

let liftM so ar =
  let mv = FreeVar.build(ar.next_meta,so) in
  let newar = {
    next_eigen = ar.next_eigen;
    next_meta  = ar.next_meta-1; 
    ith = IntMap.add ar.next_meta mv ar.ith;
    dependencies  = IntMap.add ar.next_meta ar.next_eigen ar.dependencies;
  }
  in mv,newar

let equal a1 a2 =
  (a1.next_eigen == a2.next_eigen)
  && (a1.next_meta == a2.next_meta)
  && (IntMap.equal (==) a1.dependencies a2.dependencies)
  && (IntMap.equal (fun a b -> IntSort.compare a b ==0) a1.ith a2.ith)

let prefix a1 a2 =
  (a1.next_eigen <= a2.next_eigen)
  && (a1.next_meta >= a2.next_meta)
  && (IntMap.for_all (fun i dep -> dep == IntMap.find i a2.dependencies) a1.dependencies)
  && (IntMap.for_all (fun i fv -> FreeVar.compare fv (IntMap.find i a2.ith)==0) a1.ith)

let print_in_fmtEM fmt ar = 
  let aux fmt =
    IntMap.fold (fun i mv () -> if i>=0 then Format.fprintf fmt "%i -> #%i; " i (-mv)) ar.dependencies ()
  in
  Format.fprintf fmt "?%i; %t" (-ar.next_meta) aux 

let print_in_fmtME fmt ar = 
  let aux fmt =
    IntMap.fold (fun i mv () -> if i<0 then Format.fprintf fmt "?%i -> #%i; " (-i) mv) ar.dependencies ()
  in
  Format.fprintf fmt "%i; %t" ar.next_eigen aux 

let print_in_fmt = print_in_fmtEM
