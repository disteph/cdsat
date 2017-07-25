open General
open Patricia
open Top
open Basic
       
module D = struct
  type keys = IntSort.t
  let kcompare = IntSort.compare
  type infos = unit
  let info_build = empty_info_build
  let treeHCons = Some(IntSort.id)
end
             
module IntSortSet = PATSet.Make(D)(SetConstructions.TypesFromHConsed(IntSort))

type t = IntSortSet.t

(* module SortSet = Set.Make(Sorts) *)
(* module SymbSet = Set.Make(Symbols) *)
           
module Make(S: sig
                (* val ksorts : Sorts.t -> bool *)
                val known : Symbols.t -> bool
              end) =
  struct

    type nonrec t = t

    let aux i so = IntSortSet.singleton(IntSort.build(i,so))
    
    let bV tag fv = aux tag (Variables.FreeVar.get_sort fv)

    let bC tag symb l =
      if S.known symb
      then List.fold IntSortSet.union l IntSortSet.empty
      else let so,_ = Symbols.arity symb in
           aux tag so
                            
    let bB tag (_,termB,_) = aux tag (Top.Terms.TermB.get_sort termB)

  end