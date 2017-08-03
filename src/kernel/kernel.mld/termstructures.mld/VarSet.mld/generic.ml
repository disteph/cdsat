open General
open Patricia
open Patricia_tools
open Top
open Basic
       
module D = struct
  include IntSort
  include EmptyInfo
  let treeHCons = Some(IntSort.id)
end
             
module IntSortSet = PatSet.Make(D)(TypesFromHConsed(IntSort))

(* module SortSet = Set.Make(Sorts) *)
(* module SymbSet = Set.Make(Symbols) *)
           
module Make(S: sig
                (* val ksorts : Sorts.t -> bool *)
                val known : Symbols.t -> bool
              end) =
  struct

    type t = IntSortSet.t

    let aux i so = IntSortSet.singleton(IntSort.build(i,so))
                                       
    let bV tag fv = aux tag (Variables.FreeVar.get_sort fv)

    let bC tag symb l =
      if S.known symb
      then List.fold IntSortSet.union l IntSortSet.empty
      else let so,_ = Symbols.arity symb in
           aux tag so
               
    let bB tag (_,termB,_) = aux tag (Top.Terms.TermB.get_sort termB)

  end
