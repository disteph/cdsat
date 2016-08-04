(*********************)
(* Conflict analysis *)
(*********************)

open Kernel
open Combo
open Top
open Messages

open General
open Patricia_interfaces
open Patricia
open SetConstructions
       
(* This module implements conflict analysis *)

module Make(WB : WhiteBoard) = struct

  open WB.DS
         
  module I = TypesFromHConsed(struct include Term let id = Terms.id end)

  (* type nature indicates the status of each formula accumulated in the trail so far.
The reason it was added to it was either:
- It was in the original problem
- It was propagated from other formulae already present in the trail
- It was a decision
- It was tried (i.e. a decision that is not on a formula of the finite basis, and therefore can't just be switched when realising this decision leads to conflict)

The bool in propagated indicates whether we know the level of this propagation for certain, i.e. we have exhibited a dependency path from the decision of that level.
   *)
                             
  type nature =
    | Propagated of straight WB.t
    | Decided of both WB.t
    | Tried
    | Original


  (* When doing conflict analysis, the current set of formulae in conflict is implemented as a Conflict.t below. In that set we want to retain some general information:
- level: the maximal level (or possibly an upper bound on it)
- chrono: the timestamp of the latest formula in the conflict
- term: that formula
- nature: the nature of this formula, as above
- is_uip: is (Some true) if this formula is a UIP, (Some false) if it is definitely not, None if we can't tell - because some other formulae declared of the same level have not confirmed that level *)
                             
  type max = {
      level : int;
      chrono : int;
      term : unit -> Term.t;
      nature : unit -> nature;
      is_uip : unit -> bool;
    }

  module DestWInfo = struct
    type keys    = Term.t
    let kcompare = Terms.compare
    type values  = int*int*nature
    type infos   = max

    let info_build = {

        empty_info  = {
          level = -1;
          chrono = -1;
          term =  (fun()->failwith "Empty TTrack");
          nature = (fun()->failwith "Empty TTrack");
          is_uip = (fun()->failwith "Empty TTrack");
        };

        leaf_info = (fun x (i,j,v) ->
          {
            level = i;
            chrono = j;
            term = (fun()-> x);
            nature = (fun()-> v);
            is_uip = (fun()-> true);
          }
        );

        branch_info = (fun a b ->
          if a.level < b.level then b
          else if a.level > b.level then a
          else
            let c =
              if a.chrono < b.chrono
              then b else a
            in
            { c with
              is_uip = (fun()-> false) }
        );
      }
                       
    let treeHCons  = None (* Some(LitF.id,Terms.id,Terms.equal) *)

  end

  module Trail = PATMap.Make(DestWInfo)(I)


  let rec analyse
            (trail : Trail.t)
            (conflict : unsat WB.t)
          : unsat WB.t * int * Term.t =
    let WB.WB(_,Propa(tset,_)) = conflict in
    let conflictWinfo = Trail.inter_poly (fun _ () v->v) tset trail in
    let info = Trail.info conflictWinfo in
    match info.nature() with
      
    | Decided _
      | Original -> 
       conflict, info.level, info.term()

    | Propagated _ when info.is_uip() && info.level >0 ->
       conflict, info.level, info.term()

    | Propagated msg ->
       analyse trail (WB.resolve msg conflict)   

    | Tried -> failwith "TODO"


                           
end
