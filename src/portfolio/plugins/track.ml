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
- It was propagated from other formulae already present in the trail
- It was a decision
- It was tried (i.e. a decision that is not on a formula of the finite basis, and therefore can't just be switched when realising this decision leads to conflict)

The bool in propagated indicates whether we know the level of this propagation for certain, i.e. we have exhibited a dependency path from the decision of that level.
   *)
                             
  type nature =
    | Propagated of bool * straight WB.t
    | Decided of both WB.t
    | Tried

  let is_exact = function
    | Propagated(b,_) -> b
    | Decided _ | Tried -> true


  module Dest = struct
    type keys    = Term.t
    let kcompare = Terms.compare
    type values  = int*int*nature
    type infos   = unit
    let info_build = empty_info_build
    let treeHCons  = None
  end
                  
  module Trail = PATMap.Make(Dest)(I)

  (* When doing conflict analysis, the current set of formulae in conflict is implemented as a Conflict.t below. In that set we want to retain some general information:
- level: the maximal level (or possibly an upper bound on it)
- chrono: the timestamp of the latest formula in the conflict
- term: that formula
- nature: the nature of this formula, as above
- is_uip: is (Some true) if this formula is a UIP, (Some false) if it is definitely not, None if we can't tell - because some other formulae declared of the same level have not confirmed that level *)
                             
  type max = {
      level : int;
      chrono : int;
      term : Term.t;
      nature : nature;
      is_uip : bool option;
    }

  module DestWInfo = struct
    type keys    = Term.t
    let kcompare = Terms.compare
    type values  = int*int*nature
    type infos   = max

    let info_build = {

        empty_info  = {
          level = 0;
          chrono = 0;
          term = failwith "Empty TTrack";
          nature = failwith "Empty TTrack";
          is_uip = failwith "Empty TTrack";
        };

        leaf_info = (fun x (i,j,v) ->
          {
            level = i;
            chrono = j;
            term = x;
            nature = v;
            is_uip = if is_exact v then Some true else None;
        });

        branch_info = (fun a b ->
          if a.level < b.level then b
          else if a.level > b.level then a
          else
            let c =
              if a.chrono < b.chrono
              then b else a
            in
            { c with
              is_uip =
                match a.is_uip, b.is_uip with
                | Some true, Some true
                  | _, Some false
                  | Some false, _ -> Some false
                | _ -> None
            }
        );
      }
                       
    let treeHCons  = None (* Some(LitF.id,Terms.id,Terms.equal) *)

  end

  module Conflict = PATMap.Make(DestWInfo)(I)

  (* Takes a Propa message, looks at its antecedent set, and adds to it usuful information coming from track.
   *)
  let add_info (msg : _ propa WB.t) (track: (int*int*nature, _) Conflict.param)
      : Conflict.t =
    let WB.WB(_,Propa(tset,_)) = msg in
    Conflict.inter_poly (fun _ () v->v) tset track

                               
  let rec backtrack
            (graph : Conflict.t)
            (stack : straight WB.t list)
            (conflict : unsat WB.t)
          : unsat WB.t * int * Term.t * Conflict.t =
    let tset = add_info conflict graph in
    match (Conflict.info tset).is_uip, stack with
    | Some true, _
      | _, [] -> conflict, (Conflict.info tset).level, (Conflict.info tset).term, graph
    | _,msg::tail -> backtrack graph tail (WB.resolve msg conflict)

                                   
  (* Arguments assumptions:

In our conflict analysis, we have discovered that the level of the conflict is level.

graph implements a conflict set where every formula declared with level level has been confirmed as having level level.

stack is the stack of formulae that we have analysed (replaced by their antecedent in the dependency graph, i.e. those we have "applied resolution to") between the original conflict and conflict graph (latest analysed formula is at the head of the stack).

stack_reverse outputs a pair (graph',stack') where

graph' extends graph and represents the whole dependency graph of the formulae we have seen, with the same invariant as in graph: every formula declared with level level has been confirmed as having level level.

stack' is pretty much a reversed version of argument stack, but we have also pruned it from the formulae that turned out to have a actual level lower than level.
   *)
                               
  let stack_reverse
        (level:int)
        (graph,stack : Conflict.t * (Term.t * int * straight WB.t) list)
      : Conflict.t * straight WB.t list
    =
    let aux (graph,res) (formula,chrono,msg) =
      (* We have analysed formula thinking that it was of level level.
         Here we check that it is really the case.
         Let's look at the info of its antecedents in the dependency graph *)
      let source_info = Conflict.info (add_info msg graph)
      in
      (* We introduce formula in our dependency graph graph with the level and exactness
         from its antecedent *)
      let graph =
        Conflict.add formula
          (fun _ ->
            (source_info.level,chrono,Propagated(is_exact source_info.nature,msg)))
          graph
      in
      graph,
      if source_info.level < level
      then
        (* If the antecedent is of a lower level, we exclude the message from result res *)
        res
      else
        (* If the antecedent is of level level, we include the message in result res *)
        msg::res
    in
    List.fold_left aux (graph,[]) stack
    
    
  (* Assumptions on arguments

ttrack is the tracking information we gathered during the search phase:
it maps each formula we have added to the trail to: the current level at the time of addition, the timestamp of this addition, and the status of that addition

conflict is the current conflict

stack is the stack of formulae that we have analysed (replaced by their antecedent in the dependency graph, i.e. those we have "applied resolution to") between the original conflict and conflict tmp (latest analysed formula is at the head of the stack).

optional argument level, when present, indicates we have confirmed that the level of the  conflict is level.

analyse will typically be started on the original conflict, an empty stack, and no confirmed level, and will gradually transform the conflict, with each recursive calls, until we have determined the level of the conflict and the current conflict satisfies the pre-condition for stack_reverse, at which point it calls stack_reverse.
   *)

  let rec analyse_aux ?level
            (ttrack : Trail.t)
            (conflict : Conflict.t)
            (stack : (Term.t * int * straight WB.t) list)
          : Conflict.t * straight WB.t list =
    
    let info = Conflict.info conflict in
    match level, info.nature with

    (* Conflict level is i, but nothing left of level i is left in conflict, so we stop and start stack_reverse *)
    | Some i, _ when i > info.level -> stack_reverse i (conflict,stack)

    (* Formula of highest level in conflict has confirmed level, so now we know the conflict level for certain. We now need to ensure all other conflicting formulae with that level have confirmed level, so we do continue the analysis, but indicating that we know the level
     *)
    | _, Propagated(exact,msg)
      -> begin
        match info.is_uip with
        | Some _ ->
           let conflict = Conflict.remove info.term conflict in
           let stack = (info.term,info.chrono,msg)::stack in
           analyse_aux ~level:info.level ttrack conflict stack
        | None ->
           let source = add_info msg ttrack in
           let source = Conflict.diff_poly
                          (fun key _ v->Conflict.singleton key v)
                          source
                          conflict in
           let source_info = Conflict.info source in
           if source_info.level < info.level
           then
             (* Mismatch between level declared for formula and level obtained from its antecedents: we reinject the formula in conflict with the level obtained from its antecedents *)
             let conflict =
               Conflict.add info.term
                 (fun _ -> (source_info.level,
                            info.chrono,
                            Propagated(is_exact source_info.nature,msg)))
                 conflict
             in
             analyse_aux ?level ttrack conflict stack
          else
            (* Match between level declared for formula and level obtained from its antecedents: we replace the formula with its antecedents and push the formula with its propagation in the stack *)
            let conflict = Conflict.union
                             (fun _ v->v)
                             source
                             (Conflict.remove info.term conflict) in
            let stack = (info.term,info.chrono,msg)::stack in
            let lev = if exact then Some info.level else level in
            analyse_aux ?level:lev ttrack conflict stack
      end
    (* Formula of highest level in conflict is the actual decision (so has confirmed level). We are in the same situation as in the previous case, except we already know that nothing is left at that level in conflict, which thus satisfies stack_reverse's pre-condition. So we directly call stack_reverse. *)
    | _, Decided both -> stack_reverse info.level (conflict,stack)

    | _, Tried -> failwith "TODO"

  let analyse (ttrack : Trail.t) (conflict : unsat WB.t)
      : unsat WB.t * int * Term.t * Conflict.t =
    let conflictWinfo = add_info conflict ttrack in
    let graph,stack = analyse_aux ttrack conflictWinfo [] in
    backtrack graph stack conflict
                           
end
