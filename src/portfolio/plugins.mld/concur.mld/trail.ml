(*******************************)
(* Trail and Conflict analysis *)
(*******************************)

open Async
       
open Kernel
open Top
open Basic
open Messages

open General
open Patricia_interfaces
open Patricia
open SetConstructions
open Sums

(* This module implements conflict analysis *)

module Make(WB : Export.WhiteBoard) = struct

  open WB.DS
         
  (* type nature indicates the status of each single assignment
     accumulated in the trail so far.
The reason it was added to the trail was either:
- It was in the original problem
- It was propagated from other assignments already present in the trail
- It was a decision
   *)
                             
  type nature =
    | Input
    | Deduction of straight WB.t
    | Decision

  let pp_nature fmt = function
    | Input -> Format.fprintf fmt "Input"
    | Deduction msg -> Format.fprintf fmt "Deduction(%a)" WB.pp msg
    | Decision -> Format.fprintf fmt "Decision"

  let show_nature = Print.stringOf pp_nature
      
  (* The main data-structure in the trail is a map from single assignments to a triple
     (level, chrono, nature) where
     - level is the level of the assignment
     - chrono is the identifier of the assignment,
       attributed in chronological order and also called timestamp
     - nature is the nature of the assignment as described above
     We now build that map datastructure. *)

  (* Such maps will also be used for providing useful information
     about a set of single assignments (e.g. a CDSAT conflict).
     For such a set it is useful to know the following information:
- level: the maximal level
- chrono: the timestamp of the latest single assignment in the conflict
- sassign: that assignment
- nature: the nature of this single assignment, as above
- is_uip: is true iff this assignment is a UIP
 *)

  type max = {
      level : int;
      chrono : int;
      sassign: unit -> sassign;
      nature : unit -> nature;
      is_uip : unit -> bool;
    }

  module DestWInfo = struct
    type keys    = SAssign.t
    let kcompare = SAssign.compare
    type values  = int*int*nature
    type infos   = max

    let info_build = {

        empty_info  = {
          level = -1;
          chrono = -1;
          sassign=  (fun()->failwith "Empty Trail");
          nature = (fun()->failwith "Empty Trail");
          is_uip = (fun()->failwith "Empty Trail");
        };

        leaf_info = (fun x (i,j,v) ->
          {
            level = i;
            chrono = j;
            sassign= (fun()-> SAssign.reveal x);
            nature = (fun()-> v);
            is_uip = (fun()-> true);
          }
        );

        branch_info = (fun a b ->
          if a.level < b.level then b
          else if a.level > b.level then a
          else
            let c =
              if a.chrono < b.chrono then b else a
            in
            { c with is_uip = (fun() -> false) }
        );
      }
                       
    let treeHCons  = None (* Some(LitF.id,Terms.id,Terms.equal) *)

  end

  (* We now create the data-structure for Trail Maps *)
  module TrailMap = PATMap.Make(DestWInfo)(TypesFromHConsed(SAssign))

  (* Extracts from a trail map the fragment that is used in the left-hand
  side of a propa message, from which we removed semsplit. I.e. the
  formulae in semsplit do not end up in the extraction. *)
                     
  let get_data trailmap ?(semsplit=Assign.empty) msg =
    let WB.WB(_,Propa(tset,_)) = msg in
    let map = TrailMap.inter_poly (fun _ v () ->v) trailmap (Assign.diff tset semsplit) in
    TrailMap.info map

  (* has_guess data is true iff the highest level is that of a non-Boolean decision *)

  let has_guess data =
    if data.level < 0 then false
    else match data.nature(), data.sassign() with
         | Decision,(_, Values.NonBoolean _) -> true
         | _ -> false

  (* Now we define the type for trails *)
                        
  type t = {
      map : TrailMap.t; (* It contains a trail map *)
      late : (straight WB.t * int * int) list
      (* List of late propagations (head is the latest one),
         each represented as (msg,level,actual_level),
         where msg is the propagation message, level is the current level at the time
         of the addition to the trail, and actual_level is the real level of the 
         propagation. This is useful for backjumping. *)
    }

  (* Extracts the latest level of the trail *)
  let level trail = (TrailMap.info trail.map).level

  (* Extracts the latest timestamp of the trail *)
  let chrono trail = (TrailMap.info trail.map).chrono

  (* Constructs the list of propagations A1::...::An::msg
     where A1,...,An are the propagations of level <= i
     that were added (in chronological order) to the trail after decision i was made. *)
  let late i trail seed =
    let rec aux accu = function
      | [] -> accu
      | (_,level,_)::_ when level <= i -> accu
      | (msg,_,actual_level)::tail when actual_level <= i -> aux (msg::accu) tail
      | _::tail -> aux accu tail
    in
    aux seed trail.late

  (* Empty trail *)
  let init = {
      map = TrailMap.empty;
      late = []
    }
      
  (* Adds a new assignment on the trail *)
  let add ~nature sassign trail =
    Dump.print ["trail",1] (fun p ->
        p "Adding %a as %a" pp_sassign sassign pp_nature nature);
    let infos = TrailMap.info trail.map in
    let map level = TrailMap.add
                      (SAssign.build sassign)
                      (fun _ -> level,infos.chrono+1,nature)
                      trail.map
    in
    match nature with
    | Input -> { trail with map = map 0 }
    | Decision -> { trail with map = map (infos.level+1) }
    | Deduction msg ->
       let level = Pervasives.max (get_data trail.map msg).level 0 in
       if level < infos.level (* Late propagation ! *)
       then 
         { map  = map level;
           late = (msg,infos.level,level)::trail.late }
       else
         { trail with map = map level }
           
                                                             
  let analyse trail conflict learn =
    
    let map  = trail.map in
    let data = get_data map conflict in
    let level= data.level in
    
    let rec aux
              conflict  (* The conflict to analyse; its level will always be level *)
              semsplit  (* Semsplit assignments *)
              data      (* The data about { conflict minus semsplit } *)
            : (unsat WB.t, int * straight WB.t list) sum Deferred.t
      =

      Dump.print ["trail",1] (fun p -> p "Analysing Conflict: %a" WB.pp conflict);

      if data.level < level
      then
        (* Can only happen if everything in the conflict that is
          of level >= level is a semsplit element.  It's time to do
          the split.  We want to learn the conflict as a clause, so we
          pick 2 formulae to watch. *)
        
        let t1,rest = Assign.next semsplit in
        let t2,_    = Assign.next rest in
        learn conflict t1 (Some t2) >>| fun ()->
        (* Now we output the level to backtrack to, and by curryfying the
          semsplit formulae we create a propagation message for second branch *)
        Case2(level-1, late (level-1) trail [WB.curryfy ~assign:semsplit conflict])

      else

        (* Otherwise we analyse the nature of the latest assignment
              contributing to the conflict *)

        match data.nature(),data.sassign() with

        | Input,_ ->
           (* It's an assignment of the original problem, and so is the
               whole conflict.  We stop. *)
           return(Case1 conflict)

        | _,(t,Values.Boolean b)
             (* This is a Backjump situation if the following condition is true *)
             when (Assign.is_empty semsplit) (* We are not in semsplit *)
                  && (data.is_uip())       (* and data.term() is a UIP *)
                  && level > 0             (* and conflict is not of level 0 *)
          -> (* ...we use that UIP to switch branches *)

           let msg = WB.curryfy ~flip:(t,b) conflict in
           (* First computing the assignments we need for the branch we backjump to *)
           let next = get_data map msg in
           (* Second assignment participating to conflict *)
           let second = if next.level < -1 then None
                        else Some(next.sassign()) in
           Dump.print ["trail",1] (fun p ->
               p "Conflict level: %i; first term: %a; second level: %i; second term:%a, inferred propagation:\n%a"
                 data.level
                 pp_sassign (data.sassign())
                 next.level
                 (Opt.pp_print_option SAssign.pp) (Opt.map SAssign.build second)
                 WB.pp msg
             );

           (* We learn the conflict, watching first and second formulae *)
           learn conflict (data.sassign()) second >>| fun ()->

           (* Now jumping to level of second formula contributing to conflict *)
           Case2(next.level, late next.level trail [msg])

                
        | Deduction msg, sassign ->
           (* The latest assignment in the conflict has been propagated *)
           Dump.print ["trail",1] (fun p ->
               let level,chrono,_ = TrailMap.find (SAssign.build sassign) map in
               p "Explaining %a of level %i and chrono %i, using message\n%a"
                 pp_sassign (data.sassign())
                 level chrono
                 WB.pp msg);

           (* We compute the conflict and the semsplit for the recursive call *)
           let conflict,semsplit =
             (* It depends on whether resolve would introduce a Non-Boolean decision *)
             let justif_data = get_data map msg in
             if has_guess justif_data
             then
             (* The latest contributing decision is a guess,
                 and is among the nodes we are about to add:
                 we refuse to resolve, keep the conflict 
                 and add the latest assignment to semsplit *)
               conflict, (Assign.add sassign semsplit)
           else
             (* Otherwise we resolve *)
             (WB.resolve msg conflict), semsplit
           in
           aux conflict semsplit (get_data map ~semsplit conflict)

        | Decision,_ ->
           (* If we have reached a decision and it was not spotted by the UIP case,
              it must have been a non-Boolean decision in the conflict.
              This is an Undo case.
            *)
           return(Case2(level-1, late (level-1) trail []))

           
    in aux conflict Assign.empty data
           
end
