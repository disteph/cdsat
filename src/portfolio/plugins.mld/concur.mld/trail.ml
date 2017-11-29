(*******************************)
(* Trail and Conflict analysis *)
(*******************************)

open Async
       
open Kernel
open Top
open Sassigns
open Basic
open Messages

open General
open Patricia
open Patricia_interfaces
open Patricia_tools
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

  type max = { level : int;
               chrono : int;
               sassign: unit -> sassign;
               nature : unit -> nature;
               is_uip : unit -> bool; }

  exception Empty

  module DestWInfo = struct
    include SAssign
    type values  = int*int*nature
    type infos   = max

    let info_build = {

        empty_info  = { level = -1;
                        chrono = -1;
                        sassign= (fun()-> raise Empty);
                        nature = (fun()-> raise Empty);
                        is_uip = (fun()-> raise Empty); };

        leaf_info = (fun x (i,j,v) ->
          { level = i;
            chrono = j;
            sassign= (fun()-> SAssign.reveal x);
            nature = (fun()-> v);
            is_uip = (fun()-> true); }
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
  module TrailMap = PatMap.Make(DestWInfo)(TypesFromHConsed(SAssign))

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
         | Decision,SAssign(_, Values.NonBoolean _) -> true
         | _ -> false

  (* Now we define the type for trails *)
                        
  type t = {
      map : TrailMap.t;  (* It contains a trail map *)
      last_chrono : int;
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
  let chrono trail = trail.last_chrono

  (* Increment next timestamp *)
  let chrono_incr trail = { trail with last_chrono = trail.last_chrono+1 }

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
  let init = { map = TrailMap.empty;
               last_chrono = 0;
               late = [] }

  let is_contradicting (SAssign pair) trail =
    match pair with
    | t,Values.Boolean b ->
       let sassign = SAssign.build(SAssign(negation pair)) in
       if TrailMap.mem sassign trail.map
       then Some sassign
       else None
    | _,Values.NonBoolean _ -> None

               
  (* Adds a new assignment on the trail *)
  let add ~nature sassign trail =
    let infos = TrailMap.info trail.map in
    let map level =
      Print.print ["trail",1] (fun p ->
          p "Trail adding %a at level %i and chrono %i as %a"
            pp_sassign sassign
            level
            (trail.last_chrono+1)
            pp_nature nature);
      TrailMap.add
        (SAssign.build sassign)
        (function
         | None -> level,trail.last_chrono,nature
         | Some triple -> triple)
        trail.map
    in
    match nature with
    | Input -> Some { trail with map = map 0;
                                 last_chrono = trail.last_chrono+1 }
    | Decision ->
       begin match is_contradicting sassign trail with
       | Some sassign_neg -> None
       | None -> Some { trail with map = map (infos.level+1);
                                   last_chrono = trail.last_chrono+1 }
       end
    | Deduction msg ->
       begin match is_contradicting sassign trail with
       | Some sassign_neg -> None
       | None -> 
          let level = Pervasives.max (get_data trail.map msg).level 0 in
          if level < infos.level (* Late propagation ! *)
          then 
            Some { map  = map level;
                   late = (msg,infos.level,level)::trail.late;
                   last_chrono = trail.last_chrono+1 }
          else
            Some { trail with map = map level;
                              last_chrono = trail.last_chrono+1  }
       end

  type analysis =
    | InputConflict of unsat WB.t
    | Backjump of { backjump_level : int;
                    propagations   : straight WB.t list;
                    decision       : sassign option }
                    
  let analyse trail conflict learn =
    
    let map  = trail.map in
    let data = get_data map conflict in
    let level= data.level in
    
    let rec aux
              conflict  (* The conflict to analyse; its level will always be level *)
              semsplit  (* Semsplit assignments *)
              data      (* The data about { conflict minus semsplit } *)
      =

      Print.print ["trail",1] (fun p ->
          p "Analysing level %i conflict:\n %a" level WB.pp conflict);

      (* We analyse the nature of the latest assignment contributing to the conflict *)
      try
        match data.nature(),data.sassign() with

        | Input,sassign when (Assign.is_empty semsplit) (* We are not in semsplit *) ->
           (* It's an assignment of the original problem, and so is the whole conflict.
            We stop. *)
           Print.print ["trail",1] (fun p ->
               p "Assignment %a in the conflict is part of the original problem, and so are the others. We stop."
                 pp_sassign sassign);
           return(InputConflict conflict)

        | _,SAssign((_,Values.Boolean _) as bassign)
             (* This is a Backjump situation if the following condition is true *)
             when (Assign.is_empty semsplit) (* We are not in semsplit *)
                  && (data.is_uip())       (* and data.term() is a UIP *)
                  && level > 0             (* and conflict is not of level 0 *)
          -> (* ...we use that UIP to switch branches *)

           let msg = WB.curryfy ~flip:bassign conflict in
           (* First computing the assignments we need for the branch we backjump to *)
           let highest  = data.sassign() in
           let next     = get_data map msg in
           let highest2 = if next.level > -1 then Some(next.sassign()) else None in
           Print.print ["trail",1] (fun p ->
               p "Conflict level: %i; first term: %a; second level: %i; inferred propagation:\n %a"
                 data.level
                 pp_sassign highest
                 next.level
                 WB.pp msg
             );

           (* We learn the conflict, watching first and second highest assignment *)
           learn conflict highest highest2 >>| fun ()->

           (* Now jumping to level of second formula contributing to conflict *)
           Backjump { backjump_level = Compare.max [%ord:int] 0 next.level;
                      propagations   = late next.level trail [msg];
                      decision       = None }

                    
        | Deduction msg, sassign when data.level = level ->
           (* The latest assignment in the conflict has been propagated *)
           Print.print ["trail",1] (fun p ->
               let level,chrono,_ = TrailMap.find (SAssign.build sassign) map in
               p "Explaining %a of level %i and chrono %i, using message\n %a"
                 pp_sassign (data.sassign()) level chrono WB.pp msg);

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

        | _ -> raise Empty

      with

      | Empty ->
         begin
           match Assign.is_empty semsplit with
           | true -> (* This is an Undo case *)
              Print.print ["trail",1] (fun p ->
                  p "Reached decision %a: Undo case" pp_sassign (data.sassign()));
              return None

           | false -> (* This is an UndoDecide case *)
              (* We want to learn the conflict as a clause, so we
               pick 2 formulae to watch. *)
              Print.print ["trail",1] (fun p ->
                  p "Semsplit with level(conflict minus semsplit) = %i and level(conflict) = %i"
                    data.level level);
              let t,rest = Assign.next semsplit in
              let t'     = if Assign.is_empty rest
                           then None
                           else Some(Assign.choose rest)
              in
              (if data.level < level
               then learn conflict t t'
               else return ())
              >>| fun ()-> Some t
         end >>| fun decision ->
         Backjump { backjump_level = level-1;
                    propagations   = late (level-1) trail [];
                    decision }

               
    in aux conflict Assign.empty data
           
end
