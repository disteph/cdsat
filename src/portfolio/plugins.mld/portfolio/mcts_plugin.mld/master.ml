(*********************************************************************)
(* Main plugin, implementing the combination of decision procedures
with concurrency, as provided by Jane Street's Async library.

   This is a master-slaves architecture.

   Each slave thread runs the code written in worker.ml, controlling
   the (purely sequential) execution of a decision procedure, and
   exchanging messages with the master thread, whose code is below.  *)
(*********************************************************************)

open Async
       
open Kernel
open Top.Messages
open Top.Sassigns
open Theories.Register
       
open Tools
open Interfaces

open General
open Sums
open Lib

module Make(WB4M: WhiteBoard4Master) = struct
             
  open WB4M
  open WBE

  module T = Trail.Make(WBE)

  module Agents = struct
    type t = Handlers.t option [@@deriving ord]
    let pp fmt = function
      | None -> Format.fprintf fmt "Memo"
      | Some hdl -> Format.fprintf fmt "%a" Handlers.pp hdl
  end
                    
  module AS = struct
    include Set.Make(Agents)
    let all = theories_fold (fun hdl -> add(Some hdl)) (singleton None)
    let pp fmt hdls = List.pp Agents.pp fmt (elements hdls)
  end

  (* module MovesMap = struct
   *   module Map = Map.Make(Agents)
   *   type t = DS.sassign list Map.t
   *   let all = theories_fold (fun hdl -> Map.add(Some hdl)[]) (Map.singleton None [])
   *   type binding = Agents.t*(DS.sassign list) [@@deriving show]
   *   let pp fmt hdls = List.pp pp_binding fmt (Map.bindings hdls)
   * end *)

  module Moves = Set.Make(DS.SAssign)

  type state = {
      hub      : H.t;                 (* Communication channels with other modules *)
      messages : say answer Pqueue.t; (* The buffer queue for messages that are not decisions *)
      moves    : DS.Assign.t;             (* The latest decision proposal *)
      waiting4 : AS.t;                (* The agents from which we await an answer *)
      trail    : T.t;                 (* The trail *)
      current  : sat_tmp
    }


  (* Select message function:
     reads input channel and selects a message to process;
     buffers branching requests and makes sure every agent has
     finished talking before processing one of the buffered branching requests
   *)
      
  let rec select_msg state : (say answer * state) option Deferred.t =
    match Pqueue.pop state.messages with

    | Some(msg,l) -> return(Some(msg, { state with messages = l } ))

    | None ->
      Print.print ["concur",2]
        (fun p-> p "Want to hear from %a at chrono %i"
            AS.pp state.waiting4 (T.chrono state.trail));
      if AS.is_empty state.waiting4 && not(DS.Assign.is_empty state.moves)
      then return None
      else match%bind Pipe.read (H.reader state.hub) with
        | `Eof -> failwith "Eof"
        | `Ok(Msg(agent,msg,chrono)) ->
          let state =
            if (chrono = T.chrono state.trail)&&(AS.mem agent state.waiting4)
            then { state with waiting4 = AS.remove agent state.waiting4 }
            else state
          in
          match msg with
          | Ack ->
            Print.print ["concur",2] (fun p->
                p "Hearing Ack %i from %a" chrono Agents.pp agent);
            select_msg state
          | Try sassign -> 
            Print.print ["concur",2] (fun p->
                p "Hearing guess %a from %a" DS.pp_sassign sassign Agents.pp agent);
            select_msg { state with moves = DS.Assign.add sassign state.moves }
          | Say(WB(_,Sat _)) when chrono < T.chrono state.trail ->
            Print.print ["concur",2] (fun p->
                p "Hearing Sat from %a at old chrono %i, ignoring"
                  Agents.pp agent chrono);
            select_msg state
          | Say m ->
            let () =
              match m with
              | WB(_,Sat _) ->
                Print.print ["concur",2] (fun p->
                    p "Hearing Sat from %a at current chrono %i, and buffering"
                      Agents.pp agent chrono);
                Print.print ["concur",6] (fun p-> p " %a" WBE.pp m);
              | _ ->
                Print.print ["concur",2] (fun p->
                    p "Hearing from %a at chrono %i, and buffering:\n %a"
                      Agents.pp agent chrono WBE.pp m);
            in
            select_msg { state with messages = Pqueue.push msg state.messages }

  type answer    = (T.analysis,sat_ans) sum
  type saturated = Unfinished of state | Finished of state*answer
    
  (* Main loop of the master thread *)
  let rec saturate state : saturated Deferred.t =

    Print.print ["concur",2] (fun p-> p "\nsaturate enters new loop");

    match%bind select_msg state with

    | Some(Say(WB(_,msg) as thmsg), state) ->
      begin
       match msg with
         
       | Propa(tset,Unsat) -> 
          Print.print ["concur",2] (fun p -> p "Treating from buffer:\n %a" pp thmsg);
          (* A theory found a proof. We stop and close all pipes. *)
          (* let g = Let_syntax.bind in *)
          let%map ans = T.analyse state.trail thmsg (H.suicide state.hub) in
          H.kill state.hub;
          Finished(state,Case1 ans)

       | Propa(old,Straight bassign) ->
          Print.print ["concur",2] (fun p -> p "Treating from buffer:\n %a" pp thmsg);
          (* A theory deduced a boolean assignment bassign from assignment old. *)
          let sassign = SAssign bassign in
          begin match T.add ~nature:(T.Deduction thmsg) sassign state.trail with
          | None -> (* The flip of bassign is in the trail, we have a conflict *)
             let messages = Pqueue.push (Say(WBE.unsat thmsg)) (Pqueue.empty()) in
             saturate { state with messages }
          | Some trail ->
             (* A theory deduced a boolean assignment newa from assignment
               old. We broadcast it to all theories *)
             let assign  = DS.Assign.add sassign state.current.assign in
             let current = sat_init assign ~sharing:state.current.sharing in
             let state   = { state with
                             moves = DS.Assign.empty; (* we cancel remaining decision proposal *)
                             waiting4 = AS.all;
                             trail;
                             current }
             in
             H.broadcast state.hub sassign (T.chrono state.trail) >>= fun () ->
             saturate state
          end

       | Sat {assign; sharing} -> 
          (* A theory found a counter-model newtset. If it is the
                same as the current one, then it means the theory has stamped the
                model for which we were collecting stamps. If not, now
                all other theories need to stamp newtset. *)

          match sat thmsg state.current with
          | Done(assign,sharing) as sat_ans ->
             Print.print ["concur",2] (fun p ->
                 p "All theories were fine with model %a sharing %a"
                   DS.Assign.pp assign
                   DS.TSet.pp sharing);
             (* rest being empty means that all theories have
             stamped the assignment consset as being consistent with them,
             so we can finish, closing all pipes *)
             H.kill state.hub;
             return(Finished(state,Case2 sat_ans))

          | Share toshare ->
             Print.print ["concur",2] (fun p ->
                 p "New variables to share: %a" DS.TSet.pp toshare);
             let sharing = DS.TSet.union toshare state.current.sharing in
             let state   = { state with
                             moves = DS.Assign.empty; (* we cancel remaining decision proposal *)
                             waiting4 = AS.all;
                             trail    = T.chrono_incr state.trail;
                             current = sat_init state.current.assign ~sharing }
             in
             H.share state.hub toshare (T.chrono state.trail) >>= fun () ->
             saturate state

          | GoOn current ->
             Print.print ["concur",2] (fun p ->
                 p " still waiting for %a" HandlersMap.pp current.left);
             (* some theories still haven't stamped that assignment
             as being consistent with them,
             so we read what the theories have to tell us *)
             saturate state

          | NoModelMatch assign_ref ->
             Print.print ["concur",3] (fun p ->
                 p "Outdated model? saw unexpected %a,\n didn't see expected %a"
                   DS.Assign.pp (DS.Assign.diff assign assign_ref)
                   DS.Assign.pp (DS.Assign.diff assign_ref assign));
             saturate state

          | NoSharingMatch sharing_ref ->
             Print.print ["concur",3] (fun p ->
                 p "Outdated sharing? got %a,\n but expected %a"
                   DS.TSet.pp sharing DS.TSet.pp sharing_ref);
             saturate state
          end
          
       | _ -> return(Unfinished(state))


  let apply_move sassign state =
       Print.print ["concur",1] (fun p -> p "About to apply move %a" DS.pp_sassign sassign);

       (* We attempt to create the trail extended with the decision *)
       begin match T.add ~nature:T.Decision sassign state.trail with
       | None -> (* The flip of the decision is in the trail, we ignore the decision *)
          return None
       | Some trail ->
          (* This is a branching point where we tell all slave workers:
              "Please, clone yourself; here are the new pipes to be used
              for your clone to communicate with me." *)
          let%bind hub1,hub2 = H.clone state.hub in
          H.kill state.hub;
          (* Once all slave workers have cloned themselves, we treat the first branch. *)
          Print.print ["concur",1] (fun p ->
              p "Everybody cloned themselves; now returning first child state");

          H.broadcast hub1 sassign (T.chrono trail) >>| fun () ->
          let assign1 = DS.Assign.add sassign state.current.assign in
          let state1 = { state with hub = hub1;
                            waiting4 = AS.all;
                            trail;
                                    current = sat_init assign1 ~sharing:state.current.sharing }
          in
          Some(saturate state1)
          (* In the first branch, we broadcast the guess *)
          (* (\* First recursive call *\)
           * let%bind ans = master_loop current1 newstate1 in
           * (\* Killing the pipes used in the first recursive call *\)
           * H.kill hub1;
           * (\* We analyse the answer ans of the recursive call,
           *    and decide whether to treat the second branch or to backjump further. *\)
           * match ans with
           * | Case1(T.Backjump{ backjump_level; propagations; decision })
           *   when backjump_level = T.level state.trail ->
           *   Print.print ["concur",0] (fun p -> 
           *       p "Backtrack level: %i, Propagations:\n %a"
           *         (T.level state.trail) (List.pp WBE.pp) propagations);
           *   Print.print ["concur",1] (fun p -> p "%s" "Now starting second branch");
           *   let messages =
           *     let enqueue msg = Pqueue.push(Say msg) in
           *     List.fold enqueue propagations state.messages
           *   in
           *   let messages = match decision with
           *     | Some dec -> Pqueue.push(Try dec) messages
           *     | None -> messages
           *   in
           *   let newstate2 = { state with hub = hub2; decision = None; messages } in
           *   master_loop current newstate2
           * 
           * | _ -> H.kill hub2; return ans *)
       end
  

  let successors current state =
    DS.Assign.fold (fun move sofar -> (move,1.)::sofar) state.moves [] 

  
  let master hub input =

    let treat sassign (trail,tasks) =
      match T.add ~nature:T.Input sassign trail with
      | Some trail -> trail, (H.broadcast hub sassign (T.chrono trail))::tasks
      | None -> failwith "Trail should not fail on input"
    in
    let trail,tasks = DS.Assign.fold treat input (T.init,[]) in
    let state = { hub;
                  waiting4 = AS.all;
                  messages = Pqueue.empty();
                  moves = DS.Assign.empty;
                  trail;
                  current = sat_init input ~sharing:DS.TSet.empty }
    in
    Deferred.all_unit tasks >>= fun () ->
    failwith "MCTS to plug in"
    (* match%map master_loop state with
     * | Case1(T.InputConflict conflict) ->
     *    Print.print ["concur",2] (fun p ->
     *        p "Came here for a conflict. Was not disappointed.\n %a" pp conflict);
     *    Case1 conflict
     * | Case1(T.Backjump _) -> failwith "Should not come back to level -1 with a propagation"
     * | Case2 msg -> Case2 msg *)
                         
end
