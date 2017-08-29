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

  type state = {
      hub      : H.t;                 (* Communication channels with other modules *)
      messages : say answer Pqueue.t; (* The buffer queue for messages *)
      decision : say answer option;   (* The latest decision proposal *)
      waiting4 : AS.t;                (* The agents from which we await an answer *)
      trail    : T.t                  (* The trail *)
    }


  (* Select message function:
     reads input channel and selects a message to process;
     buffers branching requests and makes sure every agent has
     finished talking before processing one of the buffered branching requests
   *)
      
  let rec select_msg state : (say answer * state) Deferred.t =
    match Pqueue.pop state.messages with

    | Some(msg,l) -> return(msg, { state with messages = l } )

    | None ->
       Print.print ["concur",2]
         (fun p-> p "Want to hear from %a at chrono %i"
                    AS.pp state.waiting4 (T.chrono state.trail));
       match state.decision with
       | Some thmsg when AS.is_empty state.waiting4 ->
          return(thmsg, { state with decision = None } )
       | _ ->
          Pipe.read (H.reader state.hub) >>= function
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
                select_msg { state with decision = Some msg }
             | Say m ->
                let () =
                  match m with
                  | WB(_,Sat _) ->
                     Print.print ["concur",2] (fun p->
                         p "Hearing Sat from %a at chrono %i, and buffering"
                           Agents.pp agent chrono);
                     Print.print ["concur",6] (fun p->
                         p " %a" WBE.pp m);
                  | _ ->
                     Print.print ["concur",2] (fun p->
                         p "Hearing from %a at chrono %i, and buffering:\n %a"
                           Agents.pp agent chrono WBE.pp m);
                in
                select_msg { state with messages = Pqueue.push msg state.messages }

  (* Main loop of the master thread *)

  let rec master_loop current state =

    Print.print ["concur",2] (fun p-> p "\nMaster thread enters new loop");

    select_msg state >>= function

    | Try sassign, state ->
       Print.print ["concur",1] (fun p -> p "About to try %a" DS.pp_sassign sassign);

       (* We attempt to create the trail extended with the decision *)
       begin match T.add ~nature:T.Decision sassign state.trail with
       | None -> (* The flip of the decision is in the trail, we ignore the decision *)
          master_loop current state
       | Some trail ->
          (* This is a branching point where we tell all slave workers:
              "Please, clone yourself; here are the new pipes to be used
              for your clone to communicate with me." *)
          H.clone state.hub >>= fun (hub1,hub2) ->
          H.kill state.hub;
          (* Once all slave workers have cloned themselves, we treat the first branch. *)
          Print.print ["concur",1] (fun p ->
              p "Everybody cloned themselves; now starting first branch");

          let assign1   = DS.Assign.add sassign current.assign in
          let current1  = sat_init assign1 ~sharing:current.sharing in
          let newstate1 = { state with
                            hub      = hub1;
                            waiting4 = AS.all;
                            trail }
          in
          (* In the first branch, we broadcast the guess *)
          H.broadcast hub1 sassign (T.chrono newstate1.trail) >>= fun () ->
          (* First recursive call *)
          master_loop current1 newstate1 >>= fun ans ->
          (* Killing the pipes used in the first recursive call *)
          H.kill hub1;
          (* We analyse the answer ans of the recursive call,
             and decide whether to treat the second branch or to backjump further. *)
          begin match ans with
          | Case1(Case2(level,msg_list)) when level=T.level state.trail ->
             Print.print ["concur",0] (fun p -> 
                 p "Backtrack level: %i, Propagations:\n %a"
                   (T.level state.trail)
                   (List.pp WBE.pp) msg_list);
             Print.print ["concur",1] (fun p -> p "%s" "Now starting second branch");
             
             let newstate2 = { state with
                               hub      = hub2;
                               waiting4 = AS.all;
                               messages = let enqueue msg = Pqueue.push(Say msg) in
                                          List.fold enqueue msg_list state.messages }
             in
             master_loop current newstate2
                         
          | _ -> H.kill hub2; return ans
          end
       end         

    | Say(WB(_,msg) as thmsg), state ->

       match msg with
         
       | Propa(tset,Unsat) -> 
          Print.print ["concur",2] (fun p -> p "Treating from buffer:\n %a" pp thmsg);
          (* A theory found a proof. We stop and close all pipes. *)
          let finalise conflict uip =
            H.suicide state.hub conflict uip
          in
          T.analyse state.trail thmsg finalise >>| fun ans ->
          H.kill state.hub;
          Case1 ans

       | Propa(old,Straight bassign) ->
          Print.print ["concur",2] (fun p -> p "Treating from buffer:\n %a" pp thmsg);
          (* A theory deduced a boolean assignment bassign from assignment old. *)
          let sassign = SAssign bassign in
          begin match T.add ~nature:(T.Deduction thmsg) sassign state.trail with
          | None -> (* The flip of bassign is in the trail, we have a conflict *)
             let messages = Pqueue.push (Say(WBE.unsat thmsg)) (Pqueue.empty()) in
             master_loop current { state with messages }
          | Some trail ->
             (* A theory deduced a boolean assignment newa from assignment
               old. We broadcast it to all theories *)
             let assign  = DS.Assign.add sassign current.assign in
             let current = sat_init assign ~sharing:current.sharing in
             let state   = { state with
                             decision = None; (* we cancel remaining decision proposal *)
                             waiting4 = AS.all;
                             trail }
             in
             H.broadcast state.hub sassign (T.chrono state.trail) >>= fun () ->
             master_loop current state
          end

       | Sat {assign; sharing} -> 
          (* A theory found a counter-model newtset. If it is the
                same as the current one, then it means the theory has stamped the
                model for which we were collecting stamps. If not, now
                all other theories need to stamp newtset. *)

          match sat thmsg current with
          | (Done(assign,sharing)) as sat_ans ->
             Print.print ["concur",2] (fun p ->
                 p "All theories were fine with model %a sharing %a"
                   DS.Assign.pp assign
                   DS.TSet.pp sharing);
             (* rest being empty means that all theories have
             stamped the assignment consset as being consistent with them,
             so we can finish, closing all pipes *)
             H.kill state.hub;
             return(Case2 sat_ans)

          | Share toshare ->
             Print.print ["concur",2] (fun p ->
                 p "New variables to share: %a" DS.TSet.pp toshare);
             let sharing = DS.TSet.union toshare current.sharing in
             let current = sat_init current.assign ~sharing in
             let state   = { state with
                             decision = None; (* we cancel remaining decision proposal *)
                             waiting4 = AS.all;
                             trail    = T.chrono_incr state.trail }
             in
             H.share state.hub toshare (T.chrono state.trail) >>= fun () ->
             master_loop current state

          | GoOn current ->
             Print.print ["concur",2] (fun p ->
                 p " still waiting for %a" HandlersMap.pp current.left);
             (* some theories still haven't stamped that assignment
             as being consistent with them,
             so we read what the theories have to tell us *)
             master_loop current state

          | NoModelMatch assign_ref ->
             Print.print ["concur",3] (fun p ->
                 p "Outdated model? saw unexpected %a,\n didn't see expected %a"
                   DS.Assign.pp (DS.Assign.diff assign assign_ref)
                   DS.Assign.pp (DS.Assign.diff assign_ref assign));
             master_loop current state

          | NoSharingMatch sharing_ref ->
             Print.print ["concur",3] (fun p ->
                 p "Outdated sharing? got %a,\n but expected %a"
                   DS.TSet.pp sharing DS.TSet.pp sharing_ref);
             master_loop current state


  let master hub input =

    let treat sassign (trail,tasks) =
      match T.add ~nature:T.Input sassign trail with
      | Some trail -> trail, (H.broadcast hub sassign (T.chrono trail))::tasks
      | None -> failwith "Trail should not fail on input"
    in
    let trail,tasks = DS.Assign.fold treat input (T.init,[]) in
    let state = {
        hub      = hub;
        waiting4 = AS.all;
        messages = Pqueue.empty();
        decision = None;
        trail    = trail
      }
    in
    Deferred.all_unit tasks >>= fun () ->
    master_loop (sat_init input ~sharing:DS.TSet.empty) state >>| function
    | Case1(Case1 conflict) ->
       Print.print ["concur",2] (fun p ->
           p "Came here for a conflict. Was not disappointed.\n %a" pp conflict);
       Case1 conflict
    | Case1(Case2 _) -> failwith "Should not come back to level -1 with a propagation"
    | Case2 msg -> Case2 msg
                         
end
