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
      hub        : H.t;                 (* Communication channels with other modules *)
      messages   : say answer Pqueue.t; (* The buffer queue for messages *)
      decisions  : say answer Pqueue.t; (* The buffer queue for decisions *)
      waiting4   : AS.t;                (* The agents from which we await an answer *)
      trail      : T.t                  (* The trail *)
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
         (fun p-> p "Want to hear from %a" AS.pp state.waiting4);
       match Pqueue.pop state.decisions with
       | Some(thmsg,l) when AS.is_empty state.waiting4 ->
          return(thmsg, { state with decisions = l } )
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
                select_msg { state with decisions = Pqueue.push msg state.decisions }
             | Say m ->
                Print.print ["concur",2] (fun p->
                    p "Hearing from %a, and buffering:\n %a" Agents.pp agent WBE.pp m);
                select_msg { state with messages = Pqueue.push msg state.messages }

  (* Main loop of the master thread *)

  let rec master_loop ?current state =

    Print.print ["concur",2] (fun p-> p "\nMaster thread enters new loop");

    select_msg state
    >>= function

    | Try sassign, state ->
       Print.print ["concur",1] (fun p -> p "About to try %a" DS.pp_sassign sassign);

       (* This is a branching point where we tell all slave workers:
              "Please, clone yourself; here are the new pipes to be used
              for your clone to communicate with me." *)

       H.clone state.hub >>= fun (hub1,hub2) ->

       (* Once all slave workers have cloned themselves,
              we add sassign to treat the first branch.
              When that finishes with answer ans, we output ans and a thunk to
              trigger the exploration of the second branch. *)

       Print.print ["concur",1] (fun p ->
           p "Everybody cloned themselves; now starting first branch");

       let newstate1 = { state with
                         hub      = hub1;
                         waiting4 = AS.all;
                         trail    = T.add ~nature:T.Decision sassign state.trail }
       in
       H.broadcast hub1 sassign (T.chrono newstate1.trail) >>= fun () ->
       master_loop newstate1 >>= fun ans1 ->
       H.kill hub1;
       begin match ans1 with
       | Case1(Case2(level,msg_list)) when level=T.level state.trail
         ->
          Print.print ["concur",0] (fun p -> 
              p "Backtrack level: %i, Propagations:\n %a"
                (T.level state.trail+1)
                (List.pp WBE.pp) msg_list);
          Print.print ["concur",1] (fun p -> p "%s" "Now starting second branch");
          
          let newstate2 = { state with
                            hub      = hub2;
                            waiting4 = AS.all;
                            messages = let enqueue msg = Pqueue.push(Say msg) in
                                       List.fold enqueue msg_list state.messages }
          in
          master_loop newstate2
                 
       | _ -> H.kill hub2; return ans1
       end

         
    | Say(WB(_,msg) as thmsg), state ->
       Print.print ["concur",2] (fun p -> p "Treating from buffer:\n %a" pp thmsg);
       match msg with
         
       | Sat newtset -> 
          (* A theory found a counter-model newtset. If it is the
                same as tset, then it means the theory has stamped the
                model for which we were collecting stamps. If not, now
                all other theories need to stamp newtset. *)

          let WB(rest, Sat consset) as current =
            match current with
            | Some(WB(_, Sat consset) as c) when DS.Assign.equal consset newtset
              ->
               Print.print ["concur",2] (fun p -> p "Matches previous model");
               sat thmsg c
            | Some(WB(_, Sat consset) as c) when DS.Assign.subset consset newtset
              ->
               Print.print ["concur",2] (fun p -> p "Fuller model");
               c
            | _ ->
               Print.print ["concur",2] (fun p -> p "New model");
               sat_init newtset
          in
          if HandlersMap.is_empty rest
          then
            (Print.print ["concur",2] (fun p ->
                 p "All theories were fine with model %a" DS.Assign.pp consset);
             (* rest being empty means that all theories have
                   stamped the assignment consset as being consistent
                   with them, so we can finish, closing all pipes *)
             H.kill state.hub;
             return(Case2 current))
          else
            (* some theories still haven't stamped that assignment
                 as being consistent with them,
                 so we read what the theories have to tell us *)
            master_loop ~current state

       | Propa(tset,Unsat) -> 
          (* A theory found a proof. We stop and close all pipes. *)
          let finalise conflict uip second_term =
            H.suicide state.hub conflict uip second_term
          in
          T.analyse state.trail thmsg finalise >>| fun ans ->
          H.kill state.hub;
          Case1 ans

       | Propa(old,Straight newa) ->
          let newa = Top.Values.boolassign newa in
          (* A theory deduced a boolean assignment newa from assignment
               old. We broadcast it to all theories *)
          let state = { state with
                        waiting4 = AS.all;
                        trail = T.add
                                  ~nature:(T.Deduction thmsg)
                                  newa
                                  state.trail }
          in
          H.broadcast state.hub newa (T.chrono state.trail) >>= fun () ->
          master_loop state


  let master hub input =

    let treat sassign (trail,tasks) =
      let trail = T.add ~nature:T.Input sassign trail in
      trail, (H.broadcast hub sassign (T.chrono trail))::tasks
    in
    let trail,tasks = DS.Assign.fold treat input (T.init,[]) in
    let state = {
        hub       = hub;
        waiting4  = AS.all;
        messages  = Pqueue.empty();
        decisions = Pqueue.empty();
        trail     = trail
      }
    in
    Deferred.all_unit tasks >>= fun () ->
    master_loop ~current:(sat_init input) state >>| function
    | Case1(Case1 conflict) -> Case1 conflict
    | Case1(Case2 _) -> failwith "Should not come back to level -1 with a propagation"
    | Case2 msg -> Case2 msg
                         
end
