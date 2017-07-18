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

module Make(WB: sig
                include WhiteBoardExt
                val theories_fold : (Handlers.t -> 'a -> 'a) -> 'a -> 'a
              end)
         (EGraph: Theories.Eq.Interfaces.API
          with type sign = Theories.Eq.MyTheory.sign
           and type termdata = WB.DS.Term.datatype
           and type value  = WB.DS.Value.t
           and type cval   = WB.DS.CValue.t
           and type assign = WB.DS.Assign.t)
  = struct

             
  open WB

  (* We load the code of the slave workers, generated from the
      Whiteboard *)

  module WM = WorkersMap.Make(WB)
  module T  = Trail.Make(WB)

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
      from_workers : msg2pl Pipe.Reader.t; (* The pipe reader where master listens *)
      to_plugin  : msg2pl Pipe.Writer.t;   (* The pipe writer where slaves write *)
      pipe_map   : WM.t;                   (* The pipe writers where master writes *)
      propa      : say answer Pqueue.t;    (* The buffer queue for propagations *)
      decisions  : say answer Pqueue.t;    (* The buffer queue for decisions *)
      waiting4   : AS.t;                   (* The agents from which we await an answer *)
      trail      : T.t                     (* The trail *)
    }

  (* Command to kill the communication pipes between master and slaves *)
  let kill_pipes pipe_map to_plugin =
    WM.broadcast (fun w -> return(Pipe.close w)) pipe_map
    >>| fun () -> Pipe.close to_plugin

  (* Sending a message msg to all slaves *)
  let send pipe_map msg = 
    let treat_worker to_worker =
      Lib.write to_worker msg
    in
    WM.broadcast treat_worker pipe_map

  (* Select message function:
     reads input channel and selects a message to process;
     buffers branching requests and makes sure every agent has
     finished talking before processing one of the buffered branching requests
   *)
      
  let rec select_msg state : (say answer * state) Deferred.t =
    Dump.print ["concur",2]
      (fun p-> p "Want to hear from %a" AS.pp state.waiting4);
    match Pqueue.pop state.propa with

    | Some(msg,l) -> return(msg, { state with propa = l } )

    | None ->
       begin match Pqueue.pop state.decisions with
       | Some(thmsg,l) when AS.is_empty state.waiting4 ->
          return(thmsg, { state with decisions = l } )
       | _ ->
          Pipe.read state.from_workers >>= function
          | `Eof -> failwith "Eof"
          | `Ok(Msg(agent,msg,chrono)) ->
             let state =
               if (chrono = T.chrono state.trail)&&(AS.mem agent state.waiting4)
               then { state with waiting4 = AS.remove agent state.waiting4 }
               else state
             in
             match msg with
             | Ack ->
                Dump.print ["concur",2] (fun p->
                    p "Hearing Ack %i from %a" chrono Agents.pp agent);
                select_msg state
             | Try sassign -> 
                Dump.print ["concur",2] (fun p->
                    p "Hearing guess %a from %a" pp_sassign sassign Agents.pp agent);
                select_msg { state with decisions = Pqueue.push msg state.decisions }
             | Say(WB _) ->
                Dump.print ["concur",2] (fun p->
                    p "Hearing from %a:" Agents.pp agent);
                select_msg { state with propa = Pqueue.push msg state.propa }
       end


  (* Main loop of the master thread *)

  let master from_workers to_pl0 pipe_map assign0 =

    let rec master ?current state =

      Dump.print ["concur",2] (fun p-> p "\nmaster enters new loop");

      select_msg state
      >>= function

      | Try sassign, state ->
         Dump.print ["concur",1] (fun p -> p "About to try %a" WB.pp_sassign sassign);

         (* This is a branching point where we tell all slave workers:
              "Please, clone yourself; here are the new pipes to be used
              for your clone to communicate with me." *)

         let aux to_worker new_from_pl1 new_to_pl1 new_from_pl2 new_to_pl2 =
           Lib.write to_worker(MsgBranch(new_from_pl1,new_to_pl1,new_from_pl2,new_to_pl2))
         in
         let new_from_workers1, new_to_pl1,
             new_from_workers2, new_to_pl2,
             tasks,
             new_pipe_map1,
             new_pipe_map2 =
           WM.clone aux state.pipe_map
         in
         Deferred.all_unit tasks >>= fun () ->

         (* Once all slave workers have cloned themselves,
              we add sassign to treat the first branch.
              When that finishes with answer ans, we output ans and a thunk to
              trigger the exploration of the second branch. *)

         Dump.print ["concur",1] (fun p ->
             p "Everybody cloned themselves; now starting first branch");

         let newstate1 = { state with
                           from_workers = new_from_workers1;
                           to_plugin = new_to_pl1;
                           pipe_map  = new_pipe_map1;
                           waiting4  = AS.all;
                           trail = T.add ~nature:T.Decision sassign state.trail }
         in
         send new_pipe_map1 (MsgStraight(sassign,T.chrono newstate1.trail)) >>= fun () ->
         master newstate1 >>= fun ans1 ->
         kill_pipes state.pipe_map state.to_plugin >>= fun () ->
         begin match ans1 with
         | Case1(Case2(level,msg_list)) when level=T.level state.trail
           ->
            Dump.print ["concur",0] (fun p -> 
                p "Backtrack level: %i, Propagations:\n%a"
                  (T.level state.trail+1)
                  (List.pp WB.pp) msg_list);
            Dump.print ["concur",1] (fun p -> p "%s" "Now starting second branch");
            
            let newstate2 = { state with
                              from_workers = new_from_workers2;
                              to_plugin = new_to_pl2;
                              pipe_map  = new_pipe_map2;
                              waiting4  = AS.all;
                              propa = let enqueue msg = Pqueue.push(Say msg) in
                                      List.fold enqueue msg_list state.propa }
            in
            master newstate2
                     
         | _ -> kill_pipes new_pipe_map2 new_to_pl2 >>| fun () -> ans1
         end

           
      | Say(WB(_,msg) as thmsg), state ->
         match msg with
           
         | Sat newtset -> 
            Dump.print ["concur",3] (fun p -> p "%a" WB.pp thmsg);
            (* A theory found a counter-model newtset. If it is the
                same as tset, then it means the theory has stamped the
                model for which we were collecting stamps. If not, now
                all other theories need to stamp newtset. *)

            let WB(rest, Sat _) as current =
              match current with
              | Some(WB(_, Sat consset) as c) when WB.DS.Assign.equal newtset consset
                -> WB.sat thmsg c
              | Some(WB(_, Sat consset) as c) when WB.DS.Assign.subset newtset consset
                -> c
              | _ -> WB.sat_init newtset
            in
            if HandlersMap.is_empty rest
            then
              (* rest being empty means that all theories have
                   stamped the assignment consset as being consistent
                   with them, so we can finish, closing all pipes *)
              kill_pipes state.pipe_map state.to_plugin >>| fun () ->
              Case2 current
            else
              (* some theories still haven't stamped that assignment
                 as being consistent with them,
                 so we read what the theories have to tell us *)
              master ~current state

         | Propa(tset,Unsat) -> 
            Dump.print ["concur",1] (fun p -> p "Conflict %a" WB.pp thmsg);
            (* A theory found a proof. We stop and close all pipes. *)
            let finalise conflict uip second_term =
              send state.pipe_map (KillYourself(conflict,uip,second_term))
            in
            T.analyse state.trail thmsg finalise >>= fun ans ->
            kill_pipes state.pipe_map state.to_plugin >>| fun () ->
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
            send state.pipe_map (MsgStraight(newa,T.chrono state.trail)) >>= fun () ->
            master state

    in
    let treat sassign (trail,tasks) =
      let trail = T.add ~nature:T.Input sassign trail in
      trail, ((send pipe_map (MsgStraight(sassign,T.chrono trail)))::tasks)
    in
    let trail,tasks = DS.Assign.fold treat assign0 (T.init,[]) in
    let state = {
        from_workers = from_workers;
        to_plugin    = to_pl0;
        pipe_map     = pipe_map;
        waiting4     = AS.all;
        propa        = Pqueue.empty();
        decisions    = Pqueue.empty();
        trail        = trail
      }
    in
    Deferred.all_unit tasks >>= fun () ->
    master ~current:(WB.sat_init assign0) state >>| function
    | Case1(Case1 conflict) -> Case1 conflict
    | Case1(Case2 _) -> failwith "Should not come back to level -1 with a propagation"
    | Case2 msg -> Case2 msg
                         
end
