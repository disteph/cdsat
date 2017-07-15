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

open General.Sums
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
      from_workers : msg2pl Pipe.Reader.t;
      to_plugin  : msg2pl Pipe.Writer.t;
      pipe_map   : WM.t;
      try_list   : say answer list;
      waiting4   : AS.t;
      level      : int;
      chrono     : int;
      trail      : T.t
    }

  let kill_pipes pipe_map to_plugin =
    WM.broadcast (fun w -> return(Pipe.close w)) pipe_map
    >>| fun () -> Pipe.close to_plugin

  let send pipe_map msg = 
    let treat_worker to_worker =
      Lib.write to_worker msg
    in
    WM.broadcast treat_worker pipe_map

  let trail_ext level chrono nature sassign trail =
    T.add (DS.SAssign.build sassign) (fun _ -> (level,chrono,nature)) trail
      
  (* Select message function:
     reads input channel and selects a message to process;
     buffers branching requests and makes sure every agent has
     finished talking before processing one of the buffered branching requests
   *)
      
  let rec select_msg state : (say answer * state) Deferred.t =
    Dump.print ["concur",2]
      (fun p-> p "Want to hear from %a" AS.pp state.waiting4);
    match state.try_list with

    | thmsg::l  when AS.is_empty state.waiting4 ->
       return(thmsg, { state with try_list = l } )

    | _ ->
       Pipe.read state.from_workers
       >>= (function
            | `Eof -> failwith "Eof"
            | `Ok(Msg(agent,msg,chrono)) ->
               let state =
                 (* match msg with *)
                 (* | Say(WB(_,Propa(_,Straight _))) -> state *)
                 (* | _ ->  *)
                    if (chrono = state.chrono)&&(AS.mem agent state.waiting4)
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
                  select_msg { state with try_list = msg::state.try_list }
               | Say(WB _) ->
                  Dump.print ["concur",2] (fun p->
                      p "Hearing from %a:" Agents.pp agent);
                  return (msg, state) )

  (* Main loop of the master thread *)

  let master from_workers to_pl0 pipe_map assign0 =

    let rec master (WB(rest, Sat consset) as current) state =

      Dump.print ["concur",2] (fun p-> p "\nmaster enters new loop");
      if HandlersMap.is_empty rest
      (* rest being empty means that all theories have
         stamped the assignment consset as being consistent
         with them, so we can finish, closing all pipes *)
      then kill_pipes state.pipe_map state.to_plugin >>| fun () ->
           Case2 current

      (* some theories still haven't stamped that assignment as being consistent with them,
         so we read what the theories have to tell us *)
      else
        select_msg state
        >>= function

        | Try sassign, state ->
           Dump.print ["concur",1] (fun p -> p "About to try %a" WB.pp_sassign sassign);

           (* This is a branching point where we tell all slave workers:
              "Please, clone yourself; here are the new pipes to be used
              for your clone to communicate with me." *)

           let aux to_worker new_from_pl1 new_to_pl1 new_from_pl2 new_to_pl2 =
             Lib.write to_worker (MsgBranch(new_from_pl1,new_to_pl1,new_from_pl2,new_to_pl2))
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
               p "Everybody cloned themselves; now starting first branch\nat level %i; chrono %i; Trying %a"
                 (state.level+1) (state.chrono+1) WB.pp_sassign sassign);

           send new_pipe_map1 (MsgStraight(sassign,state.chrono+1))
           >>= fun () ->

           let newstate1 = { state with
                             from_workers = new_from_workers1;
                             to_plugin = new_to_pl1;
                             pipe_map  = new_pipe_map1;
                             waiting4  = AS.all;
                             level     = state.level+1;
                             chrono    = state.chrono+1;
                             trail = trail_ext
                                       (state.level+1)
                                       (state.chrono+1)
                                       T.Tried
                                       sassign
                                       state.trail }
           in
           master current newstate1 >>= fun ans1 ->
           let def_ans2 msg2 =
             Dump.print ["concur",1] (fun p -> p "%s" "Now starting second branch");
             let WB(_,Propa(_,Straight new2)) = msg2 in
             let new2 = Top.Values.boolassign new2 in
             let newstate2 = { state with
                               from_workers = new_from_workers2;
                               to_plugin = new_to_pl2;
                               pipe_map  = new_pipe_map2;
                               waiting4  = AS.all;
                               chrono    = state.chrono+1;
                               trail     = trail_ext
                                             state.level
                                             (state.chrono+1)
                                             (T.Propagated msg2)
                                             new2
                                             state.trail }
             in
             send new_pipe_map2 (MsgStraight(new2,newstate2.chrono)) >>= fun () ->
             master current newstate2
           in
           let kill2 () = kill_pipes new_pipe_map2 new_to_pl2
           in
           kill_pipes state.pipe_map state.to_plugin >>= fun () ->
           begin match ans1 with
           | Case1(Case2(level,msg)) when level=state.level
             ->
              Dump.print ["concur",0] (fun p -> 
                  p "Backtrack level: %i, Propagation:\n%a"
                    (state.level+1)
                    WB.pp msg);
              def_ans2 msg
                       
           | _ -> kill2() >>| fun () -> ans1
           end



             
        | Say(WB(_,msg) as thmsg), state ->
           (match msg with
              
            | Sat newtset -> 
               Dump.print ["concur",3] (fun p -> p "%a" WB.pp thmsg);
               (* A theory found a counter-model newtset. If it is the
                same as tset, then it means the theory has stamped the
                model for which we were collecting stamps. If not, now
                all other theories need to stamp newtset. *)

               let newcurrent =
                 if WB.DS.Assign.equal newtset consset
                 then WB.sat thmsg current
                 else
                   if WB.DS.Assign.subset newtset consset
                   then current
                   else WB.sat_init newtset
               in
               master newcurrent state

            | Propa(tset,Unsat) -> 
               Dump.print ["concur",1] (fun p -> p "Conflict %a" WB.pp thmsg);
               (* A theory found a proof. We stop and close all pipes. *)
               if not(T.subset_poly (fun () _ -> true) tset state.trail)
               then ( Dump.print ["concur",0] (fun p ->
                          p "Pb: these terms are not in the trail: %a"
                            DS.Assign.pp
                            (T.merge_poly { T.sameleaf  = (fun _ _ _ -> DS.Assign.empty);
                                            T.emptyfull = (fun _ -> DS.Assign.empty);
                                            T.fullempty = (fun a -> a);
                                            T.combine   = DS.Assign.union }
                               tset state.trail)
                        );
                      failwith "Master213" );
               let finalise conflict uip second_term =
                 send state.pipe_map (KillYourself(conflict,uip,second_term))
               in
               T.analyse state.trail thmsg finalise >>= fun ans ->
               kill_pipes state.pipe_map state.to_plugin >>| fun () ->
               Case1 ans

            | Propa(old,Straight newa) ->
               let newa = Top.Values.boolassign newa in
               let chrono = state.chrono+1 in
               Dump.print ["concur",1] (fun p ->
                   p "Level %i; chrono %i; %a"
                     state.level chrono WB.pp thmsg);
               (* A theory deduced a boolean assignment newa from assignment
               old. We broadcast it to all theories *)
               send state.pipe_map (MsgStraight(newa,chrono))
               >>= fun () ->
               master current
                 { state with
                   waiting4 = AS.all;
                   chrono = chrono;
                   trail = trail_ext
                             state.level
                             chrono
                             (T.Propagated thmsg)
                             newa
                             state.trail }


               : ((unsat WB.t, int * straight WB.t) sum,
                  sat WB.t) sum Deferred.t)

    in
    let treat sassign (trail,tasks,chrono) =
      let chrono = chrono+1 in
      trail_ext 0 chrono T.Original sassign trail,
      ((send pipe_map (MsgStraight(sassign,chrono)))::tasks),
      chrono
    in
    let trail,tasks,chrono = DS.Assign.fold treat assign0 (T.empty,[],0) in
    let state = {
        from_workers = from_workers;
        to_plugin    = to_pl0;
        pipe_map     = pipe_map;
        try_list     = [];
        waiting4     = AS.all;
        level        = 0;
        chrono       = chrono;
        trail        = trail
      }
    in
    Deferred.all_unit tasks >>= fun () ->
    master (WB.sat_init assign0) state >>| function
    | Case1(Case1 conflict) -> Case1 conflict
    | Case1(Case2 _) -> failwith "Should not come back to level -1 with a propagation"
    | Case2 msg -> Case2 msg
    
end
