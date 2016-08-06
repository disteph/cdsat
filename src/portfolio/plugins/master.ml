(*********************************************************************)
(* Main plugin, implementing the combination of decision procedures
with concurrency, as provided by Jane Street's Async library.

   This is a master-slaves architecture.

   Each slave thread runs the code written in worker.ml, controlling
   the (purely sequential) execution of a decision procedure, and
   exchanging messages with the master thread, whose code is below.  *)
(*********************************************************************)

open Async.Std

open Kernel
open Top.Messages
open Theories_register
open Combo

open General.Sums
open Lib

module Make(WB: sig
                include WhiteBoardExt.Type
                val theories: unit HandlersMap.t
              end) = struct

  open WB
         
  (* We load the code of the slave workers, generated from the
      Whiteboard *)

  module Mm = Memo.Make(WB)
  module W  = Worker.Make(WB)
  module WM = WorkersMap.Make(WB)
  module T  = Trail.Make(WB)
                        
  let clause_reader,clause_writer = Pipe.create ()
  let clause_listener = Mm.make_listener clause_reader
  let clause_listener_kill() = return(Pipe.close clause_writer)

  module Agents = struct
    type t = Handlers.t option

    let compare a b =
      match a,b with
      | Some hdl1, Some hdl2 -> Handlers.compare hdl1 hdl2
      | Some _, None -> -1
      | None, Some _ -> 1
      | None, None -> 0

    let print_in_fmt fmt = function
      | None -> Format.fprintf fmt "Memo module"
      | Some hdl -> Format.fprintf fmt "%a module" Handlers.print_in_fmt hdl
                                   
  end
                    
  module AS = struct
    include Set.Make(Agents)
    let all = HandlersMap.fold (fun hdl () sofar -> add(Some hdl) sofar) theories (singleton None)

    let print_in_fmt fmt hdls =
      let _ =
        fold
          (fun a b ->
            Format.fprintf fmt "%s%a" (if b then ", " else "") Agents.print_in_fmt a;
            true)
          hdls false in
      ()

  end

  type state = {
      from_workers : msg2pl Pipe.Reader.t;
      to_plugin  : msg2pl Pipe.Writer.t;
      pipe_map   : WM.t;
      thAnd_list : say answer list;
      thOr_list  : say answer list;
      waiting4   : AS.t;
      level      : int;
      chrono     : int;
      trail      : T.t
    }

  let kill_pipes pipe_map to_plugin =
    WM.broadcast (fun w -> return(Pipe.close w)) pipe_map
    >>| fun () -> Pipe.close to_plugin

  let send pipe_map tset = 
    let treat_worker to_worker =
      Lib.write to_worker (MsgStraight tset)
    in
    WM.broadcast treat_worker pipe_map

  let action level chrono msg = {
      T.sameleaf = (fun k () v -> T.singleton k v);
      T.emptyfull = (fun trail -> trail);
      T.fullempty = T.map (fun _ () -> level,chrono,msg);
    }

  (* This is a branching function telling all slave workers:

         "Please, clone yourself; here are the new pipes to be used
         for your clone to communicate with me; add the literals newa
         to your original self, add the literals newb to your clone."

         Once all slave workers have done so, we apply continuation
         cont to treat the first branch (with newa). When that
         finishes with answer ans, we output ans and a thunk to
         trigger the exploration of the second branch (with newb).
   *)

  let branch state cont msg new1 =
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
    Dump.print ["concur",1] (fun p -> p "%s" "Everybody cloned themselves; now starting first branch");
    let newstate1 = { state with
                      from_workers = new_from_workers1;
                      to_plugin = new_to_pl1;
                      pipe_map  = new_pipe_map1;
                      waiting4  = AS.all;
                      level     = state.level+1;
                      chrono    = state.chrono+1;
                      trail = T.merge_poly
                                (action (state.level+1) (state.chrono+1) (T.Decided msg))
                                new1
                                state.trail }
    in
    send new_pipe_map1 new1 >>= fun () ->
    cont newstate1 >>| fun ans ->
    (ans,
     (fun msg1 msg2 ->
       let WB(_,Propa(_,Straight new1)) = msg1 in
       (* send new_pipe_map2 new1 >>= fun () -> *)
       (* let trail = T.merge_poly *)
       (*               (action state.level (state.chrono+1) (T.Propagated msg1)) *)
       (*               new1 *)
       (*               state.trail *)
       (* in *)
       let WB(_,Propa(_,Straight new2)) = msg2 in
       send new_pipe_map2 new2 >>= fun () ->
       let trail = T.merge_poly
                     (action state.level (state.chrono+2) (T.Propagated msg2))
                     new2
                     state.trail
       in
       Dump.print ["concur",1] (fun p -> p "%s" "Now starting second branch");
       let newstate2 = { state with
                         from_workers = new_from_workers2;
                         to_plugin = new_to_pl2;
                         pipe_map  = new_pipe_map2;
                         waiting4  = AS.all;
                         chrono    = state.chrono+1;
                         trail     = trail }
       in
       cont newstate2),
     (fun () -> kill_pipes new_pipe_map2 new_to_pl2)
    )

      
  (* Select message function:
     reads input channel and selects a message to process;
     buffers branching requests and makes sure every agent has
     finished talking before processing one of the buffered branching requests
   *)
      
  let rec select_msg state =
    Dump.print ["concur",2]
      (fun p-> p "Wanna hear from %a" AS.print_in_fmt state.waiting4);
    match state.thAnd_list, state.thOr_list with

    | thmsg::l, _  when AS.is_empty state.waiting4 ->
       return(thmsg, { state with thAnd_list = l } )

    | _, thmsg::l  when AS.is_empty state.waiting4 ->
       return(thmsg, { state with thOr_list = l } )

    | _, _ ->
       Pipe.read state.from_workers
       >>= (function
            | `Eof -> failwith "Eof"
            | `Ok(Msg(agent,msg)) ->
               Dump.print ["concur",2] (fun p-> p "Hearing from %a" Agents.print_in_fmt agent);
               let state =
                 match msg with
                 | Say(WB(_,Propa(_,Straight _))) -> state
                 | _ -> { state with
                          waiting4 =
                            if AS.mem agent state.waiting4
                            then AS.remove agent state.waiting4
                            else state.waiting4
                        }
               in
               match msg with
               | Ack ->
                  select_msg state
               | Say(WB(_,Propa(_,Both _))) -> 
                  select_msg { state with thAnd_list = msg::state.thAnd_list }
               | Say(WB(_,Propa(_,Either _))) -> 
                  select_msg { state with thOr_list = msg::state.thOr_list }
               | Say(WB _) ->
                  return (msg, state) )

  (* Main loop of the master thread *)

  let main_worker from_workers to_pl0 pipe_map tset =

    let rec main_worker (WB(rest, Sat consset) as current) state =

      Dump.print ["concur",2] (fun p-> p "\nMain_worker enters new loop");
      if HandlersMap.is_empty rest
      (* rest being empty means that all theories have
                stamped the set of literals consset as being consistent
                with them, so we can finish, closing all pipes *)
      then kill_pipes state.pipe_map state.to_plugin >>| fun () ->
           F current

      (* some theories still haven't stamped that set of
                literals consset as being consistent with them, so we
                read what the theories have to tell us *)
      else
        select_msg state
        >>= fun (Say(WB(_,msg) as thmsg), state) ->
        (match msg with
           
         | Sat newtset -> 
            Dump.print ["concur",1] (fun p -> p "Sat");
            Dump.print ["concur",3] (fun p -> p "%a" WB.print_in_fmt thmsg);
            (* A theory found a counter-model newtset. If it is the
                same as tset, then it means the theory has stamped the
                model for which we were collecting stamps. If not, now
                all other theories need to stamp newtset. *)

            let newcurrent =
              if WB.DS.TSet.equal newtset consset
              then current
              else WB.sat_init newtset
            in
            main_worker (WB.sat thmsg newcurrent) state

         | Propa(tset,Unsat) -> 
            Dump.print ["concur",1] (fun p -> p "Conflict %a" WB.print_in_fmt thmsg);
            (* A theory found a proof. We stop and close all pipes. *)
            (* if not(DS.TSet.subset tset state.trail) *)
            (* then ( Dump.print ["concur",0] (fun p -> p "Pb: %a\n%a" WB.print_in_fmt thmsg DS.TSet.print_in_fmt (DS.TSet.diff tset state.trail)); *)
            (*        failwith "Master213" ); *)
            let conflict,level,term = T.analyse state.trail thmsg in
            Dump.print ["concur",0] (fun p -> p "Current level: %i, Conflict level: %i, UIP: %a, Conflict:\n%a"
                                                state.level level DS.Term.print_in_fmt term WB.print_in_fmt conflict);
            kill_pipes state.pipe_map state.to_plugin >>| fun () ->
            A(conflict,level,term)

         | Propa(old,Straight newa) ->
            Dump.print ["concur",1] (fun p -> p "Level %i; chrono %i; %a" state.level (state.chrono+1) WB.print_in_fmt thmsg);
            (* A theory deduced literals newa from literals
               old. We broadcast them to all theories *)
            send state.pipe_map newa >>= fun () ->
            main_worker current
              { state with
                waiting4 = AS.all;
                chrono = state.chrono+1;
                trail = T.merge_poly
                          (action state.level (state.chrono+1) (T.Propagated thmsg))
                          newa
                          state.trail }

         | Propa(old,Both(newa,_)) ->
            Dump.print ["concur",1] (fun p ->
                p "Level %i; chrono %i; %a"
                  (state.level+1) (state.chrono+1) WB.print_in_fmt thmsg);
            (* A theory is asking to branch conjonctively *)
            branch state (main_worker current) thmsg newa
            >>= fun (ans1, def_ans2, kill2) -> 
            kill_pipes state.pipe_map state.to_plugin >>= fun () ->
            begin match ans1 with
            | A(ans1,level,term) when
                   (* Dump.print ["concur",0] (fun p -> 
                      p "Backtrack level: %i, Conflict level: %i, UIP: %a, Conflict:\n%a" *)
                   (* state.level level DS.Term.print_in_fmt term WB.print_in_fmt ans1); *)
                   level==state.level
              ->
               (if !Flags.memo
                then Lib.write clause_writer ans1
                else return ())
               >>= fun () ->
               def_ans2
                 (WB.both2straight thmsg ans1)
                 (WB.curryfy (DS.TSet.singleton term) ans1)
                        
            | _ -> kill2() >>| fun () -> ans1
            end

         | Propa(old,Either(newa,newb)) ->
            failwith "TODO"

            : (unsat WB.t * int * DS.Term.t, sat WB.t) sum Deferred.t)

    in
    let state = {
        from_workers = from_workers;
        to_plugin    = to_pl0;
        pipe_map     = pipe_map;
        thAnd_list   = [];
        thOr_list    = [];
        waiting4     = AS.all;
        level        = 0;
        chrono       = 0;
        trail        = T.map (fun _ () -> 0,0,T.Original) tset
      }
    in
    send state.pipe_map tset >>= fun () ->
    main_worker (WB.sat_init tset) state >>| function
    | A(conflict,_,_) -> A conflict
    | F msg -> F msg
    
end
