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
  include Track.Make(WB)
                             
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
      trail      : Trail.t
    }

  let print_pipes =
    let aux (Handlers.Handler hdl) to_worker =
      Dump.print ["concur",1] (fun p -> p "%a: %i messages queued" Sig.print_in_fmt hdl (Pipe.length to_worker))
    in
    HandlersMap.iter aux

  let kill_pipes state =
    WM.broadcast (fun w -> return(Pipe.close w)) state.pipe_map
    >>| fun () -> Pipe.close state.to_plugin

  let send pipe_map tset = 
    let treat_worker to_worker =
      Lib.write to_worker (MsgStraight tset)
    in
    WM.broadcast treat_worker pipe_map

  let action level chrono msg = {
      Trail.sameleaf = (fun k () v -> Trail.singleton k v);
      Trail.emptyfull = (fun trail -> trail);
      Trail.fullempty = Trail.map (fun _ () -> level,chrono,msg);
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

  let branch state cont new1 new2 msg =
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
    (Lib.dispatch state.from_workers [new_to_pl1; new_to_pl2],
     Deferred.all_unit tasks >>= fun () -> 
     Dump.print ["concur",1] (fun p -> p "%s" "Everybody cloned themselves; now starting first branch");
     let newstate1 = { state with
                       from_workers = new_from_workers1;
                       to_plugin = new_to_pl1;
                       pipe_map  = new_pipe_map1;
                       waiting4  = AS.all;
                       level     = state.level+1;
                       chrono    = state.chrono+1;
                       trail = Trail.merge_poly
                                 (action (state.level+1) (state.chrono+1) msg)
                                 new1
                                 state.trail }
     in
     send new_pipe_map1 new1 >>= fun () ->
     cont newstate1 >>| fun ans ->
     let newstate2 = { state with
                       from_workers = new_from_workers2;
                       to_plugin = new_to_pl2;
                       pipe_map  = new_pipe_map2;
                       waiting4  = AS.all;
                       level     = state.level+1;
                       chrono    = state.chrono+1;
                       trail = Trail.merge_poly
                                 (action (state.level+1) (state.chrono+1) msg)
                                 new2
                                 state.trail }
     in
     (ans,
      (fun () -> (Dump.print ["concur",1] (fun p -> p "%s" "Now starting second branch");
                  send new_pipe_map2 new2 >>= fun () ->
                  cont newstate2)),
      (fun () -> kill_pipes newstate2)
     )
    )

  (* A wrapper for the Whiteboard's straight function; the latter
         is not called in case the extra literals newset added by the
         straight step are not used in the proof. *)

  let resolve msg1 = function
    | A(WB(_,Propa(thset,Unsat)) as msg2) when
           (let WB(_,Propa(_,Straight newset)) = msg1 in
            let inter = WB.DS.TSet.inter thset newset in       
            not (WB.DS.TSet.is_empty inter))

      -> let msg3 = WB.resolve msg1 msg2 in
         (* Lib.write clause_writer msg3 *)
         (* >>= fun () -> *)
         return(A msg3)

    | msg2 -> return msg2
    
    
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
      then kill_pipes state >>| fun () -> F current

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
            Dump.print ["concur",1] (fun p -> p "%a" WB.print_in_fmt thmsg);
            (* A theory found a proof. We stop and close all pipes. *)
            (* if not(DS.TSet.subset tset state.trail) *)
            (* then ( Dump.print ["concur",0] (fun p -> p "Pb: %a\n%a" WB.print_in_fmt thmsg DS.TSet.print_in_fmt (DS.TSet.diff tset state.trail)); *)
            (*        failwith "Master213" ); *)
            (kill_pipes state >>| fun () -> A thmsg)

         | Propa(old,Straight newa) ->
            Dump.print ["concur",1] (fun p -> p "%a" WB.print_in_fmt thmsg);
            (* A theory deduced literals newa from literals
               old. We broadcast them to all theories *)
            send state.pipe_map newa
            >>= fun () ->
            main_worker current
              { state with waiting4 = AS.all;
                           chrono = state.chrono;
                           trail = Trail.merge_poly
                                     (action state.level (state.chrono+1) (Propagated thmsg))
                                     newa
                                     state.trail }
            >>= resolve thmsg

         | Propa(old,Both(newa,newb)) ->
            Dump.print ["concur",1] (fun p -> p "%a" WB.print_in_fmt thmsg);
            (* A theory is asking to branch conjonctively *)
            let dispatcher,def =
              branch state (main_worker current) newa newb (Decided thmsg)
            in
            Deferred.both
              dispatcher
              (def
               >>= fun (ans1, def_ans2, kill2) -> 
               begin match ans1 with
               | A(WB(_,Propa(tset,Unsat)) as ans1)
                    when not(DS.TSet.is_empty(DS.TSet.inter newa tset))
                 -> (if !Flags.memo
                     then
                       begin
                         (* let msg = WB.curryfy tset ans1 in *)
                         (* Dump.print ["concur",0] (fun p-> p "Recording %a" print_in_fmt msg); *)
                         (* Lib.write to_pl0 (Msg(None,Say msg)) *)
                         Lib.write clause_writer ans1
                       end
                     else return())
                    >>= fun () ->
                    def_ans2() >>= fun ans2 ->
                    kill_pipes state >>= fun () ->
                    let newstraight = WB.both2straight thmsg ans1 in
                    (* let WB(_,Propa(tset,Straight _)) = newstraight in *)
                    (* if not(DS.TSet.subset tset state.trail) *)
                    (* then *)
                    (*   (Dump.print ["concur",0] (fun p -> p "Pb: %a\n%a" WB.print_in_fmt newstraight DS.TSet.print_in_fmt (DS.TSet.diff tset state.trail)); *)
                    (*    failwith "Master265"); *)
                    resolve newstraight ans2
               | _ -> kill2() >>= fun ()->
                      kill_pipes state >>| fun () ->
                      ans1
               end
              )
            >>| fun ((),ans) -> ans

         | Propa(old,Either(newa,newb)) ->
            (* A theory is asking to branch disjonctively *)
            let dispatcher,def =
              branch state (main_worker current) newa newb (failwith "TODO")
            in
            def
            >>= fun (ans1, def_ans2, kill2) ->
            begin match ans1 with
            | A _ -> kill2() >>| fun ()-> failwith "TODO"
            | F _ -> def_ans2() >>| failwith "TODO"
            end
            : (unsat WB.t, sat WB.t) sum Deferred.t)

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
        trail        = Trail.map (fun _ () -> 0,0,Original) tset
      }
    in
    send state.pipe_map tset >>= fun () ->
    main_worker (WB.sat_init tset) state
    
end
