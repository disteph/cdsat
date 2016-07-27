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

  module W  = Worker.Make(WB)
  module WM = WorkersMap.Make(WB)
  module Mm = Memo.Make(WB)

  let clause_reader,clause_writer = Pipe.create ()
  let clause_listener = Mm.make_listener clause_reader
  let clause_listener_kill() = return(Pipe.close clause_writer)
    
  let print_pipes =
    let aux (Handlers.Handler hdl) to_worker =
      Dump.print ["concur",1] (fun p -> p "%a: %i messages queued" Sig.print_in_fmt hdl (Pipe.length to_worker))
    in
    HandlersMap.iter aux

  module Agents = struct
    type t = Handlers.t option
    let compare a b =
      match a,b with
      | Some hdl1, Some hdl2 -> Handlers.compare hdl1 hdl2
      | Some _, None -> -1
      | None, Some _ -> 1
      | None, None -> 0
  end
                     
  module AS = struct
    include Set.Make(Agents)
    let all = HandlersMap.fold (fun hdl () sofar -> add(Some hdl) sofar) theories (singleton None)
  end
                      
  type state = {
      from_workers : msg2pl Pipe.Reader.t;
      to_plugin  : msg2pl Pipe.Writer.t;
      pipe_map   : WM.t;
      thAnd_list : say answer list;
      thOr_list  : say answer list;
      waiting4   : AS.t
    }

  let kill_pipes state =
    WM.broadcast (fun w -> return(Pipe.close w)) state.pipe_map
    >>| fun () -> Pipe.close state.to_plugin

  (* This is a branching function telling all slave workers:

         "Please, clone yourself; here are the new pipes to be used
         for your clone to communicate with me; add the literals newa
         to your original self, add the literals newb to your clone."

         Once all slave workers have done so, we apply continuation
         cont to treat the first branch (with newa). When that
         finishes with answer ans, we output ans and a thunk to
         trigger the exploration of the second branch (with newb).
   *)

  let branch state cont newa newb =
    let aux to_worker new_from_pl new_to_pl =
      Lib.write to_worker (MsgBranch(newa,newb,new_from_pl,new_to_pl))
    in
    let new_from_workers, new_to_pl, tasks, new_pipe_map =
      WM.clone aux state.pipe_map
    in
    Deferred.all_unit tasks
    >>= fun () -> 
    Dump.print ["concur",1] (fun p -> p "%s" "Everybody cloned themselves; now starting first branch");
    cont { state with waiting4 = AS.all }
    >>| fun ans ->
    let newstate = { state with
                     from_workers = new_from_workers;
                     to_plugin = new_to_pl;
                     pipe_map  = new_pipe_map;
                     waiting4  = AS.all }
    in
    (ans,
     (fun () -> (Dump.print ["concur",1] (fun p -> p "%s" "Now starting second branch");
                 cont newstate)),
     (fun () -> kill_pipes newstate)
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
         (* >>| fun () -> *)
         return(A msg3)

    | msg2 -> return msg2
    
                    

  let rec select_msg state = match state.thAnd_list, state.thOr_list with

    | thmsg::l, _  when AS.is_empty state.waiting4 ->
       return(thmsg, { state with thAnd_list = l } )

    | _, thmsg::l  when AS.is_empty state.waiting4 ->
       return(thmsg, { state with thOr_list = l } )

    | _, _ ->
       Pipe.read state.from_workers
       >>= (function
            | `Eof -> failwith "Eof"
            | `Ok(Msg(agent,msg)) 
              -> 
               let state = { state with
                             waiting4 =
                               if AS.mem agent state.waiting4
                               then AS.remove agent state.waiting4
                               else state.waiting4
                           }
               in
               match msg with
               | Ack -> select_msg state
               | Say(WB(_,Propa(_,Both _))) -> 
                  let state = { state with thAnd_list = msg::state.thAnd_list }
                  in select_msg state
               | Say(WB(_,Propa(_,Either _))) -> 
                  let state = { state with thOr_list = msg::state.thOr_list }
                  in select_msg state
               | Say _ -> return (msg, state))

  (* Main loop of the master thread *)

  let rec main_worker state (WB(rest, Sat consset) as current) =

    (* So far for all we know, the problem is not provable *)

    Dump.print ["concur",2] (fun p-> p "\nMain_worker enters new loop");
    (* Dump.print ["concur",2] *)
    (*   (fun p-> p "Main_worker enters new loop with theories %a having to check the set\n%a" *)
    (*              HandlersMap.print_in_fmt rest *)
    (*              WB.DS.TSet.print_in_fmt consset *)
    (*   ); *)
    if HandlersMap.is_empty rest
    (* rest being empty means that all theories have
                stamped the set of literals consset as being consistent
                with them, so we can finish, closing all pipes *)
    then kill_pipes state >>| fun () -> F current

    (* some theories still haven't stamped that set of
                literals consset as being consistent with them, so we
                read what the theories have to tell us *)
    else select_msg state
         >>= fun (Say(WB(_,msg) as thmsg), state) ->
         Dump.print ["concur",1] (fun p -> p "%a" WB.print_in_fmt thmsg);
         (match msg with
            
          | Sat newtset -> 
             (* A theory found a counter-model newtset. If it
                       is the same as tset, then it means the theory
                       has stamped the model for which we were
                       collecting stamps. If not, now all other
                       theories need to stamp newtset. *)

             let newcurrent =
               if WB.DS.TSet.equal newtset consset
               then current
               else WB.sat_init newtset
             in
             main_worker state (WB.sat thmsg newcurrent)

          | Propa(_,Unsat) -> 
             (* A theory found a proof. We stop and close all pipes. *)

             (kill_pipes state >>| fun () -> A thmsg)

          | Propa(old,Straight newa) ->
             (* A theory deduced literals newa from literals
               old. We broadcast them to all theories *)
             let treat_worker to_worker =
               Lib.write to_worker (MsgStraight newa)
             in
             WM.broadcast treat_worker state.pipe_map
             >>= fun () ->
             main_worker { state with waiting4 = AS.all } current
             >>= resolve thmsg

          | Propa(old,Both(newa,newb)) ->
             (* A theory is asking to branch conjonctively *)
             branch state (fun newstate -> main_worker newstate current)
               newa newb
             >>= fun (ans1, def_ans2, kill2) -> 
             begin match ans1 with
             | A(WB(_,Propa(tset,Unsat)) as ans1)
                  when not(DS.TSet.is_empty(DS.TSet.inter newa tset))
               -> Lib.write clause_writer ans1
                  >>= fun () ->
                  def_ans2()
                  >>= resolve (WB.both2straight thmsg ans1)
             | _ -> kill2() >>| fun ()-> ans1
             end

          | Propa(old,Either(newa,newb)) ->
             (* A theory is asking to branch disjonctively *)
             branch state (fun newstate -> main_worker newstate current)
               newa newb
             >>= fun (ans1, def_ans2, kill2) ->
             begin match ans1 with
             | A _ -> kill2() >>| fun ()-> failwith "TODO"
             | F _ -> def_ans2() >>| failwith "TODO"
             end
             : (unsat WB.t, sat WB.t) sum Deferred.t)
end
