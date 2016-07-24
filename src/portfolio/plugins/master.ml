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
                include WhiteBoard
                val theories: unit HandlersMap.t
              end) = struct

  open WB
         
  (* We load the code of the slave workers, generated from the
      Whiteboard *)

  module W = Worker.Make(WB)

  (* let slaves_number = HandlersMap.cardinal theories *)

  (* This is a useful variant of a fold function for handlers
      maps, which accumulates on the side a list of tasks (unit
      Deferred.t list) *)

  let hdlfold f map init =
    HandlersMap.fold
      (fun hdl a (list,sofar) ->
        let def,newsofar = f hdl a sofar in
        (def::list), newsofar
      )
      map
      ([],init)

  (* Particular case of the above where we are only interested in
      generating a list of tasks. *)
                             
  let broadcast f map = 
    let aux _ to_worker () = f to_worker,() in
    let list, () = hdlfold aux map () in
    Deferred.all_unit list

  let print_pipes =
    let aux (Handlers.Handler hdl) to_worker =
      Dump.print ["concur",1] (fun p -> p "%a: %i messages queued" Sig.print_in_fmt hdl (Pipe.length to_worker))
    in
    HandlersMap.iter aux

  type state = {
      from_workers : W.msg2pl Pipe.Reader.t;
      to_plugin    : W.msg2pl Pipe.Writer.t;
      pipe_map   : W.msg2th Pipe.Writer.t HandlersMap.t;
      thAnd_list : W.msg2pl list;
      thOr_list  : W.msg2pl list;
      waiting4   : unit HandlersMap.t
    }

  let kill_pipes state =
    broadcast (fun w -> return(Pipe.close w)) state.pipe_map
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
    let new_from_workers,new_to_pl = Pipe.create () in
    (* Pipe.set_size_budget new_to_pl slaves_number; *)
    let tasks,new_pipe_map =
      let treat_worker hdl to_worker sofar =
        let new_from_pl,new_to_worker = Pipe.create () in
        Lib.write to_worker (W.MsgBranch(newa,newb,new_from_pl,new_to_pl)),
        HandlersMap.add hdl new_to_worker sofar
      in
      hdlfold treat_worker state.pipe_map HandlersMap.empty
    in
    Deferred.all_unit tasks
    >>= fun () -> 
    Dump.print ["concur",1] (fun p -> p "%s" "Everybody cloned themselves; now starting first branch");
    cont { state with waiting4 = theories }
    >>| fun ans ->
    let newstate = { state with
                     from_workers = new_from_workers; pipe_map = new_pipe_map;
                     waiting4 = theories }
    in
    (ans,
     (fun () -> (Dump.print ["concur",1] (fun p -> p "%s" "Now starting second branch");
                 cont newstate)),
     (fun () -> kill_pipes newstate)
    )


  (* A wrapper for the Whiteboard's straight function; the latter
         is not called in case the extra literals newset added by the
         straight step are not used in the proof. *)

  let resolve msg1 msg2 =
    let WB(_,Propa(_,Straight newset)) = msg1 in
    let WB(_,Propa(thset,o)) = msg2 in
    let inter = WB.DS.TSet.inter thset newset in
    if WB.DS.TSet.is_empty inter then msg2
    else WB.resolve msg1 msg2
                    
  (* A wrapper for the Whiteboard's and function; the latter is
         not called in case the extra literals new1 or new2 added in
         each branch are not used in the proof coming back from that
         branch. *)

  let andBranch hdl msg ans1 ans2 =
    failwith "TODO"
  (* let ThAnd(new1,new2,_) = WB.reveal msg in *)
  (* match ans1,ans2 with  *)
  (* | WB.Provable(hdls1,thset1), WB.Provable(hdls2,thset2) -> *)
  (*    let inter1 = WB.DS.TSet.inter thset1 new1 in *)
  (*      if WB.DS.TSet.is_empty inter1 *)
  (*      then ans1 *)
  (*      else *)
  (*        let inter2 = WB.DS.TSet.inter thset2 new2 in *)
  (*        if WB.DS.TSet.is_empty inter2 *)
  (*        then ans2 *)
  (*        else WB.andBranch hdl msg ans1 ans2 *)
  (* | _,WB.NotProvable(_,_) -> ans2 *)
  (* | WB.NotProvable(_,_),_ -> ans1 *)

  (* A wrapper for the Whiteboard's or function; the latter is not
         called in case the extra literals new1 or new2 added in the
         branch that returns a proof are not used in that proof. *)

  let orBranch hdl msg side ans =
    failwith "TODO"
  (* let ThOr(new1,new2,_) = WB.reveal msg in *)
  (* match ans with  *)
  (* | WB.Provable(hdls,thset) as ans -> *)
  (*    let newset = if side then new1 else new2 in *)
  (*    let inter = WB.DS.TSet.inter thset newset in *)
  (*    if WB.DS.TSet.is_empty inter *)
  (*    then ans *)
  (*    else WB.orBranch hdl msg side ans *)
  (* | WB.NotProvable _ -> failwith "Should apply orBranch on Provable" *)

  let rec select_msg state = match state.thAnd_list, state.thOr_list with

    | thmsg::l, _  when HandlersMap.is_empty state.waiting4 ->
       return(thmsg, { state with thAnd_list = l } )

    | _, thmsg::l  when HandlersMap.is_empty state.waiting4 ->
       return(thmsg, { state with thOr_list = l } )

    | _, _ ->
       Pipe.read state.from_workers
       >>= (function
            | `Eof -> failwith "Eof"
            | `Ok(W.Msg(WB(hdls,msg)) as thmsg) 
              -> 
               let state = { state with
                             waiting4 =
                               match msg with
                               | Propa _ -> HandlersMap.diff state.waiting4 hdls
                               | Sat _ -> HandlersMap.inter state.waiting4 hdls 
                           }
               in
               match msg with
               | Propa(_,Both _) -> 
                  let newstate = { state with thAnd_list = thmsg::state.thAnd_list }
                  in select_msg newstate
               | Propa(_,Either _) -> 
                  let newstate = { state with thOr_list = thmsg::state.thOr_list }
                  in select_msg newstate
               | _ -> return (thmsg, state))

  (* Main loop of the master thread *)

  let rec main_worker state (WB(rest, Sat consset) as current) =

    (* So far for all we know, the problem is not provable *)

    Dump.print ["concur",2]
      (fun p-> p "Main_worker enters new loop with theories %a having to check the set\n%a"
                 HandlersMap.print_in_fmt rest
                 WB.DS.TSet.print_in_fmt consset
      );
    if HandlersMap.is_empty rest
    (* rest being empty means that all theories have
                stamped the set of literals consset as being consistent
                with them, so we can finish, closing all pipes *)
    then kill_pipes state >>| fun () -> F current

    (* some theories still haven't stamped that set of
                literals consset as being consistent with them, so we
                read what the theories have to tell us *)
    else select_msg state
         >>= fun (W.Msg(WB(hdls,msg) as thmsg), state) ->
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
               Lib.write to_worker (W.MsgStraight newa)
             in
             broadcast treat_worker state.pipe_map
             >>= fun () ->
             main_worker { state with waiting4 = theories } current
             >>| (function
                  | A ans -> A(resolve thmsg ans)
                  | ans -> ans)

          | Propa(old,Both(newa,newb)) ->
             (* A theory is asking to branch conjonctively *)
             branch state (fun newstate -> main_worker newstate current)
               newa newb
             >>= fun (ans1, def_ans2, kill2) -> 
             begin match ans1 with
             | A(WB(_,Propa(tset,Unsat)) as ans1)
                  when not(DS.TSet.is_empty(DS.TSet.inter newa tset))
               -> def_ans2()
                  >>| (function
                       | A ans ->
                          let pr = WB.both2straight thmsg ans1 in
                          A(resolve pr ans)
                       | ans -> ans)
             | _ -> kill2() >>| fun ()-> ans1
             end

          | Propa(old,Either(newa,newb)) ->
             (* A theory is asking to branch disjonctively *)
             branch state (fun newstate -> main_worker newstate current)
               newa newb
             >>= fun (ans1, def_ans2, kill2) ->
             begin match ans1 with
             | A _ -> kill2() >>| fun ()-> orBranch hdls msg true ans1
             | F _ -> def_ans2() >>| orBranch hdls msg false
             end
             : (unsat WB.t, sat WB.t) sum Deferred.t)
end
