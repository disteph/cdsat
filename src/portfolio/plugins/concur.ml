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

open Lib

(* We are parameterised by the theories involved in the problem *)

let make theories : (module Plugin.Type) =
  (module struct

    (* We load the data-structures and the decision procedures, which
    gives us a make function *)

    include (val LoadPluginsTh.make theories)

    (* Now the top-level is going to give us a WhiteBoard *)

    module Strategy(WB: sig
      include WhiteBoard
      val projList: (DS.Term.datatype,agglo) projList
    end) = struct

      (* We load the code of the slave workers, generated from the
      Whiteboard *)

      module W = Worker.Make(WB)

      (* We use the make function to create a map m_init that maps
      every (involved) theory handler to (the initial state of) a
      decision procedure for it. *)

      let m_init = make (module WB.DS) WB.projList

      (* The set of all involved theories *)

      let all_theories = HandlersMap.map (fun _ -> ()) m_init

      (* let slaves_number = HandlersMap.cardinal all_theories *)

      (* This is a useful variant of a fold function for handlers
      maps, which accumulates on the side a list of tasks (unit
      Deferred.t list) *)

      let hdlfold f map init = HandlersMap.fold
        (fun hdl a (list,sofar) ->
          let def,newsofar = f hdl a sofar in
          (def::list), newsofar
        )
        map
        ([],init)

      (* Particular case of the above where we are only interested in
      generating a list of tasks. *)
        
      let broadcast f m = 
        let aux _ to_worker () = f to_worker,() in
        let list, () = hdlfold aux m () in
        Deferred.all_unit list

      let print_pipes =
        let aux (Handlers.Handler hdl) to_worker =
          Dump.msg (Some(fun p -> p "%a: %i messages queued" Sig.print_in_fmt hdl (Pipe.length to_worker))) None None
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
        Dump.msg (Some(fun p -> p "%s" "Everybody cloned themselves; now starting first branch")) None None;
        cont { state with waiting4 = all_theories }
        >>| fun ans ->
        let newstate = { state with
          from_workers = new_from_workers; pipe_map = new_pipe_map;
          waiting4 = all_theories }
        in
        (ans,
         (fun () -> (Dump.msg (Some(fun p -> p "%s" "Now starting second branch")) None None;
                    cont newstate)),
         (fun () -> kill_pipes newstate)
        )


      (* A wrapper for the Whiteboard's straight function; the latter
         is not called in case the extra literals newset added by the
         straight step are not used in the proof. *)

      let straight hdl (ThStraight(newset,_) as msg) ans =
        match ans with
        | WB.Provable(hdls,thset) ->
           let inter = WB.DS.TSet.inter thset newset in
           if WB.DS.TSet.is_empty inter then ans
           else WB.straight hdl msg ans
        | WB.NotProvable _ -> ans

      (* A wrapper for the Whiteboard's and function; the latter is
         not called in case the extra literals new1 or new2 added in
         each branch are not used in the proof coming back from that
         branch. *)

      let andBranch hdl (ThAnd(new1,new2,_) as msg) ans1 ans2 =
        match ans1,ans2 with 
        | WB.Provable(hdls1,thset1), WB.Provable(hdls2,thset2) ->
           let inter1 = WB.DS.TSet.inter thset1 new1 in
             if WB.DS.TSet.is_empty inter1
             then ans1
             else
               let inter2 = WB.DS.TSet.inter thset2 new2 in
               if WB.DS.TSet.is_empty inter2
               then ans2
               else WB.andBranch hdl msg ans1 ans2
        | _,WB.NotProvable(_,_) -> ans2
        | WB.NotProvable(_,_),_ -> ans1

      (* A wrapper for the Whiteboard's or function; the latter is not
         called in case the extra literals new1 or new2 added in the
         branch that returns a proof are not used in that proof. *)

      let orBranch hdl (ThOr(new1,new2,_) as msg) side = function
        | WB.Provable(hdls,thset) as ans ->
           let newset = if side then new1 else new2 in
           let inter = WB.DS.TSet.inter thset newset in
           if WB.DS.TSet.is_empty inter
           then ans
           else WB.orBranch hdl msg side ans
        | WB.NotProvable _ -> failwith "Should apply orBranch on Provable"

      let rec select_msg state = match state.thAnd_list, state.thOr_list with

        | thmsg::l, _  when HandlersMap.is_empty state.waiting4 ->
           return(thmsg, { state with thAnd_list = l } )

        | _, thmsg::l  when HandlersMap.is_empty state.waiting4 ->
           return(thmsg, { state with thOr_list = l } )

        | _, _ -> Pipe.read state.from_workers
                  >>= (function
                  | `Eof -> failwith "Eof"
                  | `Ok(W.Msg(hdl,msg) as thmsg) 
                    -> 
                     let hdl = Handlers.Handler hdl in
                     let state = if HandlersMap.mem hdl state.waiting4
                       then { state with waiting4 = HandlersMap.remove hdl state.waiting4 }
                       else state
                     in
                      match msg with
                      | ThAnd _ -> 
                         let newstate = { state with thAnd_list = thmsg::state.thAnd_list }
                         in select_msg newstate
                      | ThOr _ -> 
                         let newstate = { state with thOr_list = thmsg::state.thOr_list }
                         in select_msg newstate
                      | _ -> return (thmsg, state))

      (* Main loop of the master thread *)

      let rec main_worker state = function

        (* So far for all we know, the problem is not provable *)

        | WB.NotProvable(rest,consset) as current ->

           begin
             Dump.msg None (Some(fun p-> p "Main_worker enters new loop with %i theories having to check the set\n%a"
               (HandlersMap.cardinal rest)
               WB.DS.TSet.print_in_fmt consset
             )) None;

             if HandlersMap.is_empty rest
             (* rest being empty means that all theories have
                stamped the set of literals consset as being consistent
                with them, so we can finish, closing all pipes *)
             then kill_pipes state >>| fun () -> current

             (* some theories still haven't stamped that set of
                literals consset as being consistent with them, so we
                read what the theories have to tell us *)
             else select_msg state
                >>= function (W.Msg(hdl,msg) as thmsg, state) ->
                  Dump.msg (Some(fun p -> p "%a" W.print_in_fmt thmsg)) None None;
                  match msg with

                  | ThProvable _ -> 
                    (* A theory found a proof. We stop and close all
                       pipes. *)

                     kill_pipes state >>| fun () ->  WB.provable hdl msg

                  | ThNotProvable newtset -> 
                    (* A theory found a counter-model newtset. If it
                       is the same as tset, then it means the theory
                       has stamped the model for which we were
                       collecting stamps. If not, now all other
                       theories need to stamp newtset. *)

                     let newcurrent =
                       if WB.DS.TSet.equal newtset consset
                       then current
                       else WB.notprovable_init newtset
                     in
                     main_worker state (WB.notprovable hdl msg newcurrent)

                  | ThStraight(newa,old) ->
                    (* A theory deduced literals newa from literals
                       old. We broadcast them to all theories *)
                    let treat_worker to_worker =
                      Lib.write to_worker (W.MsgStraight newa)
                    in
                    broadcast treat_worker state.pipe_map
                    >>= fun () ->
                    main_worker { state with waiting4 = all_theories } current
                    >>| straight hdl msg

                  | ThAnd(newa,newb,old) ->
                    (* A theory is asking to branch conjonctively *)
                     branch state (fun newstate -> main_worker newstate current)
                       newa newb
                     >>= fun (ans1, def_ans2, kill2) -> 
                    begin match ans1 with
                    | WB.Provable _ -> def_ans2() >>| andBranch hdl msg ans1
                    | WB.NotProvable _ -> kill2() >>| fun ()-> ans1
                    end

                  | ThOr(newa,newb,old) ->
                    (* A theory is asking to branch disjonctively *)
                     branch state (fun newstate -> main_worker newstate current)
                       newa newb
                     >>= fun (ans1, def_ans2, kill2) ->
                    begin match ans1 with
                    | WB.Provable _ -> kill2() >>| fun ()-> orBranch hdl msg true ans1
                    | WB.NotProvable _ -> def_ans2() >>| orBranch hdl msg false
                    end
           end
        | _ -> failwith "Concur.ml: current should be of the form WB.NotProvable(_,_), but found WB.Provable(_,_)"

      (* Main code for master thread, parameterised by the original
         set of formulae we want to prove inconsistent *)


      let mysolve tset =

        (* We start by creating a pipe to write to the master
           thread. *)

        let from_workers,to_pl = Pipe.create () in
        (* Pipe.set_size_budget to_pl slaves_number; *)

        (* Then for each decision procedure in m_init, we create a
           slave worker controlling it, with a dedicated pipe to write
           to that slave. From this we collect the list of workers,
           and a map mapping each theory handler to the pipe writer
           used to communicate to its corresponding slave. *)

        let workers_list,pipe_map =
          let aux hdl a sofar =
            let from_pl,to_worker = Pipe.create () in
            let worker = W.make from_pl to_pl a (Some tset) in
            worker, HandlersMap.add hdl to_worker sofar
          in
          hdlfold aux m_init HandlersMap.empty

        in

        let state = {
          from_workers = from_workers;
          to_plugin    = to_pl;
          pipe_map     = pipe_map;
          thAnd_list   = [];
          thOr_list    = [];
          waiting4     = all_theories;
        }
        in

        (* Now we wait until all slaves have finished and master has
           finished with answer a, and we return a. *)
        Deferred.both
          (Deferred.all_unit workers_list)
          (main_worker state (WB.notprovable_init tset))
        >>| fun ((), a) -> 
        a

      (* Finally, we launch the scheduler on mysolve tset, waiting for
         all tasks to be done before returning. *)

      let solve tset = Thread_safe.block_on_async_exn (fun () -> mysolve tset)

    end

  end)
