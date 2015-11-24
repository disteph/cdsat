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

      let slaves_number = HandlersMap.cardinal m_init

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

      (* This is a branching function telling all slave workers:

         "Please, clone yourself; here are the new pipes to be used
         for your clone to communicate with me; add the literals newa
         to your original self, add the literals newb to your clone."

         Once all slave workers have done so, we apply continuation
         cont to treat the first branch (with newa). When that
         finishes with answer ans, we output ans and a thunk to
         trigger the exploration of the second branch (with newb).
      *)

      let branch from_workers pipe_map cont newa newb =
        let new_from_workers,new_to_pl = Pipe.create () in
        Pipe.set_size_budget new_to_pl slaves_number;
        let tasks,new_pipe_map =
          let treat_worker hdl to_worker sofar =
            let new_from_pl,new_to_worker = Pipe.create () in
            Pipe.write to_worker (W.MsgBranch(newa,newb,new_from_pl,new_to_pl)),
            HandlersMap.add hdl new_to_worker sofar
          in
          hdlfold treat_worker pipe_map HandlersMap.empty
        in
        Deferred.all_unit tasks
        >>= fun () -> 
        cont from_workers pipe_map
        >>| fun ans ->
        ans, fun () -> cont new_from_workers new_pipe_map

      (* A wrapper for the Whiteboard's straight function; the latter
         is not called in case the extra literals newset added by the
         straight step are not used in the proof. *)

      let straight hdl (ThStraight(newset,_) as msg) = function
        | WB.Provable(hdls,thset) as ans ->
           let inter = WB.DS.TSet.inter thset newset in
           if WB.DS.TSet.is_empty inter then ans
           else WB.straight hdl msg ans
        | WB.NotProvable _ -> failwith "Should apply straight on Provable"

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
        | _ -> failwith "Should apply andBranch on two Provable"

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

      (* Main code for master thread, parameterised by the original
      set of formulae we want to prove inconsistent *)

      let mysolve tset =

        (* We start by creating a pipe to write to the master
        thread. *)

        let from_workers,to_pl = Pipe.create () in
        Pipe.set_size_budget to_pl slaves_number;


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

        (* Main loop of the master thread *)

        let rec main_worker from_workers pipe_map = function

          (* So far for all we know, the problem is not provable *)

          | WB.NotProvable(rest,consset) as current ->
             (* print "Main_worker enters new loop" *)
             (* >>= fun () -> *)
             if HandlersMap.is_empty rest
             then

               (* rest being empty means that all theories have
               stamped the set of literals consset as being consistent
               with them, so we can finish, closing all pipes *)

               broadcast (fun w -> return(Pipe.close w)) pipe_map
                >>| fun () -> Pipe.close to_pl; current
             else

               (* some theories still haven't stamped that set of
               literals consset as being consistent with them, so we
               read what the theories have to tell us *)

               begin
                 Pipe.read from_workers
                 >>= function
                 | `Eof -> failwith "Eof"
                 | `Ok(W.Msg(hdl,msg) (* as thmsg *)) 
                   -> 
                    (* print (Dump.toString (fun p -> p "%a" W.print_in_fmt thmsg)) *)
                    (* >>= fun () -> *)
                    match msg with

                   | ThProvable _ -> 
                      (* A theory found a proof. We stop and close all
                      pipes. *)

                      broadcast (fun w -> return(Pipe.close w)) pipe_map
                      >>| fun () -> Pipe.close to_pl; WB.provable hdl msg

                   | ThNotProvable newtset -> 
                      (* A theory found a counter-model newtset. If it
                      is the same as tset, then it means the theory
                      has stamped the model for which we were
                      collecting stamps. If not, now all other
                      theories need to stamp newtset. *)

                      if WB.DS.TSet.equal newtset consset
                      then main_worker from_workers pipe_map (WB.notprovable hdl msg current)
                      else main_worker from_workers pipe_map (WB.notprovable_init newtset)

                   | ThStraight(newa,old) ->
                      (* A theory deduced literals newa from literals
                      old. We broadcast them to all theories *)

                      let treat_worker to_worker =
                        Pipe.write to_worker (W.MsgStraight newa)
                      in
                      broadcast treat_worker pipe_map
                      >>= fun () ->
                      main_worker from_workers pipe_map current
                      >>| fun ans ->
                      begin match ans with
                      | WB.Provable _ -> straight hdl msg ans
                      | WB.NotProvable _ -> ans
                      end

                   | ThAnd(newa,newb,old) ->
                      (* A theory is asking to branch conjonctively *)

                      branch from_workers pipe_map
                        (fun fw pm -> main_worker fw pm current)
                        newa newb
                      >>= fun (ans1, def_ans2) -> 
                     begin match ans1 with
                     | WB.Provable _ ->
                        begin
                          def_ans2()
                          >>| fun ans2 -> match ans2 with
                          | WB.Provable _ ->
                             andBranch hdl msg ans1 ans2
                          | WB.NotProvable _ -> ans2
                        end
                     | WB.NotProvable _ -> return ans1
                     end

                   | ThOr(newa,newb,old) ->
                      (* A theory is asking to branch disjonctively *)

                      branch from_workers pipe_map
                        (fun fw pm -> main_worker fw pm current)
                        newa newb
                      >>= fun (ans1, def_ans2) ->
                     begin match ans1 with
                     | WB.Provable _ -> return (orBranch hdl msg true ans1)
                     | WB.NotProvable _ ->
                        def_ans2()
                        >>| fun ans2 -> match ans2 with
                        | WB.Provable _ -> orBranch hdl msg true ans2
                        | WB.NotProvable _ -> failwith "Don't know"
                     end

               end
          | _ -> failwith "Concur.ml: current should be of the form WB.NotProvable(_,_), but found WB.Provable(_,_)"
        in
        (* Now we wait until all slaves have finished and master has
        finished with answer a, and we return a. *)

        Deferred.both (Deferred.all_unit workers_list) (main_worker from_workers pipe_map (WB.notprovable_init tset))
        >>| fun ((),a) -> 
          a

      (* Finally, we launch the scheduler on mysolve tset, waiting for
      all tasks to be done before returning. *)

      let solve tset = Thread_safe.block_on_async_exn (fun () -> mysolve tset)

    end

  end)
