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


      include Master.Make(struct
                  include WB
                  let theories = theories
                end)
      
      open WB

      (* We use the make function to create a map m_init that maps
      every (involved) theory handler to (the initial state of) a
      decision procedure for it. *)

      let m_init = make (module WB.DS) WB.projList

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
            let worker = W.make from_pl to_pl a tset in
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
          waiting4     = theories;
        }
        in

        (* Now we wait until all slaves have finished and master has
           finished with answer a, and we return a. *)
        Deferred.both
          (Deferred.all_unit workers_list)
          (main_worker state (WB.sat_init tset))
        >>| fun ((), a) -> 
        a

      (* Finally, we launch the scheduler on mysolve tset, waiting for
         all tasks to be done before returning. *)

      let solve tset = Thread_safe.block_on_async_exn (fun () -> mysolve tset)

    end

  end)
