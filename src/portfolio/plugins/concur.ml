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

      module WBE = WhiteBoardExt.Make(WB)
      module M = Master.Make(struct
                     include WB
                     include WBE
                     let theories = theories
                   end)
      open M
             
      (* We use the make function to create a map m_init that maps
      every (involved) theory handler to (the initial state of) a
      decision procedure for it. *)

      let m_init = make (module WB.DS) WB.projList

      (* Main code for master thread, parameterised by the original
         set of formulae we want to prove inconsistent *)

      let mysolve tset =

        (* For each decision procedure in m_init, we create a
           slave worker controlling it, with a dedicated pipe to write
           to that slave. From this we collect the list of workers,
           and a map mapping each theory handler to the pipe writer
           used to communicate to its corresponding slave. *)

        let aux = function
          | Some a -> W.make a tset
          | None -> Mm.make tset
        in
        let from_workers, to_pl, workers_list, pipe_map =
          WM.make aux m_init
        in

        let state = {
          from_workers = from_workers;
          to_plugin    = to_pl;
          pipe_map     = pipe_map;
          thAnd_list   = [];
          thOr_list    = [];
          waiting4     = AS.all;
        }
        in

        (* Now we wait until all slaves have finished and master has
           finished with answer a, and we return a. *)
        main_worker state (WB.sat_init tset)
        >>= fun a ->
        Dump.print ["concur",1] (fun p-> p "Finished computation");
        Deferred.all_unit (clause_listener_kill()::clause_listener::workers_list)
        >>| fun () -> a

      (* Finally, we launch the scheduler on mysolve tset, waiting for
         all tasks to be done before returning. *)

      let solve tset = Thread_safe.block_on_async_exn (fun () -> mysolve tset)

    end

  end)
