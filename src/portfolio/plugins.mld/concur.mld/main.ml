(*********************************************************************)
(* Main plugin, implementing the combination of decision procedures
with concurrency, as provided by Jane Street's Async library.

   This is a master-slaves architecture.

   Each slave thread runs the code written in worker.ml, controlling
   the (purely sequential) execution of a decision procedure, and
   exchanging messages with the master thread, whose code is below.  *)
(*********************************************************************)

open Async

open General.Sums

open Kernel

open Lib

module Make(K:Plugin.Input) = struct
  open K

  module WBE = struct
    include WB
    include WhiteBoardExt.Make(WB)
    let theories_fold f = Theories.Register.HandlersMap.fold (fun hdl _ -> f hdl) pluginsTh
  end

  (* We load the code of the Memoisation module, of the workers' module, and of
  the master module. *)
                 
  module Mm = Memo.Make(WBE)
  module W  = Worker.Make(WBE)
  module M  = Master.Make(WBE)

  open M
         
  (* Main code for master thread, parameterised by the original
         set of formulae we want to prove inconsistent *)

  let mysolve tset =

    (* For each decision procedure in pluginsTh, we create a
           slave worker controlling it, with a dedicated pipe to write
           to that slave. From this we collect the list of workers,
           and a map mapping each theory handler to the pipe writer
           used to communicate to its corresponding slave. *)

    let aux = function
      | Some a -> W.make a
      | None -> Mm.make
    in
    let from_workers, to_pl, workers_list, pipe_map =
      WM.make aux pluginsTh
    in

    (* Now we wait until all slaves have finished and master has
           finished with answer a, and we return a. *)
    main_worker from_workers to_pl pipe_map tset >>= fun a ->
    Dump.print ["concur",1] (fun p-> p "Finished computation");
    Deferred.all_unit workers_list
    >>| fun () -> a

  (* Finally, we launch the scheduler on mysolve tset, waiting for
         all tasks to be done before returning. *)

  let solve_th tset = Thread_safe.block_on_async_exn (fun () -> mysolve tset)

  let solve () = answer(solve_th problem)
                                                     
  let clear () = ()
end