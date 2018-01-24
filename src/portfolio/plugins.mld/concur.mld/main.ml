(*********************************************************************)
(* Main plugin, implementing the combination of decision procedures
with concurrency, as provided by Jane Street's Async library.

   This is a master-slaves architecture.

   Each slave thread runs the code written in worker.ml, controlling
   the (purely sequential) execution of a decision procedure, and
   exchanging messages with the master thread, whose code is below.  *)
(*********************************************************************)

open Async

open General
open Sums

open Kernel
open Theories.Register
open Lib

module Make(K:Plugin.Input) = struct
  open K

  module WBE = struct
    include WB
    include WhiteBoardExt.Make(WB)
  end
                 
  (* We load the code of the Memoisation module, of the workers' module, and of
  the master module. *)
                 
  module Mm = Memo.Make(WBE)
  module EG = EgraphWorker.Make(WBE)(EGraph)
  module W  = Worker.Make(WBE)
  module H  = Hub.Make(WBE)

  let aux (PluginsTh.PluginTh.Signed(tag,_) as smachine) sofar =
    HandlersMap.add (Handlers.Handler tag) (W.make smachine) sofar

  let pluginsTh = List.fold aux pluginsTh HandlersMap.empty

  module M  = Master.Make(struct
                  module WBE = WBE
                  module H = H
                  let theories_fold f seed =
                    HandlersMap.fold (fun hdl _ -> f hdl) pluginsTh (f Handlers.Eq seed)
                end)
  open M
         
  (* Main code for master thread, parameterised by the original
         set of formulae we want to prove inconsistent *)

  let mysolve tset =
    
    (* For each decision procedure in pluginsTh, we create a
           slave worker controlling it, with a dedicated pipe to write
           to that slave. From this we collect the list of workers,
           and a map mapping each theory handler to the pipe writer
           used to communicate to its corresponding slave. *)

    let workers, hub = H.make EG.make Mm.make pluginsTh in

    (* Now we wait until all slaves have finished and master has
           finished with answer a, and we return a. *)
    let%map (),a = Deferred.both workers (master hub tset) in
    Print.print ["concur",1] (fun p-> p "Finished computation");
    a

  (* Finally, we launch the scheduler on mysolve tset, waiting for
         all tasks to be done before returning. *)

  let solve assign = Thread_safe.block_on_async_exn (fun () -> mysolve assign)

  let clear () = K.clear(); Mm.clear()
end
