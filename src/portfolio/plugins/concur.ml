open Async.Std

open Kernel
open Top.Messages
open Theories_register
open Combo

let make theories : (module Plugin.Type) =
  (module struct

    include (val LoadPluginsTh.make theories)

    module Strategy(WB: sig
      include WhiteBoard
      val projList: (DS.Term.datatype,agglo) projList
    end) = struct

      module W = Worker.Make(WB)

      let m_init = make (module WB.DS) WB.projList

      let broadcast f m = Deferred.all_unit 
        (HandlersMap.fold (fun _ to_worker distrib_list -> (f to_worker::distrib_list)) m [])

      let mysolve tset =

        let from_workers,to_pl = Pipe.create () in

        let workers_list,pipe_map = HandlersMap.fold
          (fun hdl a (workers_list,pipe_map) ->
            let from_pl,to_worker = Pipe.create () in
            let worker = W.make from_pl to_pl a (Some tset) in
            (worker::workers_list,
             HandlersMap.add hdl to_worker pipe_map)
          )
          m_init
          ([],HandlersMap.empty)
        in

        let rec main_worker = function
          | WB.NotProvable(rest,consset) as current ->
             if HandlersMap.is_empty rest
             then broadcast (fun w -> return(Pipe.close w)) pipe_map
                >>| fun () -> current
             else
               begin
            Pipe.read from_workers
             >>= function
             | `Eof -> failwith "Eof"
             | `Ok(W.Msg(hdl,m) as msg) -> match m with
               | ThProvable _ -> 
                 broadcast (fun w -> return(Pipe.close w)) pipe_map
                 >>| fun() -> WB.provable hdl m
               | ThNotProvable newtset -> 
                 if WB.DS.TSet.equal newtset consset
                 then main_worker (WB.notprovable hdl m current)
                 else main_worker (WB.notprovable_init newtset)
               | _ ->
                 broadcast (fun to_worker -> Pipe.write to_worker msg) pipe_map
                 >>= fun () -> main_worker current
               end
          | _ -> failwith "Concur.ml: current should be of the form WB.NotProvable(_,_), but found WB.Provable(_,_)"
        in

        Deferred.both (Deferred.all_unit workers_list) (main_worker (WB.notprovable_init tset))
        >>| fun ((),a) -> 
        Pipe.close to_pl;
        a

      let solve tset = Thread_safe.block_on_async_exn (fun () -> mysolve tset)

    end

  end)
