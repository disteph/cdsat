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

      let hdlfold f map init = HandlersMap.fold
        (fun hdl a (list,sofar) ->
          let def,newsofar = f hdl a sofar in
          (def::list), newsofar
        )
        map
        ([],init)
        
      let broadcast f m = 
        let aux _ to_worker () = f to_worker,() in
        let list, () = hdlfold aux m () in
        Deferred.all_unit list

      let branch from_workers pipe_map cont newa newb =
        let new_from_workers,new_to_pl = Pipe.create () in
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

      let straight hdl (ThStraight(newset,_) as msg) = function
        | WB.Provable(hdls,thset) as ans ->
           let inter = WB.DS.TSet.inter thset newset in
           if WB.DS.TSet.is_empty inter then ans
           else WB.straight hdl msg ans
        | WB.NotProvable _ -> failwith "Should apply straight on Provable"

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

      let orBranch hdl (ThOr(new1,new2,_) as msg) side = function
        | WB.Provable(hdls,thset) as ans ->
           let newset = if side then new1 else new2 in
           let inter = WB.DS.TSet.inter thset newset in
           if WB.DS.TSet.is_empty inter
           then ans
           else WB.orBranch hdl msg side ans
        | WB.NotProvable _ -> failwith "Should apply orBranch on Provable"


      let mysolve tset =

        let from_workers,to_pl = Pipe.create () in

        let workers_list,pipe_map =
          let aux hdl a sofar =
            let from_pl,to_worker = Pipe.create () in
            let worker = W.make from_pl to_pl a (Some tset) in
            worker, HandlersMap.add hdl to_worker sofar
          in
          hdlfold aux m_init HandlersMap.empty

        in

        let rec main_worker from_workers pipe_map = function
          | WB.NotProvable(rest,consset) as current ->
             Dump.msg (Some(fun p -> p "Main_worker enters new loop\n")) None None;
             if HandlersMap.is_empty rest
             then broadcast (fun w -> return(Pipe.close w)) pipe_map
                >>| fun () -> current
             else
               begin
                 Pipe.read from_workers
                 >>= function
                 | `Eof -> failwith "Eof"
                 | `Ok(W.Msg(hdl,msg)) -> match msg with

                   | ThProvable _ -> 
                      broadcast (fun w -> return(Pipe.close w)) pipe_map
                      >>| fun() -> WB.provable hdl msg

                   | ThNotProvable newtset -> 
                      if WB.DS.TSet.equal newtset consset
                      then main_worker from_workers pipe_map (WB.notprovable hdl msg current)
                      else main_worker from_workers pipe_map (WB.notprovable_init newtset)

                   | ThStraight(newa,old) ->
                      let treat_worker to_worker =
                        Pipe.write to_worker (W.MsgStraight newa)
                      in
                      broadcast treat_worker pipe_map
                      >>= fun () ->
                      main_worker from_workers pipe_map current
                      >>| fun ans ->
                      begin match ans with
                      | WB.Provable _ -> straight hdl msg current
                      | WB.NotProvable _ -> ans
                      end

                   | ThAnd(newa,newb,old) ->
                      branch from_workers pipe_map
                        (fun fw pm -> main_worker fw pm current)
                        newa newb
                      >>= fun (ans1, def_ans2) -> 
                     begin match ans1 with
                     | WB.Provable _ ->
                        begin
                          def_ans2()
                          >>| fun ans2 -> match ans2 with
                          | WB.Provable _ -> andBranch hdl msg ans1 ans2
                          | WB.NotProvable _ -> ans2
                        end
                     | WB.NotProvable _ -> return ans1
                     end

                   | ThOr(newa,newb,old) ->
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

        Deferred.both (Deferred.all_unit workers_list) (main_worker from_workers pipe_map (WB.notprovable_init tset))
        >>| fun ((),a) -> 
        Pipe.close to_pl;
        a

      let solve tset = Thread_safe.block_on_async_exn (fun () -> mysolve tset)

    end

  end)
