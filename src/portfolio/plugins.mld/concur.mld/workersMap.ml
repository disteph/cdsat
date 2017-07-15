open Async

open Kernel.Theories.Register

module Make(W: sig
                type msg2th
                type msg2pl
              end) = struct

  type t = W.msg2th Pipe.Writer.t HandlersMap.t
           * W.msg2th Pipe.Writer.t

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
      init

  let fold repeat init transform hdlsmap =
    let worker,stuff = repeat None in
    let aux hdl a sofar =
      let worker,stuff = repeat (Some a) in
      worker, transform hdl stuff sofar
    in
    hdlfold aux hdlsmap ([worker],init stuff)
                        
  let make memo hdlsmap =
    let f = function
      | Some a -> a
      | None -> memo
    in
    let from_workers,to_pl = Pipe.create () in
    let repeat a =
      let from_pl,to_worker = Pipe.create () in
      f a from_pl to_pl, to_worker
    in
    let init to_worker = HandlersMap.empty, to_worker in
    let transform hdl to_worker (sofar,memo) =
      HandlersMap.add hdl to_worker sofar, memo
    in
    let tasks,pipemap = fold repeat init transform hdlsmap in
    from_workers, to_pl, tasks, pipemap
                              
  let clone f (hdlsmap,memo) =
    let f = function
      | Some v -> f v
      | None   -> f memo
    in
    let from_workers1,to_pl1 = Pipe.create () in
    let from_workers2,to_pl2 = Pipe.create () in
    let repeat a = 
      let from_pl1,to_worker1 = Pipe.create () in
      let from_pl2,to_worker2 = Pipe.create () in
      let worker = f a from_pl1 to_pl1 from_pl2 to_pl2 in
      worker, (to_worker1,to_worker2)
    in
    let init (to_worker1,to_worker2) =
      (HandlersMap.empty,to_worker1),
      (HandlersMap.empty,to_worker2)
    in
    let transform hdl (to_worker1,to_worker2) ((sofar1,memo1),(sofar2,memo2)) =
      (HandlersMap.add hdl to_worker1 sofar1,memo1),
      (HandlersMap.add hdl to_worker2 sofar2,memo2)
    in
    let tasks, (pipemap1, pipemap2) = fold repeat init transform hdlsmap in
    from_workers1, to_pl1, from_workers2, to_pl2, tasks, pipemap1, pipemap2

  let broadcast f (map,to_memo) =
    let def = f to_memo in
    let aux _ to_worker () = f to_worker,() in
    let tasks,() = hdlfold aux map ([def],()) in
    Deferred.all_unit tasks
                      
end

