open Async.Std

open Kernel.Theories_register

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

  let make f hdlsmap =
    let from_workers,to_pl = Pipe.create () in
    let aux1 a =
      let from_pl,to_worker = Pipe.create () in
      let worker = f from_pl to_pl a in
      worker, to_worker
    in
    let aux2 hdl a mapsofar =
      let worker, to_worker = aux1(Some a) in
      worker, HandlersMap.add hdl to_worker mapsofar
    in
    let workers,hdlsmap = hdlfold aux2 hdlsmap ([],HandlersMap.empty) in
    let worker, to_worker = aux1 None in
    from_workers,to_pl,(worker::workers),(hdlsmap,to_worker)

  let clone f (hdlsmap,memo) =
    let f from_pl to_pl = function
      | Some v -> f from_pl to_pl v
      | None   -> f from_pl to_pl memo
    in
    make f hdlsmap

  let broadcast f (map,to_memo) =
    let def = f to_memo in
    let aux _ to_worker () = f to_worker,() in
    let tasks,() = hdlfold aux map ([def],()) in
    Deferred.all_unit tasks
                      
end

