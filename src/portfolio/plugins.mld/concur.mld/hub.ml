open Async

open General.Sums
open Kernel.Theories.Register
open Interfaces
  
module Make(WB: WhiteBoardExt) = struct
  open WB

  type t = {
      reader : msg2pl Pipe.Reader.t;
      master : msg2pl Pipe.Writer.t;
      egraph : egraph msg2th Pipe.Writer.t;
      memo   : regular msg2th Pipe.Writer.t;
      others : regular msg2th Pipe.Writer.t HandlersMap.t
    }

  let reader hub = hub.reader
             
  let map_write f writer_b =
    let reader_a, writer_a = Pipe.create () in
    Pipe.transfer reader_a writer_b ~f, writer_a

  let th2egraph address tv = TheoryAsk(address,tv)
                                        
  let make egraph memo hdlsmap =
    let read2pl, write2pl         = Pipe.create () in
    let read2egraph, write2egraph = Pipe.create () in
    let egraph_ports = {
        reader = read2egraph;
        writer = write2pl;
        eports = EPorts
      }
    in
    let egraph_task = egraph egraph_ports in
    let read2memo, write2memo     = Pipe.create () in
    let memo2egraph_task, memo2egraph =
      map_write (th2egraph write2memo) write2egraph
    in 
    let memo_ports = {
        reader = read2memo;
        writer = write2pl;
        eports = RegularPorts memo2egraph
      }
    in
    let memo_task = memo memo_ports in
    let aux hdl worker (tasks,map) =
      let read2worker,write2worker = Pipe.create () in
      let th2egraph_task, th2egraph =
        map_write (th2egraph write2worker) write2egraph
      in
      let ports = {
          reader = read2worker;
          writer = write2pl;
          eports = RegularPorts th2egraph
        }
      in
      (worker ports)::th2egraph_task::tasks,
      HandlersMap.add hdl write2worker map
    in
    let tasks = [egraph_task; memo_task; memo2egraph_task] in
    let tasks, write2workers =
      HandlersMap.fold aux hdlsmap (tasks,HandlersMap.empty)
    in
    Deferred.all_unit tasks,
    { reader = read2pl;
      master = write2pl;
      egraph = write2egraph;
      memo   = write2memo;
      others = write2workers }

  let kill hub =
    HandlersMap.iter (fun _ w -> Pipe.close w) hub.others;
    Pipe.close hub.master;
    Pipe.close hub.egraph;
    Pipe.close hub.memo
      
  let broadcast hub sassign ~chrono =
    let msg = MsgStraight(sassign,chrono) in
    let send2egraph = Lib.write hub.egraph msg in
    let send2memo = Lib.write hub.memo msg in
    let aux _ worker sofar = (Lib.write worker msg)::sofar in
    Deferred.all_unit
      (HandlersMap.fold aux hub.others [send2egraph; send2memo])

  let suicide hub msg a1 a2 =
    let msg = KillYourself(msg,a1,a2) in
    let send2egraph = Lib.write hub.egraph msg in
    let send2memo = Lib.write hub.memo msg in
    let aux _ worker sofar = (Lib.write worker msg)::sofar in
    Deferred.all_unit
      (HandlersMap.fold aux hub.others [send2egraph; send2memo])

  let clone hub = failwith "TODO"

end
