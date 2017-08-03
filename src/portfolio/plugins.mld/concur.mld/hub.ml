open Async

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

  let common egraph memo hdlsmap =
    let read2pl, write2pl         = Pipe.create () in
    let read2egraph, write2egraph = Pipe.create () in
    let egraph_ports = {
        reader = read2egraph;
        writer = write2pl;
        eports = EPorts
      }
    in
    let egraph_task = egraph egraph_ports in
    let read2memo, write2memo = Pipe.create () in
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
    let aux hdl worker (utasks,tasks,map) =
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
      th2egraph_task::utasks,
      HandlersMap.add hdl (worker ports) tasks,
      HandlersMap.add hdl write2worker map
    in
    let utasks, tasks, write2workers =
      HandlersMap.fold
        aux
        hdlsmap
        ([memo2egraph_task],HandlersMap.empty,HandlersMap.empty)
    in
    egraph_task,
    memo_task,
    tasks,
    { reader = read2pl;
      master = write2pl;
      egraph = write2egraph;
      memo   = write2memo;
      others = write2workers }

  let make egraph memo hdlsmap =
    let egraph_task,memo_task,tasks,hub
      = common egraph memo hdlsmap
    in
    let tasks = HandlersMap.fold (fun _ task sofar -> task::sofar) tasks [] in
    Deferred.all_unit
      (egraph_task::memo_task::tasks),
    hub

  let clone hub =
    let aux writer ports1 ports2 = Lib.write writer (MsgBranch(ports1,ports2)) in
    let hdlsmap = HandlersMap.map aux hub.others in
    let egraph_ptask,memo_ptask,ptasks,hub1
      = common (aux hub.egraph) (aux hub.memo) hdlsmap
    in
    let tasks,hub2 = make egraph_ptask memo_ptask ptasks in
    tasks >>| fun () -> hub1,hub2

  let kill hub =
    HandlersMap.iter (fun _ w -> Pipe.close w) hub.others;
    Pipe.close hub.master;
    Pipe.close hub.egraph;
    Pipe.close hub.memo

  let send hub to_egraph to_others =
    let send2egraph = Lib.write hub.egraph to_egraph in
    let send2memo   = Lib.write hub.memo to_others in
    let aux _ worker sofar = (Lib.write worker to_others)::sofar in
    Deferred.all_unit
      (HandlersMap.fold aux hub.others [send2egraph; send2memo])
               
  let broadcast hub sassign ~chrono =
    let msg = MsgStraight(sassign,chrono) in
    send hub msg msg

  let share hub tset ~chrono =
    let msg = MsgSharing(tset,chrono) in
    send hub msg msg

  let suicide hub msg a =
    let msg = KillYourself(msg,a) in
    send hub msg msg

end
