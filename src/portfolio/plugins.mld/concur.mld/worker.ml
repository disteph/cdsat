open Async
open Lib

open Kernel
open Top.Messages
open Top.Specs
open Theories.Register

open Plugin
open Interfaces

module Make(WB: WhiteBoardExt) = struct

  open WB
  open DS

  let add     (SlotMachine m) = m.add
  let clone   (SlotMachine m) = m.clone()
  let suicide (SlotMachine m) = m.suicide

  let rec flush ports msg =
    let aux = function
      | MsgStraight _ | Infos _ -> flush ports msg
      | MsgBranch(ports1,ports2) -> 
         Deferred.all_unit
           [
             Lib.write ports1.writer msg;
             Lib.write ports2.writer msg;
             flush ports1 msg;
             flush ports2 msg
           ]
      | KillYourself _ -> return()
    in
    Lib.read ports.reader aux


  let rec loop_read hdl cont ports = 
    let aux msg =
      Dump.print ["worker",1] (fun p-> p "%a reads %a" Tags.pp hdl pp_msg2th msg);
      match msg with
      | MsgStraight(tset,chrono)
        -> loop_write hdl (add cont (Some tset)) chrono ports
      | Infos _
        -> loop_read hdl cont ports
      | MsgBranch(ports1,ports2)
        -> let newcont = clone cont in
           Deferred.all_unit
             [loop_read hdl cont    ports1 ;
              loop_read hdl newcont ports2 ]
      | KillYourself(WB(_,Propa(assign,Unsat)),_) -> return(suicide cont assign)
    in
    Lib.read
      ~onkill:(fun ()->return(Dump.print ["worker",2] (fun p-> p "%a dies" Tags.pp hdl)))
      ports.reader aux

  and loop_write hdl (say,cont) chrono ports =

    let hhdl = Some(Handlers.Handler hdl) in

    Dump.print ["worker",1] (fun p-> p "%a looks at its output_msg" Tags.pp hdl);

    match say with
    | Silence -> 
       Dump.print ["worker",1] (fun p-> p "%a: Silence" Tags.pp hdl);
       Deferred.all_unit
         [
           Lib.write ports.writer (Msg(hhdl,Ack,chrono)) ;
           loop_read hdl cont ports
         ]

    | Msg msg ->
       Dump.print ["worker",1] (fun p-> p "%a: Message %a" Tags.pp hdl Msg.pp msg);
       let msg2pl = Msg(hhdl,Say(WB.stamp hdl msg),chrono) in
       Deferred.all_unit
         [
           Lib.write ports.writer msg2pl ;
           match msg with
           | Propa(_,Unsat) ->
              Dump.print ["worker",1] (fun p-> p "%a enters flush" Tags.pp hdl);
              flush ports msg2pl
           | _ ->
              loop_read hdl cont ports
         ]

    | Try sassign ->
       Dump.print ["worker",1] (fun p-> p "%a: Try %a" Tags.pp hdl pp_sassign sassign);
       let msg2pl = Msg(hhdl,Try sassign,chrono) in
       Deferred.all_unit
         [
           Lib.write ports.writer msg2pl ;
           loop_read hdl cont ports
         ]

  let make (PluginsTh.PluginTh.Signed(hdl,init)) = loop_read hdl init

end
