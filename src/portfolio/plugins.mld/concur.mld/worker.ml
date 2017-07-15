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

  let rec flush reader writer msg =
    let aux = function
      | MsgStraight _ -> flush reader writer msg
      | MsgBranch(newreader1,newwriter1,newreader2,newwriter2) -> 
         Deferred.all_unit
           [
             Lib.write newwriter1 msg;
             Lib.write newwriter2 msg;
             flush newreader1 newwriter1 msg;
             flush newreader2 newwriter2 msg
           ]
      | KillYourself _ -> return()
    in
    Lib.read reader aux


  let rec loop_read hdl cont from_pl to_pl = 
    let aux msg =
      Dump.print ["worker",1] (fun p-> p "%a reads %a" Tags.pp hdl print2th_in_fmt msg);
      match msg with
      | MsgStraight(tset,chrono)
        -> loop_write hdl (add cont (Some tset)) chrono from_pl to_pl
      | MsgBranch(newreader1,newwriter1,newreader2,newwriter2)
        -> let newcont = clone cont in
           Deferred.all_unit
             [loop_read hdl cont    newreader1 newwriter1 ;
              loop_read hdl newcont newreader2 newwriter2 ]
      | KillYourself(WB(_,Propa(assign,Unsat)),_,_) -> return(suicide cont assign)
    in
    Lib.read
      ~onkill:(fun ()->return(Dump.print ["worker",2] (fun p-> p "%a dies" Tags.pp hdl)))
      from_pl aux

  and loop_write hdl (say,cont) chrono from_pl to_pl =

    let hhdl = Some(Handlers.Handler hdl) in

    Dump.print ["worker",1] (fun p-> p "%a looks at its output_msg" Tags.pp hdl);

    match say with
    | Silence -> 
       Dump.print ["worker",1] (fun p-> p "%a: Silence" Tags.pp hdl);
       Deferred.all_unit
         [
           Lib.write to_pl (Msg(hhdl,Ack,chrono)) ;
           loop_read hdl cont from_pl to_pl
         ]

    | Msg msg ->
       Dump.print ["worker",1] (fun p-> p "%a: Message %a" Tags.pp hdl Msg.pp msg);
       let msg2pl = Msg(hhdl,Say(WB.stamp hdl msg),chrono) in
       Deferred.all_unit
         [
           Lib.write to_pl msg2pl ;
           match msg with
           | Propa(_,Unsat) ->
              Dump.print ["worker",1] (fun p-> p "%a enters flush" Tags.pp hdl);
              flush from_pl to_pl msg2pl
           | _ ->
              loop_read hdl cont from_pl to_pl
         ]

    | Try sassign ->
       Dump.print ["worker",1] (fun p-> p "%a: Try %a" Tags.pp hdl pp_sassign sassign);
       let msg2pl = Msg(hhdl,Try sassign,chrono) in
       Deferred.all_unit
         [
           Lib.write to_pl msg2pl ;
           loop_read hdl cont from_pl to_pl
         ]

  let make (PluginsTh.PluginTh.Signed(hdl,init)) = loop_read hdl init

end
