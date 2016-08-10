open Async.Std
open Lib

open Kernel
open Top.Messages
open Top.Specs
open Theories_register
open Combo

open LoadPluginsTh

module Make(WB: WhiteBoardExt.Type) = struct

  open WB
  open DS

  let add   (type sign) (cont: (sign,TSet.t) slot_machine) = let module Cont = (val cont) in Cont.add
  let clone (type sign) (cont: (sign,TSet.t) slot_machine) = let module Cont = (val cont) in Cont.clone()
  let suicide (type sign) (cont: (sign,TSet.t) slot_machine) = let module Cont = (val cont) in Cont.suicide

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
      Dump.print ["worker",1] (fun p-> p "%a reads %a" Sig.print_in_fmt hdl print2th_in_fmt msg);
      match msg with
      | MsgStraight(tset,chrono)
        -> loop_write hdl (add cont (Some tset)) chrono from_pl to_pl
      | MsgBranch(newreader1,newwriter1,newreader2,newwriter2)
        -> let Output(_,newcont) = clone cont in
           Deferred.all_unit
             [loop_read hdl cont    newreader1 newwriter1 ;
              loop_read hdl newcont newreader2 newwriter2 ]
      | KillYourself(WB(_,msg),_,_) -> return(suicide cont msg)
    in
    Lib.read ~onkill:(fun ()->return(Dump.print ["memo",2] (fun p-> p "%a dies" Sig.print_in_fmt hdl)))
      from_pl aux

  and loop_write hdl (Output(output_msg,cont)) chrono from_pl to_pl =

    let hhdl = Some(Handlers.Handler hdl) in

    Dump.print ["worker",1] (fun p-> p "%a looks at its output_msg" Sig.print_in_fmt hdl);

    match output_msg with
    | None -> 
       Dump.print ["worker",1] (fun p-> p "%a: no output msg" Sig.print_in_fmt hdl);
       Deferred.all_unit
         [
           Lib.write to_pl (Msg(hhdl,Ack,chrono)) ;
           loop_read hdl cont from_pl to_pl
         ]
    | Some msg ->
       Dump.print ["worker",1] (fun p-> p "%a: Outputting message %a" Sig.print_in_fmt hdl Msg.print_in_fmt msg);
       let msg2pl = Msg(hhdl,Say(WB.stamp hdl msg),chrono) in
       Deferred.all_unit
         [
           Lib.write to_pl msg2pl ;
           match msg with
           | Propa(_,Unsat) ->
              Dump.print ["worker",1] (fun p-> p "%a enters flush" Sig.print_in_fmt hdl);
              flush from_pl to_pl msg2pl
           | _ ->
              loop_read hdl cont from_pl to_pl
         ]

  let make (Signed(hdl,init)) = loop_read hdl init

end
