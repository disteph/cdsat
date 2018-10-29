open Async
open Lib

open General
       
open Kernel
open Top.Messages
open Top.Terms
open Theories.Theory

open Plugin

module Make(WB: WhiteBoardExt.S) = struct

  open WhiteBoardExt
  open WB

  let add     (SlotMachine{add})     = add
  let share   (SlotMachine{share})   = share
  let clone   (SlotMachine{clone})   = clone()
  let suicide (SlotMachine{suicide}) = suicide
  let propose (SlotMachine{propose}) = propose

  let rec flush ports msg =
    let aux = function
      | MsgStraight _ | MsgSharing _ | MsgPropose _ | Infos _ -> flush ports msg
      | MsgSpawn newports -> 
         Deferred.all_unit
           [ Lib.write ports.writer msg;
             Lib.write newports.writer msg;
             flush ports msg;
             flush newports msg ]
      | KillYourself _ -> return()
    in
    Lib.read ports.reader aux


  let rec loop_read hdl cont ports = 
    let aux msg =
      Print.print ["worker",1] (fun p-> p "%a reads %a" Tags.pp hdl pp_msg2th msg);
      match msg with
      | MsgStraight{ sassign; chrono }
        -> loop_write hdl (add cont (Some sassign)) chrono ports
      | MsgSharing{ tset; chrono }
        -> loop_write hdl (share cont tset) chrono ports
      | MsgPropose{ howmany; chrono }
        ->
        let decisions = propose cont howmany in
        Print.print ["worker",1] (fun p-> p "%a: Proposing" Tags.pp hdl);
        let msg2pl = Msg{ handler = Some(Handlers.Handler hdl);
                          answer  = Try decisions;
                          chrono }
        in
        Deferred.all_unit
         [ Lib.write ports.writer msg2pl ;
           loop_read hdl cont ports ]
      | Infos _
        -> loop_read hdl cont ports
      | MsgSpawn newports
        -> let newcont = clone cont in
           Deferred.all_unit
             [loop_read hdl cont    ports ;
              loop_read hdl newcont newports ]
      | KillYourself{ conflict = WB(_,Propa(assign,Unsat),_) } -> return(suicide cont assign)
    in
    Lib.read
      ~onkill:(fun ()->return(Print.print ["worker",2] (fun p-> p "%a dies" Tags.pp hdl)))
      ports.reader aux

  and loop_write hdl (say,cont) chrono ports =

    let handler = Some(Handlers.Handler hdl) in

    Print.print ["worker",1] (fun p-> p "%a looks at its output_msg" Tags.pp hdl);

    match say with
    | Silence -> 
       Print.print ["worker",1] (fun p-> p "%a: Silence" Tags.pp hdl);
       Deferred.all_unit
         [
           Lib.write ports.writer (Msg{ handler; answer = Ack; chrono }) ;
           loop_read hdl cont ports
         ]

    | Msg msg ->
       Print.print ["worker",1] (fun p-> p "%a: Message %a" Tags.pp hdl pp_message msg);
       let msg2pl = Msg{ handler; answer = Say(WB.sign hdl msg); chrono } in
       Deferred.all_unit
         [
           Lib.write ports.writer msg2pl ;
           match msg with
           | Propa(_,Unsat) ->
              Print.print ["worker",1] (fun p-> p "%a enters flush" Tags.pp hdl);
              flush ports msg2pl
           | _ ->
              loop_read hdl cont ports
         ]


  let make (PluginsTh.PluginTh.Signed(hdl,init)) = loop_read hdl init

end
