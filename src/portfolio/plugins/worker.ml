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

  let make (Signed(hdl,init)) tset =

    let hhdl = Some(Handlers.Handler hdl) in

    let read from_pl f =
      Dump.print ["worker",1] (fun p-> p "%a wants to read" Sig.print_in_fmt hdl);
      Pipe.read from_pl
      >>= function
      | `Eof ->
         Dump.print ["worker",1] (fun p-> p "%a dies" Sig.print_in_fmt hdl);
         return()
      | `Ok msg ->
         Dump.print ["worker",1] (fun p-> p "%a reads %a" Sig.print_in_fmt hdl print2th_in_fmt msg);
         f msg
    in

    let rec flush reader writer msg =
      (* Dump.print ["worker",1] (fun p-> p "%a enters flush" Sig.print_in_fmt hdl); *)
      read reader (function
      | MsgStraight _ -> flush reader writer msg
      | MsgBranch(_,_,newreader,newwriter) -> 
         Deferred.all_unit
           [ Lib.write newwriter (Msg(hhdl,Say(WB.stamp hdl msg)));
             flush newreader newwriter msg;
             flush reader writer msg
           ]
      )
    in

    let rec loop (Output(output_msg,cont)) from_pl to_pl =

      let keep_on() = read from_pl (function
        | MsgStraight tset
          -> loop (add cont (Some tset)) from_pl to_pl
        | MsgBranch(tset1,tset2,newreader,newwriter)
          -> let Output(_,newcont) = clone cont in
             Deferred.all_unit
               [loop (add cont    (Some tset1)) from_pl   to_pl     ;
                loop (add newcont (Some tset2)) newreader newwriter ]
      )
      in

      (* Dump.print ["worker",1] (fun p-> p "%a looks at its output_msg" Sig.print_in_fmt hdl); *)

      match output_msg with
      | None -> 
         Dump.print ["worker",1] (fun p-> p "%a: no output msg" Sig.print_in_fmt hdl);
         Deferred.all_unit
           [
             Lib.write to_pl (Msg(hhdl,Ack)) ;
             loop (add cont None) from_pl to_pl
           ]
      | Some msg ->
         Dump.print ["worker",1] (fun p-> p "%a: Outputting message %a" Sig.print_in_fmt hdl Msg.print_in_fmt msg);
         Deferred.all_unit
           [
             Lib.write to_pl (Msg(hhdl,Say(WB.stamp hdl msg))) ;
             match msg with
             | Propa(_,Unsat) -> flush from_pl to_pl msg
             | _              -> keep_on()
           ]
                                                                                          
    in loop (add init (Some tset))

end
