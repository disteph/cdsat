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

  let make from_pl to_pl (Signed(hdl,init)) tset =

    let read from_pl f =
      (* Dump.print ["worker",1] (fun p-> p "%a wants to read" Sig.print_in_fmt hdl); *)
      Pipe.read from_pl
      >>= function
      | `Eof -> return()
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
           [ Lib.write newwriter (Msg(WB.stamp hdl msg));
             flush newreader newwriter msg;
             flush reader writer msg
           ]
      )
    in

    let rec loop from_pl to_pl (Output(output_msg,cont)) =

      let keep_on() = read from_pl (function
        | MsgStraight tset
          -> loop from_pl to_pl (add cont (Some tset))
        | MsgBranch(tset1,tset2,newreader,newwriter)
          -> let Output(_,newcont) = clone cont in
             Deferred.all_unit
               [loop from_pl   to_pl     (add cont    (Some tset1));
                loop newreader newwriter (add newcont (Some tset2))]
      )
      in

      (* Dump.print ["worker",1] (fun p-> p "%a looks at its output_msg" Sig.print_in_fmt hdl); *)

      match output_msg with
      | None -> 
         Dump.print ["worker",1] (fun p-> p "%a: no output msg" Sig.print_in_fmt hdl);
        loop from_pl to_pl (add cont None)
      | Some msg ->
         (* print (Dump.toString (fun p-> p "Outputting message %a" print_in_fmt (Msg(hdl,msg)))) >>= fun () ->  *)
         Deferred.all_unit 
           [ Lib.write to_pl (Msg(WB.stamp hdl msg));
             match msg with
             | Propa(_,Unsat) -> flush from_pl to_pl msg
             | _              -> keep_on()
           ]
                                                                                          
    in loop from_pl to_pl (add init (Some tset))

end
